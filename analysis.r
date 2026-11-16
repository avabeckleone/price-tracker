prices <- read.csv(
  "prices_clean.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

currency_cols <- c("price", "regular_price", "non_deal_price")
prices[currency_cols] <- lapply(
  prices[currency_cols],
  function(x) as.numeric(gsub("[$,]", "", x))
)
library(fixest)

setFixest_notes(FALSE)

output_dir <- "analysis_output"
dir.create(output_dir, showWarnings = FALSE)

# The CSV has no category column; pair_id identifies each matched product pair.
# Use only rows with a valid positive baseline price and unit count.
baseline_rows <- subset(
  prices,
  !is.na(pair_id) &
    !is.na(unit_count) & unit_count > 0 &
    !is.na(baseline_price) & baseline_price > 0
)
baseline_rows$ln_unit_baseline <- log(
  baseline_rows$baseline_price / baseline_rows$unit_count
)

# 1. Collapse repeated observations to pair, item, and unit-count level.
df_item_base <- aggregate(
  ln_unit_baseline ~ pair_id + is_basics + unit_count,
  data = baseline_rows,
  FUN = median
)

# 2. Paired fixed-effects regression on unit-normalized log baseline price.
reg_baseline <- feols(ln_unit_baseline ~ is_basics | pair_id,
                      data = df_item_base,
                      cluster = ~pair_id)

# The CSV has timestamp but no time_bucket; use calendar day as the time bucket.
panel_rows <- baseline_rows
panel_rows$time_bucket <- factor(substr(panel_rows$timestamp, 1, 10))

# Panel sensitivity check with pair and day fixed effects.
reg_baseline_panel <- feols(ln_unit_baseline ~ is_basics | pair_id + time_bucket,
                            data = panel_rows,
                            cluster = ~pair_id)

regression_table <- etable(
  reg_baseline,
  reg_baseline_panel,
  headers = c("Item Median Baseline", "Panel w/ Pair and Day FE")
)
writeLines(
  capture.output(print(regression_table)),
  file.path(output_dir, "baseline_regression.txt")
)

# Convert log coefficient to exact percentage gap (Amazon Basics vs. 3P Twin)
pct_gap_est <- 100 * (exp(coef(reg_baseline)["is_basics"]) - 1)
cat("Average Baseline Price Gap (%):", round(pct_gap_est, 2), "%\n")

# 3. Paired dot plot (one dot per pair, converted to percent).
df_pair_gaps <- aggregate(
  ln_unit_baseline ~ pair_id + is_basics,
  data = df_item_base,
  FUN = median
)
df_pair_gaps <- reshape(
  df_pair_gaps,
  idvar = "pair_id",
  timevar = "is_basics",
  direction = "wide"
)
df_pair_gaps$pct_gap <- 100 * (
  exp(df_pair_gaps$ln_unit_baseline.1 - df_pair_gaps$ln_unit_baseline.0) - 1
)
df_pair_gaps <- df_pair_gaps[is.finite(df_pair_gaps$pct_gap), ]
df_pair_gaps <- df_pair_gaps[order(df_pair_gaps$pct_gap), ]

write.csv(
  df_pair_gaps,
  file.path(output_dir, "baseline_price_gaps.csv"),
  row.names = FALSE
)

png(
  file.path(output_dir, "baseline_price_gaps.png"),
  width = 1600,
  height = 1000,
  res = 150
)
pair_colors <- setNames(
  grDevices::hcl.colors(length(unique(df_pair_gaps$pair_id)), palette = "Dark 3"),
  sort(unique(df_pair_gaps$pair_id))
)
pair_index <- 100 + df_pair_gaps$pct_gap
plot_range <- range(c(0, 100, pair_index))
plot_padding <- max(diff(plot_range) * 0.05, 5)
par(mar = c(5, 12, 4, 2) + 0.1)
plot(
  pair_index,
  seq_len(nrow(df_pair_gaps)),
  xlim = c(plot_range[1] - plot_padding, plot_range[2] + plot_padding),
  ylim = c(0.5, nrow(df_pair_gaps) + 0.5),
  pch = 17,
  cex = 1.2,
  col = pair_colors[df_pair_gaps$pair_id],
  yaxt = "n",
  xlab = "Baseline unit price index (matched twin = 100)",
  ylab = "",
  main = "Baseline Unit Price by Matched Pair",
  sub = "Circle = matched twin; triangle = Amazon Basics. Same-color points belong to the same pair."
)
grid(nx = NA, ny = NULL, col = "grey90")
abline(v = 100, lty = 2, lwd = 1.5, col = "grey40")
for (i in seq_len(nrow(df_pair_gaps))) {
  pair_color <- pair_colors[df_pair_gaps$pair_id[i]]
  segments(
    x0 = 100,
    y0 = i,
    x1 = pair_index[i],
    y1 = i,
    col = pair_color,
    lwd = 2
  )
  points(100, i, pch = 16, cex = 1.2, col = pair_color)
  points(pair_index[i], i, pch = 17, cex = 1.2, col = pair_color)
  text(
    pair_index[i],
    i,
    labels = sprintf("%+.1f%%", df_pair_gaps$pct_gap[i]),
    pos = if (pair_index[i] >= 100) 4 else 2,
    offset = 0.7,
    cex = 0.8
  )
}
axis(2, at = seq_len(nrow(df_pair_gaps)), labels = df_pair_gaps$pair_id, las = 1)
dev.off()

# Promotional analysis uses columns that exist in prices_clean.csv.
# A time bucket is a calendar day; sale episodes are starts of observed sale runs per ASIN.
promo_rows <- subset(prices, !is.na(pair_id) & !is.na(is_on_sale))
promo_rows$time_bucket <- factor(substr(promo_rows$timestamp, 1, 10))
promo_rows <- promo_rows[order(promo_rows$asin, promo_rows$timestamp), ]
promo_rows$episode_start <- 0L
for (asin_rows in split(seq_len(nrow(promo_rows)), promo_rows$asin)) {
  sale_status <- promo_rows$is_on_sale[asin_rows] == 1
  previous_sale <- c(FALSE, head(sale_status, -1))
  promo_rows$episode_start[asin_rows] <- as.integer(sale_status & !previous_sale)
}
promo_rows$discount_depth_all <- ifelse(
  promo_rows$is_on_sale == 1,
  promo_rows$discount_depth,
  0
)

# Summarize each matched pair and brand side, weighting calendar days equally.
df_item_sales <- do.call(
  rbind,
  lapply(
    split(
      promo_rows,
      interaction(promo_rows$pair_id, promo_rows$is_basics, drop = TRUE)
    ),
    function(item_rows) {
      daily_rows <- split(item_rows, as.character(item_rows$time_bucket))
      daily_sale_share <- vapply(
        daily_rows,
        function(day) mean(day$is_on_sale == 1),
        numeric(1)
      )
      daily_sale_depth <- vapply(
        daily_rows,
        function(day) {
          values <- day$discount_depth_all
          if (any(!is.na(values))) mean(values, na.rm = TRUE) else NA_real_
        },
        numeric(1)
      )
      sale_depths <- item_rows$discount_depth[
        item_rows$is_on_sale == 1 & !is.na(item_rows$discount_depth)
      ]

      data.frame(
        pair_id = item_rows$pair_id[1],
        is_basics = item_rows$is_basics[1],
        observed_buckets = length(daily_rows),
        sale_buckets = sum(vapply(
          daily_rows,
          function(day) any(day$is_on_sale == 1),
          logical(1)
        )),
        sale_share = mean(daily_sale_share),
        episodes = sum(item_rows$episode_start),
        ep_per_30 = 30 * sum(item_rows$episode_start) / length(daily_rows),
        depth_if_sale = if (length(sale_depths)) mean(sale_depths) else NA_real_,
        depth_all = if (any(!is.na(daily_sale_depth))) {
          mean(daily_sale_depth, na.rm = TRUE)
        } else {
          NA_real_
        },
        max_depth = if (length(sale_depths)) max(sale_depths) else NA_real_
      )
    }
  )
)
rownames(df_item_sales) <- NULL

# Summary table comparing Amazon Basics (1) vs. 3P Twin (0).
sales_metrics <- c(
  "sale_share", "ep_per_30", "depth_if_sale", "depth_all", "max_depth"
)
df_sales_summary <- do.call(
  rbind,
  lapply(split(df_item_sales, df_item_sales$is_basics), function(group) {
    data.frame(
      is_basics = group$is_basics[1],
      matched_pairs = length(unique(group$pair_id)),
      sale_share = mean(group$sale_share, na.rm = TRUE),
      ep_per_30 = mean(group$ep_per_30, na.rm = TRUE),
      depth_if_sale = mean(group$depth_if_sale, na.rm = TRUE),
      depth_all = mean(group$depth_all, na.rm = TRUE),
      max_depth = mean(group$max_depth, na.rm = TRUE)
    )
  })
)
print(df_sales_summary)
write.csv(
  df_sales_summary,
  file.path(output_dir, "promotional_summary.csv"),
  row.names = FALSE
)

# Pair and calendar-day fixed effects; standard errors are clustered by pair.
m_sale_lpm <- feols(
  is_on_sale ~ is_basics | pair_id + time_bucket,
  data = promo_rows,
  cluster = ~pair_id
)
m_sale_logit <- feglm(
  is_on_sale ~ is_basics | pair_id + time_bucket,
  family = binomial(link = "logit"),
  data = promo_rows,
  cluster = ~pair_id
)
m_depth_all <- feols(
  discount_depth_all ~ is_basics | pair_id + time_bucket,
  data = promo_rows,
  cluster = ~pair_id
)
m_depth_cond <- feols(
  discount_depth ~ is_basics | pair_id + time_bucket,
  data = subset(promo_rows, is_on_sale == 1 & !is.na(discount_depth)),
  cluster = ~pair_id
)
m_episodes <- fepois(
  episodes ~ is_basics | pair_id,
  offset = ~log(observed_buckets),
  data = df_item_sales,
  cluster = ~pair_id
)

promotional_table <- etable(
  m_sale_lpm,
  m_sale_logit,
  m_depth_all,
  m_depth_cond,
  m_episodes,
  headers = c(
    "P(Sale) LPM", "P(Sale) Logit", "Depth (All)",
    "Depth (On Sale)", "Episodes (Poisson)"
  )
)
writeLines(
  capture.output(print(promotional_table)),
  file.path(output_dir, "promotional_regression.txt")
)

irr_episodes <- exp(coef(m_episodes)["is_basics"])
cat("Sale Episodes Incidence Rate Ratio (AB / Twin):", round(irr_episodes, 2), "\n")

# Slope charts use pair_id for identity and color; category is not in this CSV.
plot_pair_slope <- function(data, metric_col, y_label, plot_title, file_name) {
  pair_ids <- sort(unique(data$pair_id))
  colors <- setNames(
    grDevices::hcl.colors(length(pair_ids), palette = "Dark 3"),
    pair_ids
  )
  png(file.path(output_dir, file_name), width = 1600, height = 900, res = 150)
  layout(matrix(c(1, 2), nrow = 1), widths = c(3.5, 1.5))
  par(mar = c(5, 5, 4, 1) + 0.1)
  values <- data[[metric_col]]
  finite_values <- values[is.finite(values)]
  plot_range <- if (length(finite_values)) {
    range(finite_values)
  } else {
    c(0, 1)
  }
  if (diff(plot_range) == 0) plot_range <- plot_range + c(-0.5, 0.5)
  plot(
    c(1, 2),
    plot_range,
    type = "n",
    xaxt = "n",
    xlim = c(0.8, 2.2),
    ylim = plot_range,
    xlab = "",
    ylab = y_label,
    main = plot_title
  )
  axis(1, at = c(1, 2), labels = c("3P Twin", "Amazon Basics"))
  grid(nx = NA, ny = NULL, col = "grey90")
  for (pair in pair_ids) {
    pair_rows <- data[data$pair_id == pair, ]
    pair_rows <- pair_rows[order(pair_rows$is_basics), ]
    pair_values <- pair_rows[[metric_col]]
    if (nrow(pair_rows) == 2 && all(is.finite(pair_values))) {
      lines(c(1, 2), pair_values, col = colors[pair], lwd = 1.5)
      points(c(1, 2), pair_values, col = colors[pair], pch = 19)
    }
  }
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend(
    "center",
    legend = pair_ids,
    col = colors[pair_ids],
    lty = 1,
    pch = 19,
    cex = 0.8,
    bty = "n",
    title = "Matched pair"
  )
  dev.off()
}

plot_pair_slope(
  df_item_sales, "sale_share", "Share of Observed Days on Sale",
  "Promotional Frequency by Pair", "promotional_frequency.png"
)
plot_pair_slope(
  df_item_sales, "depth_if_sale", "Average Discount Depth When on Sale",
  "Promotional Depth by Pair", "promotional_depth.png"
)
plot_pair_slope(
  df_item_sales, "ep_per_30", "Sale Episodes per 30 Observed Days",
  "Promotional Episode Cadence by Pair", "promotional_episodes.png"
)