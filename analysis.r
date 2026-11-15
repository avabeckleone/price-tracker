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
