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

# Restore package-level baselines that were recorded as per-unit prices.
original_unit_count <- prices$unit_count
per_unit_baseline <- !is.na(prices$baseline_price) &
  !is.na(original_unit_count) & original_unit_count > 1 &
  (
    prices$baseline_price < 1 |
      (!is.na(prices$unit_price) &
         abs(prices$baseline_price - prices$unit_price) < 1e-8)
  ) &
  !(prices$pair_id %in% "baby_wipes")
prices$baseline_price[per_unit_baseline] <-
  prices$baseline_price[per_unit_baseline] * original_unit_count[per_unit_baseline]

# The wipes' baseline values were also recorded as per-wipe prices.
prices$baseline_price[
  prices$pair_id %in% "baby_wipes" & prices$asin %in% "B07H4V7M4C"
] <- 24.99
bad_huggies_baseline <- prices$pair_id %in% "baby_wipes" &
  prices$asin %in% "B0BL8RWHV2" &
  !is.na(prices$baseline_price) & prices$baseline_price < 1
prices$baseline_price[bad_huggies_baseline] <- prices$price[bad_huggies_baseline]

# Fill missing baselines from observed price; use regular price only if price is absent.
missing_baseline <- is.na(prices$baseline_price) | prices$baseline_price <= 0
fallback_price <- prices$price
missing_price <- is.na(fallback_price) | fallback_price <= 0
fallback_price[missing_price] <- prices$regular_price[missing_price]
prices$baseline_price[missing_baseline] <- fallback_price[missing_baseline]

# Compare package prices for all pairs except diapers and wipes, which use item counts.
paired_rows <- !is.na(prices$pair_id)
prices$unit_count[paired_rows & !(prices$pair_id %in% c("diaper_pail_refills", "baby_wipes"))] <- 1
prices$unit_count[prices$pair_id %in% "diaper_pail_refills"] <- 8
prices$unit_count[prices$pair_id %in% "baby_wipes" & prices$asin %in% "B07H4V7M4C"] <- 810
prices$unit_count[prices$pair_id %in% "baby_wipes" & prices$asin %in% "B0BL8RWHV2"] <- 960

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

# The strategic-interaction section expects a daily price panel with the same
# variable names used elsewhere in this script.
df <- subset(
  prices,
  !is.na(pair_id) &
    !is.na(is_basics) &
    !is.na(price) &
    price > 0
)
df$time_bucket <- factor(substr(df$timestamp, 1, 10))

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

pair_categories <- c(
  aa_batteries = "Electronics, computers & office",
  keyboard_mouse_combo = "Electronics, computers & office",
  led_strip = "Electronics, computers & office",
  markers = "Electronics, computers & office",
  staples = "Electronics, computers & office",
  tablet_stand = "Electronics, computers & office",
  paper_plates = "Grocery & Whole foods market",
  baby_wipes = "Toys, kids, baby, & STEM",
  diaper_pail_refills = "Toys, kids, baby, & STEM",
  play_climber = "Toys, kids, baby, & STEM",
  fabric_shaver = "Clothing, shoes, & jewellery",
  golf_balls = "Sports & outdoors",
  water_bottle = "Sports & outdoors",
  car_cover = "Automotive & Industrial",
  razor_blades = "Automotive & Industrial",
  safety_hammer = "Automotive & Industrial",
  cleaning_cloths = "Home, garden, pets, & tools",
  flashlight = "Home, garden, pets, & tools",
  picture_frames = "Home, garden, pets, & tools",
  spatulas = "Home, garden, pets, & tools",
  trash_bags = "Home, garden, pets, & tools"
)
pair_labels <- c(
  aa_batteries = "AA batteries",
  baby_wipes = "Baby wipes",
  car_cover = "Car cover",
  cleaning_cloths = "Microfiber cleaning cloths",
  diaper_pail_refills = "Diaper pail refills",
  fabric_shaver = "Fabric shaver",
  flashlight = "Tactical flashlight",
  golf_balls = "Golf balls",
  keyboard_mouse_combo = "Keyboard + mouse combo",
  led_strip = "Smart LED strip",
  markers = "Permanent markers",
  paper_plates = "Disposable paper plates",
  picture_frames = "Picture frames",
  play_climber = "Indoor play climber",
  razor_blades = "Industrial scraper blades",
  safety_hammer = "Car escape hammer",
  spatulas = "Silicone spatula sets",
  staples = "Staples",
  tablet_stand = "Tablet stand",
  trash_bags = "Kitchen trash bags",
  water_bottle = "Insulated sport bottle"
)
category_order <- c(
  "Electronics, computers & office",
  "Grocery & Whole foods market",
  "Toys, kids, baby, & STEM",
  "Clothing, shoes, & jewellery",
  "Sports & outdoors",
  "Automotive & Industrial",
  "Home, garden, pets, & tools"
)
unmapped_pairs <- setdiff(as.character(df_pair_gaps$pair_id), names(pair_categories))
if (length(unmapped_pairs)) {
  stop("Baseline price chart has uncategorized pairs: ",
       paste(unmapped_pairs, collapse = ", "))
}
df_pair_gaps$category <- unname(pair_categories[df_pair_gaps$pair_id])
df_pair_gaps$item_label <- unname(pair_labels[df_pair_gaps$pair_id])
if (anyNA(df_pair_gaps$item_label)) {
  stop("Baseline price chart is missing a display label for one or more pairs.")
}
df_pair_gaps <- df_pair_gaps[
  order(match(df_pair_gaps$category, category_order), df_pair_gaps$pct_gap),
]

write.csv(
  df_pair_gaps,
  file.path(output_dir, "baseline_price_gaps.csv"),
  row.names = FALSE
)

present_categories <- category_order[category_order %in% df_pair_gaps$category]
category_colors <- setNames(
  grDevices::hcl.colors(length(present_categories), palette = "Dark 3"),
  present_categories
)
category_summary <- aggregate(
  pct_gap ~ category,
  data = df_pair_gaps,
  FUN = median
)
category_summary <- category_summary[
  match(present_categories, category_summary$category),
]
category_positions <- seq_len(nrow(category_summary))
plot_range <- range(c(0, category_summary$pct_gap))
plot_padding <- max(diff(plot_range) * 0.05, 5)
plot_xlim <- c(plot_range[1] - plot_padding, plot_range[2] + plot_padding)
cheaper_category_count <- sum(category_summary$pct_gap < 0)

png(
  file.path(output_dir, "baseline_price_gaps.png"),
  width = 1800,
  height = 1000,
  res = 150
)
par(mar = c(5, 20, 5, 4) + 0.1)
plot(
  category_summary$pct_gap,
  category_positions,
  type = "n",
  xlim = plot_xlim,
  ylim = c(nrow(category_summary) + 0.5, 0.5),
  yaxt = "n",
  xlab = "Median baseline unit-price gap vs. matched product (%)",
  ylab = "",
  main = sprintf(
    "Amazon Basics has lower median prices in %d of %d categories",
    cheaper_category_count,
    nrow(category_summary)
  ),
  sub = "Each bar shows the category median; negative gaps mean Amazon Basics is cheaper."
)
grid(nx = NULL, ny = NA, col = "grey90")
abline(v = 0, lty = 2, lwd = 1.5, col = "grey40")
axis(
  2,
  at = category_positions,
  labels = category_summary$category,
  las = 1,
  tick = FALSE,
  cex.axis = 0.9
)
for (i in seq_len(nrow(category_summary))) {
  category <- category_summary$category[i]
  category_gap <- category_summary$pct_gap[i]
  category_color <- category_colors[category]
  segments(
    x0 = 0,
    y0 = category_positions[i],
    x1 = category_gap,
    y1 = category_positions[i],
    col = category_color,
    lwd = 8,
    lend = "butt"
  )
  points(
    category_gap,
    category_positions[i],
    pch = 21,
    cex = 1.4,
    col = category_color,
    bg = category_color
  )
  text(
    category_gap,
    category_positions[i],
    labels = sprintf("%+.1f%%", category_gap),
    pos = if (category_gap >= 0) 4 else 2,
    offset = 0.6,
    cex = 0.9
  )
}
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

unmapped_promo_pairs <- setdiff(unique(df_item_sales$pair_id), names(pair_categories))
if (length(unmapped_promo_pairs)) {
  stop("Promotional summary has uncategorized pairs: ",
       paste(unmapped_promo_pairs, collapse = ", "))
}
df_item_sales$category <- unname(pair_categories[df_item_sales$pair_id])
present_promo_categories <- category_order[
  category_order %in% df_item_sales$category
]
mean_or_na <- function(values) {
  values <- values[is.finite(values)]
  if (length(values)) mean(values) else NA_real_
}
promo_category_summary <- do.call(
  rbind,
  lapply(present_promo_categories, function(category) {
    category_rows <- df_item_sales[df_item_sales$category == category, ]
    twin_rows <- category_rows[category_rows$is_basics == 0, ]
    basics_rows <- category_rows[category_rows$is_basics == 1, ]
    data.frame(
      category = category,
      matched_pairs = length(unique(category_rows$pair_id)),
      twin_pairs_with_sales = sum(is.finite(twin_rows$depth_if_sale)),
      basics_pairs_with_sales = sum(is.finite(basics_rows$depth_if_sale)),
      twin_sale_share = mean(twin_rows$sale_share),
      basics_sale_share = mean(basics_rows$sale_share),
      twin_depth_if_sale = mean_or_na(twin_rows$depth_if_sale),
      basics_depth_if_sale = mean_or_na(basics_rows$depth_if_sale),
      stringsAsFactors = FALSE
    )
  })
)
promo_category_summary$frequency_diff_pp <- 100 * (
  promo_category_summary$basics_sale_share -
    promo_category_summary$twin_sale_share
)
promo_category_summary$depth_diff_pp <- 100 * (
  promo_category_summary$basics_depth_if_sale -
    promo_category_summary$twin_depth_if_sale
)
promo_category_summary$interpretation <- vapply(
  seq_len(nrow(promo_category_summary)),
  function(i) {
    frequency_diff <- promo_category_summary$frequency_diff_pp[i]
    depth_diff <- promo_category_summary$depth_diff_pp[i]
    frequency_text <- if (frequency_diff > 0.05) {
      sprintf("More often (+%.1f pp)", frequency_diff)
    } else if (frequency_diff < -0.05) {
      sprintf("Less often (%.1f pp)", frequency_diff)
    } else {
      sprintf("Similar frequency (%+.1f pp)", frequency_diff)
    }
    depth_text <- if (is.na(depth_diff)) {
      if (is.na(promo_category_summary$basics_depth_if_sale[i]) &&
          is.na(promo_category_summary$twin_depth_if_sale[i])) {
        "No sales for either brand"
      } else if (is.na(promo_category_summary$basics_depth_if_sale[i])) {
        "No AB sales"
      } else {
        "No twin sales"
      }
    } else if (depth_diff > 0.05) {
      sprintf("Deeper cuts (+%.1f pp)", depth_diff)
    } else if (depth_diff < -0.05) {
      sprintf("Shallower cuts (%.1f pp)", depth_diff)
    } else {
      sprintf("Similar depth (%+.1f pp)", depth_diff)
    }
    paste(frequency_text, depth_text, sep = "; ")
  },
  character(1)
)
write.csv(
  promo_category_summary,
  file.path(output_dir, "promotional_category_summary.csv"),
  row.names = FALSE
)

format_percent <- function(value) {
  if (is.finite(value)) sprintf("%.1f%%", 100 * value) else "n/a"
}
png(
  file.path(output_dir, "promotional_category_summary.png"),
  width = 2200,
  height = 1300,
  res = 150
)
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(
  0,
  0,
  type = "n",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xaxs = "i",
  yaxs = "i",
  axes = FALSE,
  xlab = "",
  ylab = ""
)
text(
  0.5,
  0.965,
  "Promotional patterns by product category",
  cex = 1.55,
  font = 2
)
text(
  0.5,
  0.925,
  "Means across matched products. Frequency = % of observed days on sale; depth = average discount on sale days. Last column is AB minus Twin; pp = percentage points (AB = Amazon Basics).",
  cex = 0.88
)
text(0.025, 0.865, "Category", adj = 0, font = 2, cex = 1.02)
text(0.292, 0.865, "Pairs", font = 2, cex = 1.02)
text(0.43, 0.865, "On-sale frequency", font = 2, cex = 1.02)
text(0.64, 0.865, "Discount depth", font = 2, cex = 1.02)
text(0.78, 0.865, "What it means for AB", adj = 0, font = 2, cex = 1.02)
twin_color <- "grey25"
basics_color <- "#0072B2"
text(0.385, 0.83, "Twin", col = twin_color, font = 2, cex = 0.9)
text(0.485, 0.83, "AB", col = basics_color, font = 2, cex = 0.9)
text(0.59, 0.83, "Twin", col = twin_color, font = 2, cex = 0.9)
text(0.69, 0.83, "AB", col = basics_color, font = 2, cex = 0.9)
text(0.64, 0.805, "(sale days only; n with sales)", cex = 0.72)
abline(h = 0.79, col = "grey45")
row_positions <- seq(0.75, 0.21, length.out = nrow(promo_category_summary))
for (i in seq_len(nrow(promo_category_summary))) {
  row <- promo_category_summary[i, ]
  y <- row_positions[i]
  rect(
    0.015,
    y - 0.037,
    0.985,
    y + 0.037,
    col = if (i %% 2) "grey96" else "white",
    border = NA
  )
  text(0.025, y, row$category, adj = 0, cex = 0.92)
  text(0.292, y, row$matched_pairs, cex = 0.95)
  text(0.385, y, format_percent(row$twin_sale_share), col = twin_color, cex = 0.95)
  text(0.485, y, format_percent(row$basics_sale_share), col = basics_color, cex = 0.95)
  text(
    0.59,
    y,
    sprintf(
      "%s (%d)",
      format_percent(row$twin_depth_if_sale),
      row$twin_pairs_with_sales
    ),
    col = twin_color,
    cex = 0.88
  )
  text(
    0.69,
    y,
    sprintf(
      "%s (%d)",
      format_percent(row$basics_depth_if_sale),
      row$basics_pairs_with_sales
    ),
    col = basics_color,
    cex = 0.88
  )
  text(0.78, y, row$interpretation, adj = 0, cex = 0.88)
}
text(
  0.025,
  0.095,
  "Depth averages exclude products with no observed sales; n with sales is shown in parentheses.",
  adj = 0,
  cex = 0.85
)
text(
  0.025,
  0.06,
  "Frequency and depth are category averages across matched pairs; they describe this sample, not every Amazon product.",
  adj = 0,
  cex = 0.8,
  col = "grey35"
)
dev.off()

# ============ PILLAR 3: STRATEGIC INTERACTION & LEADER-FOLLOWER DYNAMICS ============
library(tidyverse)
library(fixest)
library(survival)

# ---- 3a. Panel & Lag Preparation ----
# Ensure daily/time-bucket alignment: pick the last observation per pair-brand-date
df_daily <- df %>%
  mutate(date = as_date(time_bucket)) %>%
  group_by(pair_id, is_basics, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Verify that every pair has both Amazon Basics (is_basics == 1) and Twin (is_basics == 0)
bad_pairs <- df_daily %>%
  group_by(pair_id) %>%
  summarize(roles = n_distinct(is_basics), .groups = "drop") %>%
  filter(roles != 2)

if (nrow(bad_pairs) > 0) {
  stop("Pairs missing an Amazon Basics or a Twin series: ", paste(bad_pairs$pair_id, collapse = ", "))
}

# Create a complete calendar balanced panel per pair and brand
pair_calendar <- df_daily %>%
  group_by(pair_id) %>%
  reframe(date = seq(min(date), max(date), by = "day"))

df_panel <- pair_calendar %>%
  left_join(df_daily %>% select(pair_id, is_basics, date, price), by = c("pair_id", "date")) %>%
  arrange(pair_id, is_basics, date) %>%
  group_by(pair_id, is_basics) %>%
  mutate(dlnp = log(price) - log(lag(price))) %>%
  ungroup()

# Reshape wide so both AB (is_basics == 1) and Twin (is_basics == 0) changes sit side-by-side
pair_date_cal <- df_daily %>%
  group_by(pair_id) %>%
  reframe(date = seq(min(date), max(date), by = "day"))

df_wide <- pair_date_cal %>%
  left_join(df_panel %>% filter(is_basics == 1) %>% select(pair_id, date, d_ab = dlnp), by = c("pair_id", "date")) %>%
  left_join(df_panel %>% filter(is_basics == 0) %>% select(pair_id, date, d_tw = dlnp), by = c("pair_id", "date")) %>%
  arrange(pair_id, date)

# Generate positive/negative components and K lags (e.g., K = 5 days) for distributed-lag modeling
K <- 5
for (s in c("ab", "tw")) {
  df_wide[[paste0("d_", s, "_pos")]] <- pmax(df_wide[[paste0("d_", s)]], 0)
  df_wide[[paste0("d_", s, "_neg")]] <- pmin(df_wide[[paste0("d_", s)]], 0)
  for (k in 0:K) {
    for (suf in c("", "_pos", "_neg")) {
      src <- paste0("d_", s, suf)
      df_wide[[paste0("L", k, suf, "_", s)]] <- ave(df_wide[[src]], df_wide$pair_id, FUN = function(z) dplyr::lag(z, k))
    }
  }
}

# ---- 3b. Stacked Distributed-Lag Regression & Response Curves ----
make_stack <- function(w) {
  mk <- function(follower, leader, fab) {
    out <- tibble(pair_id = w$pair_id, date = w$date, follower_is_ab = fab, y = w[[paste0("d_", follower)]])
    for (k in 0:K) {
      out[[paste0("x_L",  k)]] <- w[[paste0("L", k, "_",      leader)]]
      out[[paste0("xp_L", k)]] <- w[[paste0("L", k, "_pos_", leader)]]
      out[[paste0("xn_L", k)]] <- w[[paste0("L", k, "_neg_", leader)]]
    }
    out
  }
  bind_rows(mk("ab", "tw", 1L), mk("tw", "ab", 0L)) %>%
    mutate(role_id = paste(pair_id, follower_is_ab), day_str = as.character(date))
}

stk <- make_stack(df_wide)

lag_terms <- paste0("x_L", 0:K)
fml_resp <- as.formula(paste("y ~", paste(c(lag_terms, paste0(lag_terms, ":follower_is_ab")), collapse = " + "),
                             "| role_id + day_str^follower_is_ab"))
m_resp <- feols(fml_resp, data = stk, cluster = ~pair_id)
response_table <- etable(m_resp, headers = c("Distributed Lag Response Model"))
writeLines(
  capture.output(print(response_table)),
  file.path(output_dir, "strategic_response_regression.txt")
)
print(response_table)

# Helper function for linear combinations of coefficients
lincom <- function(m, terms, w = rep(1, length(terms))) {
  b <- coef(m)[terms]; V <- vcov(m)[terms, terms, drop = FALSE]
  est <- sum(w * b); se <- sqrt(drop(t(w) %T*% V %*% w)); dfree <- degrees_freedom(m, type = "t")
  tibble(est = est, se = se, lo = est - qt(.975, dfree) * se, hi = est + qt(.975, dfree) * se,
         p = 2 * pt(-abs(est / se), dfree))
}

# ---- 3c. Asymmetry (Response to Price Increases vs. Price Cuts) ----
fml_asym <- as.formula(paste("y ~",
  paste(c(paste0("xp_L", 0:K), paste0("xn_L", 0:K),
          paste0("xp_L", 0:K, ":follower_is_ab"), paste0("xn_L", 0:K, ":follower_is_ab")), collapse = " + "),
  "| role_id + day_str^follower_is_ab"))
m_asym <- feols(fml_asym, data = stk, cluster = ~pair_id)
asymmetry_table <- etable(m_asym, headers = c("Price Increase vs. Cut Response"))
writeLines(
  capture.output(print(asymmetry_table)),
  file.path(output_dir, "strategic_asymmetry_regression.txt")
)
print(asymmetry_table)

# ---- 3d. Response Time (Survival Analysis & Kaplan-Meier Curves) ----
min_change <- 0.01 # 1% log price change threshold to trigger an event
max_wait   <- 7    # 7-day follow-up window

make_events <- function(w, leader, follower) {
  w %>%
    group_by(pair_id) %>%
    group_modify(function(g, key) {
      dl <- g[[paste0("d_", leader)]]; dfw <- g[[paste0("d_", follower)]]; n <- length(dl)
      idx <- which(!is.na(dl) & abs(dl) >= min_change)
      map_dfr(idx, function(i) {
        js <- seq_len(min(max_wait, n - i))
        if (length(js) == 0) return(NULL)
        fol <- dfw[i + js]
        hit <- which(!is.na(fol) & abs(fol) >= min_change)
        if (length(hit) > 0) return(tibble(date = g$date[i], time = js[hit[1]], event = 1L, trig = dl[i]))
        seen <- which(!is.na(fol))
        if (length(seen) == 0) return(NULL)
        tibble(date = g$date[i], time = js[max(seen)], event = 0L, trig = dl[i])
      })
    }) %>%
    ungroup()
}

ev_df <- bind_rows(
    make_events(df_wide, "tw", "ab") %>% mutate(follower_is_ab = 1L),
    make_events(df_wide, "ab", "tw") %>% mutate(follower_is_ab = 0L)
  ) %>%
  mutate(
    trig_abs  = abs(trig),
    trig_down = as.integer(trig < 0),
    trig_dir  = if_else(trig < 0, "Price cut", "Price increase"),
    follower  = if_else(follower_is_ab == 1, "Amazon Basics follows Twin", "Twin follows Amazon Basics")
  )

# Kaplan-Meier Survival Curves for Response Time
km_all <- survfit(Surv(time, event) ~ follower, data = ev_df)

km_tidy <- function(fit) {
  s <- if (is.null(fit$strata)) rep("all", length(fit$time)) else rep(names(fit$strata), fit$strata)
  tibble(time = fit$time, surv = fit$surv, lo = fit$lower, hi = fit$upper, strata = s) %>%
    group_by(strata) %>%
    group_modify(~ bind_rows(tibble(time = 0, surv = 1, lo = 1, hi = 1), .x)) %>%
    ungroup() %>%
    mutate(follower = str_match(strata, "follower=([^,]+)")[, 2])
}

col_follow <- c("Amazon Basics follows Twin" = "#d95f02", "Twin follows Amazon Basics" = "grey40")

km_plot <- ggplot(km_tidy(km_all), aes(x = time, y = 1 - surv, color = follower, fill = follower)) +
  geom_ribbon(aes(ymin = 1 - hi, ymax = 1 - lo), alpha = 0.15, color = NA) +
  geom_step(linewidth = 1) +
  scale_color_manual(values = col_follow) +
  scale_fill_manual(values = col_follow) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Time-to-Response Survival Curves",
    subtitle = "Cumulative share of products responding to competitor price changes",
    x = "Days after Competitor Price Change",
    y = "Share That Has Responded",
    color = NULL, fill = NULL
  )
ggsave(
  file.path(output_dir, "strategic_response_survival.png"),
  plot = km_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Cox Proportional Hazards Model (testing response speed between Amazon Basics and twins)
cox_within <- coxph(Surv(time, event) ~ follower_is_ab + trig_abs + trig_down + strata(pair_id) + cluster(pair_id),
                    data = ev_df, ties = "efron")

cox_summary <- summary(cox_within)
writeLines(
  capture.output(print(cox_summary)),
  file.path(output_dir, "strategic_cox_model.txt")
)
print(cox_summary)