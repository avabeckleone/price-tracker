#!/usr/bin/env Rscript

# Analyze the price tracker CSV using base R only.
# Usage: Rscript analysis.r [input.csv] [output-directory]
# Tony testing
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 2L) {
  stop("Usage: Rscript analysis.r [input.csv] [output-directory]", call. = FALSE)
}

script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_argument)) {
  sub("^--file=", "", script_argument[[1L]])
} else {
  file.path(getwd(), "analysis.r")
}
script_directory <- dirname(normalizePath(script_path, mustWork = FALSE))

input_path <- if (length(arguments) >= 1L) {
  arguments[[1L]]
} else {
  file.path(script_directory, "prices.csv")
}
output_directory <- if (length(arguments) >= 2L) {
  arguments[[2L]]
} else {
  file.path(script_directory, "analysis_output")
}

if (!file.exists(input_path)) {
  stop(sprintf("Input CSV not found: %s", input_path), call. = FALSE)
}

prices <- read.csv(
  input_path,
  colClasses = "character",
  na.strings = c("", "NA"),
  check.names = FALSE,
  fileEncoding = "UTF-8"
)
names(prices) <- sub("^\ufeff", "", names(prices))

required_columns <- c("timestamp", "asin", "price", "status")
missing_columns <- setdiff(required_columns, names(prices))
if (length(missing_columns)) {
  stop(
    sprintf("Input is missing required columns: %s", paste(missing_columns, collapse = ", ")),
    call. = FALSE
  )
}
if (!nrow(prices)) {
  stop("Input CSV contains no data rows.", call. = FALSE)
}

optional_columns <- c("title", "regular_price", "non_deal_price")
for (column in setdiff(optional_columns, names(prices))) {
  prices[[column]] <- NA_character_
}

parse_price <- function(value) {
  value <- trimws(value)
  value[!nzchar(value)] <- NA_character_
  value <- gsub(",", "", value, fixed = TRUE)
  value <- gsub("[^0-9.-]", "", value)
  suppressWarnings(as.numeric(value))
}

timestamp_text <- trimws(prices$timestamp)
timestamp_text[is.na(timestamp_text)] <- ""
minute_precision <- grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$", timestamp_text)
timestamp_for_parse <- timestamp_text
timestamp_for_parse[minute_precision] <- paste0(timestamp_text[minute_precision], ":00")
timestamp_parsed <- suppressWarnings(
  as.POSIXct(timestamp_for_parse, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
)
timestamp_valid <- !is.na(timestamp_parsed) &
  format(timestamp_parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC") == timestamp_for_parse

prices$price_numeric <- parse_price(prices$price)
prices$regular_price_numeric <- parse_price(prices$regular_price)
prices$non_deal_price_numeric <- parse_price(prices$non_deal_price)
prices$capture_date <- ifelse(timestamp_valid, substr(timestamp_text, 1L, 10L), NA_character_)

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

missingness_summary <- data.frame(
  column = names(prices),
  missing_count = vapply(prices, function(column) sum(is.na(column) | !nzchar(trimws(column))), integer(1L)),
  present_count = vapply(prices, function(column) sum(!is.na(column) & nzchar(trimws(column))), integer(1L)),
  stringsAsFactors = FALSE
)
write.csv(
  missingness_summary,
  file.path(output_directory, "missingness_summary.csv"),
  row.names = FALSE,
  na = ""
)

status_values <- trimws(prices$status)
status_values[is.na(status_values) | !nzchar(status_values)] <- "(missing)"
status_counts <- sort(table(status_values), decreasing = TRUE)
status_summary <- data.frame(
  status = names(status_counts),
  row_count = as.integer(status_counts),
  stringsAsFactors = FALSE
)
write.csv(
  status_summary,
  file.path(output_directory, "status_summary.csv"),
  row.names = FALSE
)

duplicate_capture_count <- sum(duplicated(prices[c("timestamp", "asin")]))
data_quality <- data.frame(
  metric = c(
    "rows",
    "unique_products",
    "valid_timestamps",
    "invalid_timestamps",
    "valid_prices",
    "invalid_or_missing_prices",
    "duplicate_timestamp_asin_rows"
  ),
  value = c(
    nrow(prices),
    length(unique(prices$asin[!is.na(prices$asin) & nzchar(prices$asin)])),
    sum(timestamp_valid),
    sum(!timestamp_valid),
    sum(!is.na(prices$price_numeric)),
    sum(is.na(prices$price_numeric)),
    duplicate_capture_count
  ),
  stringsAsFactors = FALSE
)
write.csv(
  data_quality,
  file.path(output_directory, "data_quality_summary.csv"),
  row.names = FALSE
)

product_ids <- unique(prices$asin[!is.na(prices$asin) & nzchar(trimws(prices$asin))])
product_summary <- do.call(rbind, lapply(product_ids, function(product_id) {
  product_indices <- which(!is.na(prices$asin) & prices$asin == product_id)
  product_rows <- prices[product_indices, , drop = FALSE]
  product_title <- product_rows$title[!is.na(product_rows$title) & nzchar(trimws(product_rows$title))]
  priced_rows <- product_rows[!is.na(product_rows$price_numeric), , drop = FALSE]
  product_timestamps <- timestamp_parsed[product_indices]
  if (nrow(priced_rows)) {
    product_prices <- priced_rows$price_numeric
    priced_timestamps <- product_timestamps[!is.na(product_rows$price_numeric)]
    if (any(!is.na(priced_timestamps))) {
      latest_price <- product_prices[which.max(ifelse(
        is.na(priced_timestamps),
        -Inf,
        as.numeric(priced_timestamps)
      ))]
    } else {
      latest_price <- product_prices[[length(product_prices)]]
    }
  } else {
    product_prices <- numeric()
    latest_price <- NA_real_
  }

  valid_product_timestamps <- product_timestamps[!is.na(product_timestamps)]
  data.frame(
    asin = product_id,
    title = if (length(product_title)) product_title[[1L]] else NA_character_,
    capture_count = nrow(product_rows),
    successful_capture_count = sum(product_rows$status == "ok", na.rm = TRUE),
    priced_capture_count = length(product_prices),
    latest_timestamp = if (length(valid_product_timestamps)) {
      format(max(valid_product_timestamps), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else {
      NA_character_
    },
    latest_priced_value = latest_price,
    minimum_price = if (length(product_prices)) min(product_prices) else NA_real_,
    median_price = if (length(product_prices)) median(product_prices) else NA_real_,
    mean_price = if (length(product_prices)) mean(product_prices) else NA_real_,
    maximum_price = if (length(product_prices)) max(product_prices) else NA_real_,
    stringsAsFactors = FALSE
  )
}))
product_summary <- product_summary[order(product_summary$asin), , drop = FALSE]
write.csv(
  product_summary,
  file.path(output_directory, "product_summary.csv"),
  row.names = FALSE,
  na = ""
)

daily_rows <- prices[!is.na(prices$capture_date) & !is.na(prices$price_numeric), , drop = FALSE]
if (nrow(daily_rows)) {
  daily_groups <- split(daily_rows, interaction(daily_rows$capture_date, daily_rows$asin, drop = TRUE))
  daily_summary <- do.call(rbind, lapply(daily_groups, function(group) {
    data.frame(
      capture_date = group$capture_date[[1L]],
      asin = group$asin[[1L]],
      capture_count = nrow(group),
      mean_price = mean(group$price_numeric),
      minimum_price = min(group$price_numeric),
      maximum_price = max(group$price_numeric),
      stringsAsFactors = FALSE
    )
  }))
  daily_summary <- daily_summary[order(daily_summary$capture_date, daily_summary$asin), , drop = FALSE]
} else {
  daily_summary <- data.frame(
    capture_date = character(),
    asin = character(),
    capture_count = integer(),
    mean_price = numeric(),
    minimum_price = numeric(),
    maximum_price = numeric(),
    stringsAsFactors = FALSE
  )
}
write.csv(
  daily_summary,
  file.path(output_directory, "daily_price_summary.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  prices,
  file.path(output_directory, "cleaned_prices.csv"),
  row.names = FALSE,
  na = ""
)

cat(sprintf("Analyzed %s (%s rows, %s products).\n",
            input_path, nrow(prices), length(product_ids)))
cat(sprintf("Results written to: %s\n", normalizePath(output_directory)))
cat("The source CSV was not modified; unsuccessful captures and duplicate rows are retained.\n")
