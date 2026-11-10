prices <- read.csv(
  "prices.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

print(head(prices))

if (interactive()) {
  View(prices)
}