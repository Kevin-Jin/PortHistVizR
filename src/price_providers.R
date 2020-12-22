recent.transaction.price.provider <- list(
  available.dates=function(symbol) c(),
  price=function(symbol, date, tx.for.date)
    if (is.null(tx.for.date)) NA
    # Price of cash should always be $1.00 even if we acquire cash costlessly through a dividend.
    # This is the only exception in join.cost where cost is not equal to price times quantity.
    else if (symbol == "$") mean(tx.for.date$Price)
    else sum(abs(tx.for.date$Cost)) / sum(abs(tx.for.date$Quantity)),
  low.price=function(symbol, date, tx.for.date)
    if (is.null(tx.for.date)) NA else min(tx.for.date$Price),
  high.price=function(symbol, date, tx.for.date)
    if (is.null(tx.for.date)) NA else max(tx.for.date$Price))

create.price.provider.with.overrides <- function(
    override.prices, fallback=recent.transaction.price.provider) {
  stopifnot(anyDuplicated(override.prices[, c("Date", "Symbol")]) == 0)
  
  # Convert the data.frame into environments to improve performance since data.frames do not support
  #  indexing.
  override.dates <- list2env(split(override.prices$Date, override.prices$Symbol))
  overrides.by.symbol <- list2env(lapply(
    split(override.prices, override.prices$Symbol), function(for.symbol) {
      rownames(for.symbol) <- as.character(for.symbol$Date)
      for.symbol
    }))
  
  list(
    available.dates=function(symbol) {
      for.symbol <- if (length(override.dates[[symbol]]) > 0)
        override.dates[[symbol]]
      else
        as.Date(integer(), origin="1970-01-01")
      c(for.symbol, fallback$available.dates(symbol))
    },
    price=function(symbol, date, tx.for.date) {
      override.price <- overrides.by.symbol[[symbol]][as.character(date), "Close"]
      if (length(override.price) > 0 && !is.na(override.price))
        override.price
      else
        fallback$price(symbol, date, tx.for.date)
    },
    low.price=function(symbol, date, tx.for.date) {
      override.price <- overrides.by.symbol[[symbol]][as.character(date), "Low"]
      low.prices <- c(fallback$low.price(symbol, date, tx.for.date), override.price)
      if (all(is.na(low.prices)))
        NA
      else
        min(low.prices, na.rm=TRUE)
    },
    high.price=function(symbol, date, tx.for.date) {
      override.price <- overrides.by.symbol[[symbol]][as.character(date), "High"]
      high.prices <- c(fallback$high.price(symbol, date, tx.for.date), override.price)
      if (all(is.na(high.prices)))
        NA
      else
        max(high.prices, na.rm=TRUE)
    })
}
