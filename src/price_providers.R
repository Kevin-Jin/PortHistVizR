recent.transaction.price.provider <- list(
  available.dates=function(symbol) c(),
  # The only exception in join.cost where cost is not equal to quantity times price is when the
  #  reference symbol is non-empty.
  price=function(symbol, date, tx.for.date) {
    # Exclude dividends, fees, and journal transaction that washed out.
    tx.for.date <- tx.for.date[
      tx.for.date$Reference.Symbol == "" & (tx.for.date$Quantity != 0 | tx.for.date$Price != 0), ]
    if (is.null(tx.for.date) || nrow(tx.for.date) == 0) NA
    # Simple average price. Position fully closed on same day and at same price it was opened.
    else if (all(tx.for.date$Quantity == 0)) mean(tx.for.date$Price)
    # Average price weighted by quantity.
    else sum(abs(tx.for.date$Cost)) / sum(abs(tx.for.date$Quantity))
  },
  # The only exception in join.cost where price is not a traded price is when the reference symbol
  #  is non-empty.
  low.price=function(symbol, date, tx.for.date) {
    # Exclude dividends, fees, and journal transaction that washed out.
    tx.for.date <- tx.for.date[
      tx.for.date$Reference.Symbol == "" & (tx.for.date$Quantity != 0 | tx.for.date$Price != 0), ]
    if (is.null(tx.for.date) || nrow(tx.for.date) == 0) NA else min(tx.for.date$Price)
  },
  high.price=function(symbol, date, tx.for.date) {
    # Exclude dividends, fees, and journal transaction that washed out.
    tx.for.date <- tx.for.date[
      tx.for.date$Reference.Symbol == "" & (tx.for.date$Quantity != 0 | tx.for.date$Price != 0), ]
    if (is.null(tx.for.date) || nrow(tx.for.date) == 0) NA else max(tx.for.date$Price)
  },
  dump=function() list(NULL))

create.price.provider.with.overrides <- function(
    override.prices, fallback=recent.transaction.price.provider) {
  stopifnot(anyDuplicated(override.prices[, c("Date", "Symbol")]) == 0)
  
  override.prices <- override.prices[, c("Symbol", "Date", "High", "Low", "Close")]
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
    },
    dump=function() rbind(override.prices, fallback$dump()))
}

filter.price.provider.dates <- function(price.provider, from.date, to.date) {
  # There seems to be infinite recursion unless this is evaluated eagerly.
  force(price.provider)
  list(
    available.dates=function(symbol) {
      d <- price.provider$available.dates(symbol)
      d[d >= from.date & d <= to.date]
    },
    price=function(symbol, date, tx.for.date) {
      if (date >= from.date & date <= to.date) price.provider$price(symbol, date, tx.for.date)
      else NA
    },
    low.price=function(symbol, date, tx.for.date) {
      if (date >= from.date & date <= to.date) price.provider$low.price(symbol, date, tx.for.date)
      else NA
    },
    high.price=function(symbol, date, tx.for.date) {
      if (date >= from.date & date <= to.date) price.provider$high.price(symbol, date, tx.for.date)
      else NA
    },
    dump=function() {
      d <- price.provider$dump()
      d[d$Date >= from.date & d$Date <= to.date, ]
    })
}
