split.factors.from.recent.transactions <- function(symbol, tx.all.dates) {
  # TODO: make Reference.Symbol=Symbol, Price=0, and Quantity=(Split.Factor - 1) * Pos.Quantity.
  # Actually, to make it easier for us so that we don't have to track pos quantity: have two rows
  # with Reference.Symbol=Symbol and Price == 0 where one is Quantity=-Pos.Quantity and the other is
  # Quantity=Split.Factor*Pos.Quantity.
  NULL
}

adjust.price <- function(price, split.factors) {
  # TODO: 
  price
}

recent.transaction.price.provider <- list(
  available.dates=function(symbol) c(),
  # Users of the tx must use this to adjust historical trade quantities as well. Seems best to just
  #  process tx once for splits using price.provider in port.viz after the filter.tx call.
  split.factors=split.factors.from.recent.transactions,
  # The only exception in join.cost where cost is not equal to quantity times price is for corporate
  #  actions and fees.
  price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
    # Exclude dividends, fees, and journal transaction that washed out.
    tx.for.date <- tx.for.date[
      !is.corp.action.or.fee(tx.for.date) & (tx.for.date$Quantity != 0 | tx.for.date$Price != 0), ]
    price <- if (is.null(tx.for.date) || nrow(tx.for.date) == 0) NA
    # Simple average price. Position fully closed on same day and at same price it was opened.
    else if (all(tx.for.date$Quantity == 0)) mean(tx.for.date$Price)
    # Average price weighted by quantity.
    else sum(abs(tx.for.date$Cost)) / sum(abs(tx.for.date$Quantity))
    if (adj) {
      price <- adjust.price(price, split.factors.from.recent.transactions(symbol, tx.all.dates))
    }
    price
  },
  # The only exception in join.cost where price is not a traded price is for corporate actions and
  #  fees.
  low.price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
    # Exclude dividends, fees, and journal transaction that washed out.
    tx.for.date <- tx.for.date[
      !is.corp.action.or.fee(tx.for.date) & (tx.for.date$Quantity != 0 | tx.for.date$Price != 0), ]
    price <- if (is.null(tx.for.date) || nrow(tx.for.date) == 0) NA else min(tx.for.date$Price)
    if (adj) {
      price <- adjust.price(price, split.factors.from.recent.transactions(symbol, tx.all.dates))
    }
    price
  },
  high.price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
    # Exclude dividends, fees, and journal transaction that washed out.
    tx.for.date <- tx.for.date[
      !is.corp.action.or.fee(tx.for.date) & (tx.for.date$Quantity != 0 | tx.for.date$Price != 0), ]
    price <- if (is.null(tx.for.date) || nrow(tx.for.date) == 0) NA else max(tx.for.date$Price)
    if (adj) {
      price <- adjust.price(price, split.factors.from.recent.transactions(symbol, tx.all.dates))
    }
    price
  },
  dump=function() list(NULL))

split.factors.from.overrides <- function(symbol, tx.all.dates, fallback, override.prices) {
  # TODO: use override.prices$Split.Factor
  fallback$split.factors(symbol, tx.all.dates)
}

create.price.provider.with.overrides <- function(
    override.prices, fallback=recent.transaction.price.provider) {
  stopifnot(anyDuplicated(override.prices[, c("Date", "Symbol")]) == 0)
  
  override.prices <- override.prices[, c("Symbol", "Date", "High", "Low", "Close", "Split.Factor")]
  # Convert the data.frame into environments to improve performance since data.frames do not support
  #  indexing.
  override.dates <- list2env(split(override.prices$Date, override.prices$Symbol))
  overrides.by.symbol <- list2env(lapply(
    split(override.prices, override.prices$Symbol), function(for.symbol) {
      rownames(for.symbol) <- as.character(for.symbol$Date)
      for.symbol
    }))
  
  list(
    split.factors=function(symbol, tx.all.dates) {
      split.factors.from.overrides(symbol, tx.all.dates, fallback, override.prices)
    },
    available.dates=function(symbol) {
      for.symbol <- if (length(override.dates[[symbol]]) > 0)
        override.dates[[symbol]]
      else
        as.Date(integer(), origin="1970-01-01")
      c(for.symbol, fallback$available.dates(symbol))
    },
    price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
      override.price <- overrides.by.symbol[[symbol]][as.character(date), "Close"]
      price <- if (length(override.price) > 0 && !is.na(override.price))
        override.price
      else
        fallback$price(symbol, date, tx.for.date, tx.all.dates, FALSE)
      if (adj) {
        price <- adjust.price(
          price, split.factors.from.overrides(symbol, tx.all.dates, fallback, override.prices))
      }
      price
    },
    low.price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
      override.price <- overrides.by.symbol[[symbol]][as.character(date), "Low"]
      low.prices <- c(
        fallback$low.price(symbol, date, tx.for.date, tx.all.dates, FALSE), override.price)
      price <- if (all(is.na(low.prices)))
        NA
      else
        min(low.prices, na.rm=TRUE)
      if (adj) {
        price <- adjust.price(
          price, split.factors.from.overrides(symbol, tx.all.dates, fallback, override.prices))
      }
      price
    },
    high.price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
      override.price <- overrides.by.symbol[[symbol]][as.character(date), "High"]
      high.prices <- c(
        fallback$high.price(symbol, date, tx.for.date, tx.all.dates, FALSE), override.price)
      price <- if (all(is.na(high.prices)))
        NA
      else
        max(high.prices, na.rm=TRUE)
      if (adj) {
        price <- adjust.price(
          price, split.factors.from.overrides(symbol, tx.all.dates, fallback, override.prices))
      }
      price
    },
    dump=function() rbind(override.prices, fallback$dump()))
}

filter.price.provider.dates <- function(price.provider, from.date, to.date) {
  # There seems to be infinite recursion unless this is evaluated eagerly.
  force(price.provider)
  list(
    split.factors=function(symbol, tx.all.dates) {
      price.provider$split.factors(symbol, tx.all.dates)
    },
    available.dates=function(symbol) {
      d <- price.provider$available.dates(symbol)
      d[d >= from.date & d <= to.date]
    },
    price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
      if (date >= from.date & date <= to.date)
        price.provider$price(symbol, date, tx.for.date, tx.all.dates, adj)
      else
        NA
    },
    low.price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
      if (date >= from.date & date <= to.date)
        price.provider$low.price(symbol, date, tx.for.date, tx.all.dates, adj)
      else
        NA
    },
    high.price=function(symbol, date, tx.for.date, tx.all.dates, adj=TRUE) {
      if (date >= from.date & date <= to.date)
        price.provider$high.price(symbol, date, tx.for.date, tx.all.dates, adj)
      else
        NA
    },
    dump=function() {
      d <- price.provider$dump()
      d[d$Date >= from.date & d$Date <= to.date, ]
    })
}
