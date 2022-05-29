lapply.and.set.names <- function(names, fn) {
  setNames(lapply(names, fn), names)
}

reduce.on.factor <- function(frame, factor.name, func) {
  # This preserves order of each factor's first appearance in frame as opposed to natural ordering
  #  under split's default behavior of calling as.factor.
  factor.values <- factor(
    as.character(frame[, factor.name]), levels=as.character(unique(frame[, factor.name])))
  frame.by.factor <- lapply(
    split(frame[, names(frame) != factor.name, drop=FALSE], factor.values), func)
  frame.by.factor <- lapply(
    names(frame.by.factor), function(for.factor) cbind(
      setNames(data.frame(for.factor, stringsAsFactors=FALSE), factor.name),
      # This, unlike frame.by.factor[[for.factor]], lets us index list elements named "". For some
      #  reason, is.null(setNames(list(1), "")[[""]]) but names(setNames(list(1), "")) == "".
      frame.by.factor[[which(names(frame.by.factor) == for.factor)]]))
  frame.by.factor <- do.call(rbind, frame.by.factor)

  setAs("character", "Date", function(from) as.Date(from))
  frame.by.factor[, factor.name] <- as(frame.by.factor[, factor.name], class(frame[, factor.name]))

  row.names(frame.by.factor) <- c()
  frame.by.factor
}

coalesce.tx.by.date.symbol.price <- function(tx) {
  # This does not maintain the order of all transactions under a date but it does maintain the order
  #  of all transactions for a specific symbol under a date if all of those transactions were done
  #  at different prices. For example, the sequence (1) buy 1 ABC at $1.00, (2) buy 1 XYZ at $1.99,
  #  (3) buy 1 ABC at $0.99, (4) buy 1 XYZ at $2.00 will be reordered to (1) buy 1 ABC at $1.00,
  #  (2) buy 1 ABC at $0.99, (3) buy 1 XYZ at $1.99, (4) buy 1 XYZ at $2.00.
  # TODO: support vector of factor names to reduce.on.factor for factor interaction to eliminate
  #  this limitation. The result would then maintain the order of the first appearances of a
  #  (Date, Symbol, Price) tuple.
  reduce.on.factor(tx, "Date", function(tx.for.date) {
    reduce.on.factor(tx.for.date, "Symbol", function(tx.for.symbol) {
      reduce.on.factor(tx.for.symbol, "Reference.Symbol", function(tx.for.ref.symbol) {
        reduce.on.factor(tx.for.ref.symbol, "Price", function(tx.for.price) {
          # Make sure we're not dropping any columns here.
          stopifnot(colnames(tx.for.price) == "Quantity")
          data.frame(Quantity=round(sum(tx.for.price$Quantity), QTY_FRAC_DIGITS))
        })
      })
    })
  })
}

join.cost <- function(tx, adjust.securities.costs.by.dividends.and.fees=TRUE) {
  tx$Cost <- tx$Price * tx$Quantity
  # Cash transactions with a Reference.Symbol are "involuntary transactions" that are not a contra
  #  action to a voluntary action. The cost of acquiring cash should only increase when we deposit
  #  money into the account or sell a security. The cost of cash positions should only decline when
  #  we withdraw money out of the account or buy a security. Cash acquisitions from dividends are
  #  at no cost to us. Cash losses from fees and commissions do not unintuitively decrease the cost
  #  of acquiring cash.
  # This smooths out the portfolio's capital gains time series since capital gains that decline due
  #  to a share price decline on the ex-dividend date should be offset by a "capital gain" in the
  #  cash balance on the dividend payment date, though there will be a dip in the portfolio's gain
  #  in between those dates. This should also make the portfolio's overall cost perfectly flat in
  #  between deposits and withdrawals so that day over day gain is exactly equal to the change in
  #  portfolio value as long as no deposits or withdrawals are made that day.
  stopifnot(all(tx$Symbol[tx$Reference.Symbol != ""] == "$"))
  if (adjust.securities.costs.by.dividends.and.fees) {
    # Rather than keep the cost of cash positions constant on involuntary actions, lower the cost of
    #  securities positions on dividends and raise their cost on fees. This attributes the impact of
    #  dividends to the symbol that paid them rather than just mixing together dividends across all
    #  symbols into the gain for cash positions. For securities with substantial transaction costs,
    #  like options, this reveals the true cost of acquiring those positions.
    # Create a copy of each involuntary transaction. One will be the cash transaction, with cost
    #  left equal to value, and one will be a transaction that impacts the cost of the security but
    #  not its value.
    tx$Row.Order <- seq_len(nrow(tx))
    involuntaries <- tx[tx$Reference.Symbol != "", ]
    if (nrow(involuntaries) != 0) {
      last.involuntary.row <- 0
      new.tx <- NULL
      for (i in seq_len(nrow(involuntaries))) {
        cost.adj <- involuntaries[i, ]
        cost.adj$Symbol <- cost.adj$Reference.Symbol
        # Leave security's value the same.
        cost.adj$Quantity <- 0
        cost.adj$Price <- 0
        # Increase to cash positions is a decrease to securities positions and vice versa.
        cost.adj$Cost <- -cost.adj$Cost
        # tx[involuntaries$Row.Order[i]] is the cash transaction, so we just need to insert the
        #  security transaction after it.
        new.tx <- rbind(
          new.tx, tx[(last.involuntary.row + 1):involuntaries$Row.Order[i], ], cost.adj)
        last.involuntary.row <- involuntaries$Row.Order[i]
        # Leave Reference.Symbol for both the cash transaction and security transaction non-empty
        #  because some routines, like recent.transaction.price.provider$price, use the presence of
        #  that field to perform different logic for involuntary actions.
      }
      if (last.involuntary.row != nrow(tx)) {
        new.tx <- rbind(new.tx, tx[(last.involuntary.row + 1):nrow(tx), ])
      }
      tx <- new.tx
    }
  } else {
    tx$Cost[tx$Reference.Symbol != ""] <- 0
  }
  tx
}

join.pct.drawdown <- function(tx) {
  tx$Row.Order <- seq_len(nrow(tx))
  tx <- reduce.on.factor(tx, "Symbol", function(tx.for.symbol) {
    peak.price <- max(tx.for.symbol$Price)
    tx.for.symbol$Pct.Drawdown <- (1 - tx.for.symbol$Price / peak.price) * 100
    tx.for.symbol
  })
  tx[order(tx$Row.Order), -which(colnames(tx) == "Row.Order")]
}

get.peak.price.from.pct.drawdown <- function(tx) {
  round(sum(tx$Price) / sum(1 - tx$Pct.Drawdown / 100), PX_FRAC_DIGITS)
}

filter.tx <- function(tx, symbols, from.date, to.date) {
  tx <- tx[
    (is.null(symbols) | tx$Symbol %in% symbols | is.options.symbol(tx$Symbol))
    & tx$Date >= from.date & tx$Date <= to.date, ]
  tx
}

calc.portfolio.size.snapshot <- function(
    tx, price.provider, date, size.vs.time.tables, peak.prices) {
  avg.price <- function(tx.for.factor) {
    cost <- round(sum(tx.for.factor$Cost), CALC_FRAC_DIGITS)
    quantity <- round(sum(tx.for.factor$Quantity), QTY_FRAC_DIGITS)
    peak.price <- get.peak.price.from.pct.drawdown(tx.for.factor)
    unit.cost <- cost / quantity
    data.frame(
      Quantity=quantity,
      Unit.Cost=unit.cost,
      Cost=cost,
      Cost.Pct.Drawdown=(1 - unit.cost / peak.price) * 100,
      Peak.Price=peak.price,
      Full.Rebound.Gain=round(quantity * peak.price - cost, CALC_FRAC_DIGITS))
  }
  
  get.size.vs.time.table <- function(symbol) {
    if (symbol %in% names(size.vs.time.tables)) {
      size.vs.time.tables[[symbol]]
    } else {
      calc.size.vs.time(tx, symbol, price.provider)
    }
  }
  
  get.peak.price <- function(symbol) {
    if (symbol %in% names(peak.prices)) {
      peak.prices[[symbol]]
    } else {
      for.symbol <- tx[tx$Symbol == symbol, c("Price", "Pct.Drawdown")]
      get.peak.price.from.pct.drawdown(for.symbol)
    }
  }
  
  agg.by.symbol <- do.call(rbind, lapply(names(size.vs.time.tables), function(symbol) {
    size.vs.time <- size.vs.time.tables[[symbol]]
    size.vs.time$Quantity <- round(cumsum(size.vs.time$Quantity), QTY_FRAC_DIGITS)
    # TODO: utilize the price.provider to calculate peak.price in join.pct.drawdown so that
    #  Peak.Price == max(size.vs.time$High.Price[size.vs.time$Quantity != 0]).
    size.vs.time$Peak.Price <- get.peak.price(symbol)
    # calc.portfolio.size.snapshot.with.day.over.day.gain passes the same size.vs.time.tables
    #  structure for both T and T - 1. Any positions that were entered on T will still show up in
    #  the tables even though no positions existed in T - 1.
    snapshot.date <- max(as.Date("0001-01-01"), size.vs.time$Date[size.vs.time$Date <= date])
    size.vs.time <- size.vs.time[size.vs.time$Date == snapshot.date, ]
    size.vs.time$Symbol <- rep(symbol, nrow(size.vs.time))
    size.vs.time$Unit.Cost <- size.vs.time$Cost / size.vs.time$Quantity
    size.vs.time$Cost.Pct.Drawdown <- (1 - size.vs.time$Unit.Cost / size.vs.time$Peak.Price) * 100
    size.vs.time$Full.Rebound.Gain <- round(
      size.vs.time$Quantity * size.vs.time$Peak.Price - size.vs.time$Cost, CALC_FRAC_DIGITS)
    col.names <- c(
      "Symbol", "Quantity", "Unit.Cost", "Cost", "Cost.Pct.Drawdown", "Peak.Price",
      "Full.Rebound.Gain")
    size.vs.time[, col.names]
  }))
  
  remaining.tx <- tx[!(tx$Symbol %in% agg.by.symbol$Symbol) & tx$Date <= date, ]
  if (nrow(remaining.tx) != 0) {
    agg.by.symbol <- rbind(agg.by.symbol, reduce.on.factor(remaining.tx, "Symbol", avg.price))
  }
  agg.by.symbol <- agg.by.symbol[order(agg.by.symbol$Cost), ]
  row.names(agg.by.symbol) <- c()
  
  agg.by.ref.sym <- reduce.on.factor(
    tx[tx$Reference.Symbol != "", ], "Reference.Symbol", function(tx.for.ref.sym) {
      data.frame(
        Dividends=sum(
          (tx.for.ref.sym$Quantity * tx.for.ref.sym$Price)[tx.for.ref.sym$Quantity > 0]),
        Fees=sum((tx.for.ref.sym$Quantity * tx.for.ref.sym$Price)[tx.for.ref.sym$Quantity < 0]))
    })
  if (is.null(agg.by.ref.sym)) {
    agg.by.ref.sym <- data.frame(
      matrix(ncol=3, nrow=0, dimnames=list(NULL, c("Reference.Symbol", "Dividends", "Fees"))))
  }
  agg.by.ref.sym <- agg.by.ref.sym[
    match(agg.by.symbol$Symbol, agg.by.ref.sym$Reference.Symbol),
    colnames(agg.by.ref.sym) != "Reference.Symbol", drop=FALSE]
  agg.by.ref.sym[is.na(agg.by.ref.sym)] <- 0
  agg.by.symbol <- cbind(agg.by.symbol, agg.by.ref.sym)
  
  agg.by.symbol$Unit.Value <- unlist(lapply(
    agg.by.symbol$Symbol,
    function(symbol) get.current.price(get.size.vs.time.table(symbol), date)))
  agg.by.symbol$Value <- round(agg.by.symbol$Quantity * agg.by.symbol$Unit.Value, CALC_FRAC_DIGITS)
  agg.by.symbol$Value.Pct.Drawdown <- (
    1 - agg.by.symbol$Unit.Value / agg.by.symbol$Peak.Price) * 100
  agg.by.symbol$Gain <- round(agg.by.symbol$Value - agg.by.symbol$Cost, CALC_FRAC_DIGITS)
  
  agg.by.symbol
}

group.options.and.total.in.portfolio.size.snapshot <- function(agg.by.symbol, misc.symbols) {
  aggregate.on.portfolio <- function(group.name, agg.over) {
    # TODO: calculate portfolio Pct.Drawdown.
    agg.portfolio <- cbind(
      Symbol=group.name,
      as.data.frame(t(colSums(
        agg.over[, c(
          "Cost", "Full.Rebound.Gain", "Value", "Dividends", "Fees", "Gain",
          "Day.Over.Day.Gain")]))),
      stringsAsFactors=FALSE)
    fill.cols <- names(agg.over)[!(names(agg.over) %in% names(agg.portfolio))]
    agg.portfolio <- cbind(
      agg.portfolio,
      t(setNames(rep(NA, length(fill.cols)), fill.cols)))[names(agg.over)]
    agg.portfolio
  }
  
  agg.options <- agg.by.symbol[is.options.symbol(agg.by.symbol$Symbol), ]
  options.roots <- sub("\\s+$", "", substr(agg.options$Symbol, 1, 6))
  agg.options <- split(agg.options, options.roots)
  agg.options <- do.call(rbind, lapply(
    names(agg.options),
    function(root) aggregate.on.portfolio(paste(root, "options"), agg.options[[root]])))
  # TODO: sort agg.options by net cost
  
  agg.all <- rbind(agg.by.symbol, agg.options)
  if (length(misc.symbols) != 0) {
    agg.all <- rbind(
      agg.all, aggregate.on.portfolio("Miscellaneous", agg.all[agg.all$Symbol %in% misc.symbols, ]))
  }
  # The calling function wants both the aggregate miscellaneous size and options size per root as
  #  well as the breakdowns, so both are present in agg.all. Make sure we don't double count
  #  miscellaneous and options symbols in the portfolio aggregate though.
  agg.all <- rbind(
    agg.all,
    aggregate.on.portfolio(
      "Portfolio",
      agg.all[agg.all$Symbol != "Miscellaneous" & !is.options.symbol(agg.all$Symbol), ]))
  agg.all
}

calc.portfolio.size.snapshot.with.day.over.day.gain <- function(
    tx, price.provider=recent.transaction.price.provider, date=get.current.date(tx, price.provider),
    size.vs.time.tables=NULL, peak.prices=NULL) {
  # We need to account for max(tx$Date) since Fidelity adds T + 1 transfers into BrokerageLink to
  #  the history on T, so max(tx$Date) can potentially be in the future in which case we want to
  #  make sure we include the transactions on that date when snapping our positions. Today's price
  #  will just be carried forward into the future date by get.current.price.
  agg.all <- calc.portfolio.size.snapshot(
    tx, price.provider, max(tx$Date, date), size.vs.time.tables, peak.prices)
  if (any(tx$Date <= date - 1)) {
    # If the latest price date is a Monday, get.current.price will carry forward Friday to Sunday.
    agg.all.yday <- calc.portfolio.size.snapshot(
      tx, price.provider, date - 1, size.vs.time.tables, peak.prices)
  } else {
    agg.all.yday <- data.frame(matrix(ncol=1, nrow=0, dimnames=list(NULL, c("Symbol"))))
  }
  
  yday.idx <- match(agg.all$Symbol, agg.all.yday$Symbol)
  yday.gain <- ifelse(is.na(yday.idx), 0, agg.all.yday$Gain[yday.idx])
  cbind(agg.all, Day.Over.Day.Gain=round(agg.all$Gain - yday.gain, CALC_FRAC_DIGITS))
}

solve.entry.price.and.quantity <- function(tx, target.net.pct.drawdown, target.portfolio.net.cost) {
  # TODO: pass in target percentage allocation of each asset and we can perform an optimization on
  #  portfolio's net % drawdown. Right now, we can only perform an optimization on each symbol's net
  #  % drawdown.
  current.portfolio.net.cost <- sum(tx$Cost)
  reduce.on.factor(tx, "Symbol", function(tx.for.symbol) {
    peak.price <- get.peak.price.from.pct.drawdown(tx.for.symbol)
    current.net.quantity <- sum(tx.for.symbol$Quantity)
    current.net.cost <- sum(tx.for.symbol$Cost)
    target.net.price <- peak.price * (1 - target.net.pct.drawdown / 100)
    # Net cost for this single symbol that will enable us to hit the target portfolio net cost
    #  objective when no additional transactions are performed for any other symbol.
    target.net.cost <- target.portfolio.net.cost - (current.portfolio.net.cost - current.net.cost)
    target.marginal.cost <- target.net.cost - current.net.cost
    target.marginal.quantity <- target.net.cost / target.net.price - current.net.quantity
    target.marginal.price <- target.marginal.cost / target.marginal.quantity
    target.marginal.pct.drawdown <- (1 - target.marginal.price / peak.price) * 100
    target.full.rebound.gain <- (
      target.marginal.quantity + current.net.quantity) * peak.price - target.net.cost
    data.frame(
      Quantity=target.marginal.quantity, Price=target.marginal.price, Cost=target.marginal.cost,
      Pct.Drawdown=target.marginal.pct.drawdown, Full.Rebound.Gain=target.full.rebound.gain)
  })
}

calc.price.vs.time <- function(tx, symbol) {
  tx <- tx[tx$Symbol == symbol, ]
  peak.price <- get.peak.price.from.pct.drawdown(tx)
  tx <- tx[, c("Date", "Quantity", "Cost")]
  if (any(tx$Quantity > 0)) {
    purchase.prices <- reduce.on.factor(
      tx[tx$Quantity > 0, ], "Date", function(for.date) data.frame(
        Average=sum(for.date$Cost) / sum(for.date$Quantity),
        Low=min(for.date$Cost / for.date$Quantity),
        High=max(for.date$Cost / for.date$Quantity)))
    purchase.prices <- purchase.prices[order(purchase.prices$Date), ]
  } else {
    purchase.prices <- data.frame(
      Date=as.Date(character()), Average=numeric(), Low=numeric(), High=numeric())
  }
  if (any(tx$Quantity < 0)) {
    sale.prices <- reduce.on.factor(
      tx[tx$Quantity < 0, ], "Date", function(for.date) data.frame(
        Average=sum(for.date$Cost) / sum(for.date$Quantity),
        Low=min(for.date$Cost / for.date$Quantity),
        High=max(for.date$Cost / for.date$Quantity)))
    sale.prices <- sale.prices[order(sale.prices$Date), ]
  } else {
    sale.prices <- data.frame(
      Date=as.Date(character()), Average=numeric(), Low=numeric(), High=numeric())
  }
  
  list(peak.price=peak.price, purchase.prices=purchase.prices, sale.prices=sale.prices)
}

calc.size.vs.price <- function(tx, symbol, bucket.width=1, carry.down.sales=TRUE) {
  tx <- tx[tx$Symbol == symbol, c("Pct.Drawdown", "Cost", "Price", "Quantity")]
  if (bucket.width != 0)
    tx$Pct.Drawdown <- trunc(tx$Pct.Drawdown / bucket.width) * bucket.width
  costs <- reduce.on.factor(tx, "Pct.Drawdown", function(for.pct.drawdown) {
    data.frame(
      Cost=round(sum(for.pct.drawdown$Cost), CALC_FRAC_DIGITS),
      Quantity=round(sum(for.pct.drawdown$Quantity), QTY_FRAC_DIGITS))
  })
  costs <- costs[order(costs$Pct.Drawdown), ]
  
  if (bucket.width != 0) {
    equal.spaced.buckets <- do.call(seq, c(as.list(range(tx$Pct.Drawdown)), bucket.width))
    equal.spaced.buckets <- equal.spaced.buckets[!(equal.spaced.buckets %in% costs$Pct.Drawdown)]
    if (length(equal.spaced.buckets) > 0) {
      costs <- rbind(costs, data.frame(Pct.Drawdown=equal.spaced.buckets, Cost=0, Quantity=0))
      costs <- costs[order(costs$Pct.Drawdown), ]
    }
  }
  
  # TODO: cumulative cost can be done on unrounded Pct.Drawdown. Line doesn't need to be bucketized
  #  unlike histogram.
  costs$Cum.Cost <- round(cumsum(costs$Cost), CALC_FRAC_DIGITS)
  if (carry.down.sales) {
    # Permit last bucket to be negative, but negative costs for all other buckets must be carried
    #  down.
    costs$Cost <- Reduce(function(acc, add) {
      if (length(acc) > 0 && tail(acc, 1) < 0) {
        add <- add + tail(acc, 1)
        acc <- c(head(acc, -1), 0)
      }
      c(acc, add)
    }, costs$Cost, c())
  }
  
  costs
}

calc.size.vs.time <- function(tx, symbol, price.provider=recent.transaction.price.provider) {
  tx <- tx[tx$Symbol == symbol, c("Date", "Quantity", "Cost", "Price", "Reference.Symbol")]
  tx$Date.Copy <- tx$Date
  costs <- reduce.on.factor(tx, "Date", function(for.date) {
    date <- unique(for.date$Date.Copy)
    data.frame(
      Cost=sum(for.date$Cost),
      Average.Price=price.provider$price(symbol, date, for.date),
      Low.Price=price.provider$low.price(symbol, date, for.date),
      High.Price=price.provider$high.price(symbol, date, for.date),
      Quantity=round(sum(for.date$Quantity), QTY_FRAC_DIGITS))
  })
  available.price.dates <- price.provider$available.dates(symbol)
  available.price.dates <- available.price.dates[
    !(available.price.dates %in% costs$Date) & available.price.dates >= min(costs$Date)]
  if (length(available.price.dates) > 0) {
    costs <- rbind(costs, data.frame(
      Date=available.price.dates,
      Cost=0,
      Average.Price=unlist(lapply(
        available.price.dates, function(date) price.provider$price(symbol, date, NULL))),
      Low.Price=unlist(lapply(
        available.price.dates, function(date) price.provider$low.price(symbol, date, NULL))),
      High.Price=unlist(lapply(
        available.price.dates, function(date) price.provider$high.price(symbol, date, NULL))),
      Quantity=0))
  }
  costs <- costs[order(costs$Date), ]
  
  # Last observation carry forward on NA prices.
  # This should only be needed for interest and dividend payment days since those transactions don't
  #  have a trade price that recent.transaction.price.provider can use and there isn't necessarily a
  #  purchase or sale that must occur on the same day unlike in the case of fee transactions.
  valid.idx <- !is.na(costs$Average.Price)
  costs$Average.Price <- c(NA, costs$Average.Price[valid.idx])[cumsum(valid.idx) + 1]
  costs$Low.Price[is.na(costs$Low.Price)] <- costs$Average.Price[is.na(costs$Low.Price)]
  costs$High.Price[is.na(costs$High.Price)] <- costs$Average.Price[is.na(costs$High.Price)]
  
  costs$Cost <- round(cumsum(costs$Cost), CALC_FRAC_DIGITS)
  costs$Value <- costs$Average.Price * cumsum(costs$Quantity)
  costs$Low.Value <- costs$Low.Price * cumsum(costs$Quantity)
  costs$High.Value <- costs$High.Price * cumsum(costs$Quantity)
  costs
}

calc.portfolio.size.vs.time <- function(
    tx, symbols, price.provider=recent.transaction.price.provider, other.size.vs.time.tables=NULL) {
  if (length(symbols) + length(other.size.vs.time.tables) == 0) {
    return(data.frame(Date=as.Date(character())))
  }
  
  size.vs.time.by.symbol <- c(
    lapply(symbols, function(symbol) calc.size.vs.time(tx, symbol, price.provider)),
    other.size.vs.time.tables)
  dates <- sort(unique(do.call(c, lapply(
    size.vs.time.by.symbol, function(for.symbol) unique(for.symbol$Date)))))
  col.names <- names(size.vs.time.by.symbol[[1]])
  agg.col.names <- col.names[
    !(col.names %in% c("Date", "Average.Price", "Low.Price", "High.Price", "Quantity"))]
  size.vs.time <- lapply(
    agg.col.names,
    # TODO: flip Low.Price and High.Price for negative quantity positions.
    function(col.name) rowSums(do.call(cbind, lapply(
      size.vs.time.by.symbol,
      function(for.symbol) stepfun(for.symbol$Date, c(0, for.symbol[, col.name]))(dates)))))
  size.vs.time <- do.call(data.frame, setNames(size.vs.time, agg.col.names))
  size.vs.time$Date <- dates
  size.vs.time[, c("Average.Price", "Low.Price", "High.Price", "Quantity")] <- NA
  size.vs.time <- size.vs.time[, col.names]
  size.vs.time$Cost <- round(size.vs.time$Cost, CALC_FRAC_DIGITS)
  size.vs.time
}

get.current.dates.by.symbol <- function(symbols, price.provider=recent.transaction.price.provider) {
  setNames(
    lapply(
      symbols, function(symbol) max(as.Date("0001-01-01"), price.provider$available.dates(symbol))),
    symbols)
}

get.current.date <- function(tx, price.provider=recent.transaction.price.provider) {
  # Get the latest date for which we have a price available. Do not solely rely on max(tx$Date)
  #  since it's possible we haven't made any transactions in a long time.
  max(do.call(c, get.current.dates.by.symbol(unique(tx$Symbol), price.provider)))
}

get.current.price <- function(size.vs.time, date=Sys.Date()) {
  # Some options roots may have no transactions or price history whatsoever. In that case return NA.
  if (all(is.na(size.vs.time$Average.Price)))
    NA
  else
    stepfun(size.vs.time$Date, c(NA, size.vs.time$Average.Price))(date)
}

identify.tx.nearby <- function(tx, symbol, current.price) {
  tx.for.symbol <- tx[tx$Symbol == symbol, c("Price", "Quantity", "Pct.Drawdown")]
  tx.for.symbol <- reduce.on.factor(tx.for.symbol, "Price", function(for.price)
    data.frame(
      Quantity=round(sum(for.price$Quantity), QTY_FRAC_DIGITS),
      Pct.Drawdown=unique(for.price$Pct.Drawdown)))
  tx.buys <- tx.for.symbol[tx.for.symbol$Quantity > 0 & tx.for.symbol$Price >= current.price, ]
  tx.buys <- tail(tx.buys[order(tx.buys$Price, decreasing=TRUE), ], 5)
  tx.sells <- tx.for.symbol[tx.for.symbol$Quantity < 0 & tx.for.symbol$Price <= current.price, ]
  tx.sells <- head(tx.sells[order(tx.sells$Price, decreasing=TRUE), ], 5)
  rbind(tx.buys, data.frame(Price=current.price, Quantity=NA, Pct.Drawdown=NA), tx.sells)
}

group.interesting.options <- function(tx, interesting.symbols, is.interesting.option) {
  interesting.options <- interesting.symbols[is.interesting.option]
  interesting.options <- substr(
    interesting.options, 1, nchar(interesting.options) - nchar(" options"))
  interesting.options <- lapply.and.set.names(
    interesting.options, function(options.root) unique(tx$Symbol[
      is.options.symbol(tx$Symbol) & sub("\\s+$", "", substr(tx$Symbol, 1, 6)) == options.root]))
  interesting.options[lapply(interesting.options, length) != 0]
}

get.size.vs.time.tables.with.opt.roots <- function(
    price.provider, size.vs.time.tables, interesting.options, current.date) {
  symbols <- names(size.vs.time.tables)
  # get.interactive.option.payoff.plot needs spot prices, so make sure to cache the prices for
  #  options roots as well even if we have no transactions on the underlying.
  symbols.and.options.roots <- unique(c(symbols, names(interesting.options)))
  # This is not necessarily the same as names(interesting.options) since it excludes overlap with
  #  underlyings on which transactions were made.
  options.roots.only <- symbols.and.options.roots[!(symbols.and.options.roots %in% symbols)]
  c(
    size.vs.time.tables,
    lapply.and.set.names(
      options.roots.only,
      function(symbol) {
        # Create a stub transaction table that will still allow calc.size.vs.time to go to other
        #  price providers beside recent.transaction.price.provider.
        stub.tx <- data.frame(
          Date=c(as.Date("0001-01-01"), current.date),
          Symbol=symbol,
          Quantity=0,
          Cost=0,
          Price=0,
          Reference.Symbol=symbol)
        calc.size.vs.time(stub.tx, symbol, price.provider)
      }
    )
  )
}

get.recent.options <- function(tx, formatted.tx.all, misc.symbols) {
  formatted.tx <- group.options.and.total.in.portfolio.size.snapshot(formatted.tx.all, misc.symbols)
  
  formatted.tx.options <- formatted.tx.all[is.options.symbol(formatted.tx.all$Symbol), ]
  options.roots <- sub("\\s+$", "", substr(formatted.tx.options$Symbol, 1, 6))
  # First, grab all options positions that are currently open.
  open.options.by.root <- split(
    formatted.tx.options$Symbol[formatted.tx.options$Quantity != 0],
    options.roots[formatted.tx.options$Quantity != 0])
  # Then, grab the most recently transacted options until we have at least 8 per root.
  closed.options.by.root <- lapply(
    split(
      formatted.tx.options$Symbol[formatted.tx.options$Quantity == 0],
      options.roots[formatted.tx.options$Quantity == 0]
    ), function(for.root) {
      for.root <- unique(tx[tx$Symbol %in% for.root, c("Symbol", "Date")])
      for.root <- do.call(rbind, lapply(split(for.root, for.root$Symbol), function(for.symbol) {
        data.frame(Symbol=for.symbol$Symbol[1], Date=max(for.symbol$Date), stringsAsFactors=FALSE)
      }))
      recent.dates <- head(
        sort(for.root$Date, partial=seq_len(min(8, nrow(for.root))), decreasing=TRUE), 8)
      for.root <- for.root[for.root$Date %in% recent.dates, ]
      for.root$Symbol[order(for.root$Date, decreasing=TRUE)]
    })
  options.roots <- unique(options.roots)
  do.call(
    c,
    Map(function(open.for.root, closed.for.root) {
      c(open.for.root, head(closed.for.root, 8 - min(length(open.for.root), 8)))
    }, open.options.by.root[options.roots], closed.options.by.root[options.roots]))
}

get.recent.transactions <- function(tx, symbol) {
  # Include dividend and interest transactions, but not fees, cash contra transactions, deposits,
  #  and withdrawals.
  # TODO: support deposits and withdrawals. Either add a new column to the transaction table for
  #  identifying cash contra transactions, or group transactions by date and subtract non-cash
  #  costs with no reference symbol from cash costs with no reference symbol to get day's net
  #  inflow.
  for.symbol <- tx[
    tx$Symbol == symbol & (
      if (symbol == "$") (tx$Reference.Symbol == "$" & tx$Cost < 0)
      else (tx$Reference.Symbol == "" | tx$Cost < 0)
    ),
    c("Date", "Price", "Quantity", "Cost")]
  recent.dates <- head(
    sort(for.symbol$Date, partial=seq_len(min(10, nrow(for.symbol))), decreasing=TRUE), 10)
  for.symbol <- for.symbol[for.symbol$Date %in% recent.dates, ]
  # Preserve intraday transaction order if Date is already sorted ascending or descending.
  # nrow(for.symbol) can be 0 if symbol == "$" if no interest payments have been made yet, so pass
  #  defaults to min and max.
  if (
    !is.unsorted(for.symbol$Date)
    && min(as.Date("9999-12-31"), for.symbol$Date) != max(as.Date("0001-01-01"), for.symbol$Date)
  ) {
    # Sorted in ascending order. Flip it around.
    for.symbol <- for.symbol[rev(seq_len(nrow(for.symbol))), ]
  } else if (is.unsorted(rev(for.symbol$Date))) {
    # Neither sorted in ascending nor descending order. Order it ourselves.
    for.symbol <- for.symbol[order(for.symbol$Date, for.symbol$Price, decreasing=TRUE), ]
  }
  for.symbol
}
