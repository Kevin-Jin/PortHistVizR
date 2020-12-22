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

filter.tx <- function(tx, symbols, from.date, to.date) {
  tx <- tx[
    (is.null(symbols) | tx$Symbol %in% symbols | is.options.symbol(tx$Symbol))
    & tx$Date >= from.date & tx$Date <= to.date, ]
  tx
}

calc.portfolio.size.snapshot <- function(
    tx, price.provider=recent.transaction.price.provider, date=max(tx$Date)) {
  avg.price <- function(tx.for.factor) {
    cost <- sum(tx.for.factor$Cost)
    quantity <- round(sum(tx.for.factor$Quantity), QTY_FRAC_DIGITS)
    peak.price <- sum(tx.for.factor$Price) / sum(1 - tx.for.factor$Pct.Drawdown / 100)
    unit.cost <- cost / quantity
    data.frame(
      Quantity=quantity,
      Unit.Cost=unit.cost,
      Cost=cost,
      Cost.Pct.Drawdown=(1 - unit.cost / peak.price) * 100,
      Peak.Price=peak.price,
      Full.Rebound.Gain=quantity * peak.price - cost)
  }
  
  tx <- tx[tx$Date <= date, ]
  agg.by.symbol <- reduce.on.factor(tx, "Symbol", avg.price)
  agg.by.symbol <- agg.by.symbol[order(agg.by.symbol$Cost), ]
  row.names(agg.by.symbol) <- c()
  
  agg.by.ref.sym <- reduce.on.factor(
    tx[tx$Reference.Symbol != "", ], "Reference.Symbol", function(tx.for.ref.sym) {
      data.frame(Dividends.Minus.Fees=sum(tx.for.ref.sym$Quantity * tx.for.ref.sym$Price))
    })
  if (is.null(agg.by.ref.sym)) {
    agg.by.ref.sym <- data.frame(
      matrix(ncol=2, nrow=0, dimnames=list(NULL, c("Reference.Symbol", "Dividends.Minus.Fees"))))
  }
  agg.by.ref.sym <- agg.by.ref.sym[
    match(agg.by.symbol$Symbol, agg.by.ref.sym$Reference.Symbol),
    colnames(agg.by.ref.sym) != "Reference.Symbol", drop=FALSE]
  agg.by.ref.sym[is.na(agg.by.ref.sym)] <- 0
  agg.by.symbol <- cbind(agg.by.symbol, agg.by.ref.sym)
  
  agg.by.symbol$Unit.Value <- unlist(lapply(
    agg.by.symbol$Symbol,
    function(symbol) get.current.price(tx, symbol, price.provider, date)))
  agg.by.symbol$Value <- agg.by.symbol$Quantity * agg.by.symbol$Unit.Value
  agg.by.symbol$Value.Pct.Drawdown <- (
    1 - agg.by.symbol$Unit.Value / agg.by.symbol$Peak.Price) * 100
  
  aggregate.on.portfolio <- function(group.name, agg.over) {
    # TODO: calculate portfolio Pct.Drawdown.
    agg.portfolio <- cbind(
      Symbol=group.name,
      as.data.frame(t(colSums(
        agg.over[, c("Cost", "Full.Rebound.Gain", "Value", "Dividends.Minus.Fees")]))))
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
  
  agg.all <- agg.by.symbol[!is.options.symbol(agg.by.symbol$Symbol), ]
  agg.all <- rbind(agg.all, agg.options)
  agg.all <- rbind(agg.all, aggregate.on.portfolio("Portfolio", agg.all))
  agg.all$Gain <- agg.all$Value - agg.all$Cost
  agg.all
}

calc.portfolio.size.snapshot.with.day.over.day.gain <- function(
    tx, price.provider=recent.transaction.price.provider) {
  agg.all <- calc.portfolio.size.snapshot(tx, price.provider)
  if (any(tx$Date <= max(tx$Date) - 1)) {
    agg.all.yday <- calc.portfolio.size.snapshot(tx, price.provider, max(tx$Date) - 1)
  } else {
    agg.all.yday <- data.frame(matrix(ncol=1, nrow=0, dimnames=list(NULL, c("Symbol"))))
  }
  
  yday.idx <- match(agg.all$Symbol, agg.all.yday$Symbol)
  yday.gain <- ifelse(is.na(yday.idx), 0, agg.all.yday$Gain[yday.idx])
  cbind(agg.all, Day.Over.Day.Gain=agg.all$Gain - yday.gain)
}

solve.entry.price.and.quantity <- function(tx, target.net.pct.drawdown, target.portfolio.net.cost) {
  # TODO: pass in target percentage allocation of each asset and we can perform an optimization on
  #  portfolio's net % drawdown. Right now, we can only perform an optimization on each symbol's net
  #  % drawdown.
  current.portfolio.net.cost <- sum(tx$Cost)
  reduce.on.factor(tx, "Symbol", function(tx.for.symbol) {
    peak.price <- sum(tx.for.symbol$Price) / sum(1 - tx.for.symbol$Pct.Drawdown / 100)
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
  peak.price <- sum(tx$Price) / sum(1 - tx$Pct.Drawdown / 100)
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
      Cost=sum(for.pct.drawdown$Cost), Quantity=sum(for.pct.drawdown$Quantity))
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
  costs$Cum.Cost <- cumsum(costs$Cost)
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
  
  costs$Cost <- cumsum(costs$Cost)
  costs$Value <- costs$Average.Price * cumsum(costs$Quantity)
  costs$Low.Value <- costs$Low.Price * cumsum(costs$Quantity)
  costs$High.Value <- costs$High.Price * cumsum(costs$Quantity)
  costs
}

calc.portfolio.size.vs.time <- function(
    tx, symbols, price.provider=recent.transaction.price.provider) {
  size.vs.time.by.symbol <- lapply(
    symbols, function(symbol) calc.size.vs.time(tx, symbol, price.provider))
  dates <- sort(unique(do.call(c, lapply(
    size.vs.time.by.symbol, function(for.symbol) unique(for.symbol$Date)))))
  col.names <- names(size.vs.time.by.symbol[[1]])
  size.vs.time <- lapply(
    col.names,
    function(col.name) rowSums(do.call(cbind, lapply(
      size.vs.time.by.symbol,
      function(for.symbol) stepfun(for.symbol$Date, c(0, for.symbol[, col.name]))(dates)))))
  size.vs.time <- do.call(data.frame, setNames(size.vs.time, col.names))
  size.vs.time$Date <- dates
  size.vs.time[, c("Average.Price", "Low.Price", "High.Price", "Quantity")] <- NA
  size.vs.time
}

get.current.price <- function(
    tx, symbol, price.provider=recent.transaction.price.provider, date=Sys.Date()) {
  size.vs.time <- calc.size.vs.time(tx, symbol, price.provider)
  stepfun(size.vs.time$Date, c(NA, size.vs.time$Average.Price))(date)
}

identify.tx.nearby <- function(tx, symbol, price.provider=recent.transaction.price.provider) {
  current.price <- get.current.price(tx, symbol, price.provider)
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
