reduce.on.factor <- function(frame, factor, func) {
  frame.by.factor <- lapply(
    split(frame[, names(frame) != factor, drop=FALSE], frame[, factor]), func)
  frame.by.factor <- lapply(
    names(frame.by.factor), function(for.factor) cbind(
      setNames(data.frame(for.factor, stringsAsFactors=FALSE), factor),
      frame.by.factor[[for.factor]]))
  frame.by.factor <- do.call(rbind, frame.by.factor)

  setAs("character", "Date", function(from) as.Date(from))
  frame.by.factor[, factor] <- as(frame.by.factor[, factor], class(frame[, factor]))

  row.names(frame.by.factor) <- c()
  frame.by.factor
}

coalesce.tx.by.date.symbol.price <- function(tx) {
  reduce.on.factor(tx, "Date", function(tx.for.date) {
    reduce.on.factor(tx.for.date, "Symbol", function(tx.for.symbol) {
      tx.by.price <- lapply(split(tx.for.symbol$Quantity, tx.for.symbol$Price), sum)
      data.frame(
        Quantity=round(unname(unlist(tx.by.price)), QTY_FRAC_DIGITS),
        Price=as.numeric(names(tx.by.price)))
    })
  })
}

join.cost <- function(tx) {
  tx$Cost <- tx$Price * tx$Quantity
  tx
}

join.pct.drawdown <- function(tx) {
  reduce.on.factor(tx, "Symbol", function(tx.for.symbol) {
    peak.price <- max(tx.for.symbol$Price)
    tx.for.symbol$Pct.Drawdown <- (1 - tx.for.symbol$Price / peak.price) * 100
    tx.for.symbol
  })
}

filter.tx <- function(tx, symbols, from.date, to.date) {
  tx <- tx[
    (is.null(symbols) | tx$Symbol %in% symbols | is.options.symbol(tx$Symbol))
    & tx$Date >= from.date & tx$Date <= to.date, ]
  tx <- tx[order(tx$Date), ]
  tx
}

aggregate.purchases.and.sales <- function(tx, price.provider=recent.transaction.price.provider) {
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
  
  agg.by.symbol <- reduce.on.factor(tx, "Symbol", avg.price)
  agg.by.symbol <- agg.by.symbol[order(agg.by.symbol$Cost), ]
  row.names(agg.by.symbol) <- c()
  
  agg.by.symbol$Unit.Value <- unlist(lapply(
    agg.by.symbol$Symbol,
    function(symbol) get.current.price(tx, symbol, price.provider)))
  agg.by.symbol$Value <- agg.by.symbol$Quantity * agg.by.symbol$Unit.Value
  agg.by.symbol$Value.Pct.Drawdown <- (
    1 - agg.by.symbol$Unit.Value / agg.by.symbol$Peak.Price) * 100
  
  aggregate.on.portfolio <- function(group.name, agg.over) {
    # TODO: calculate portfolio Pct.Drawdown.
    agg.portfolio <- cbind(
      Symbol=group.name,
      as.data.frame(t(colSums(agg.over[, c("Cost", "Full.Rebound.Gain", "Value")]))))
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
  tx <- tx[tx$Symbol == symbol, c("Date", "Quantity", "Cost", "Price")]
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
