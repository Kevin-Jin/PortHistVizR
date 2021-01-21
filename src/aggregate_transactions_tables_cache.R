get.miscellaneous.symbols <- function(symbols, interesting.stocks, interesting.options) {
  interesting.symbols.expanded <- c("$", interesting.stocks, do.call(c, interesting.options))
  symbols[!(symbols %in% interesting.symbols.expanded)]
}

get.misc.syms.for.port.sum <- function(miscellaneous.symbols) {
  # For now, always treat options as interesting symbols in the portfolio summary to avoid three
  #  level child rows.
  miscellaneous.symbols[!is.options.symbol(miscellaneous.symbols)]
}

get.unit.cost <- function(tx, symbol) {
  sum(tx$Cost[tx$Symbol == symbol]) / sum(tx$Quantity[tx$Symbol == symbol])
}

create.agg.tx.tables <- function(
    tx, price.provider=recent.transaction.price.provider, interesting.symbols=c(),
    obfuscate.cost=FALSE, target.net.pct.drawdown=NULL, target.portfolio.net.cost=NULL) {
  is.interesting.option <- grepl(" options$", interesting.symbols)
  interesting.stocks <- interesting.symbols[!is.interesting.option]
  interesting.options <- group.interesting.options(tx, interesting.symbols, is.interesting.option)
  symbols <- unique(tx$Symbol)
  
  tx <- coalesce.tx.by.date.symbol.price(tx)
  tx <- join.cost(tx)
  tx <- join.pct.drawdown(tx)
  if (obfuscate.cost) {
    tx.by.symbol <- reduce.on.factor(
      tx, "Symbol", function(tx.for.symbol) data.frame(Cost=sum(tx.for.symbol$Cost)))
    tx$Cost <- 100 * tx$Cost / abs(tx.by.symbol$Cost[match(tx$Symbol, tx.by.symbol$Symbol)])
    tx$Quantity <- 100 * tx$Quantity / abs(tx.by.symbol$Cost[match(tx$Symbol, tx.by.symbol$Symbol)])
    tx.by.symbol <- tx.by.symbol[tx.by.symbol$Symbol %in% interesting.stocks, ]
  } else {
    tx.by.symbol <- reduce.on.factor(
      tx[tx$Symbol %in% interesting.stocks, ], "Symbol",
      function(tx.for.symbol) data.frame(Cost=sum(tx.for.symbol$Cost)))
  }
  if (!is.null(tx.by.symbol) && nrow(tx.by.symbol) != 0) {
    interesting.stocks <- tx.by.symbol$Symbol[order(tx.by.symbol$Cost, decreasing=TRUE)]
  } else {
    interesting.stocks <- c()
  }
  
  current.dates.by.symbol <- get.current.dates.by.symbol(symbols, price.provider)
  current.date <- max(do.call(c, current.dates.by.symbol))
  
  price.vs.time.tables <- lapply.and.set.names(
    interesting.stocks, function(symbol) calc.price.vs.time(tx, symbol))
  size.vs.price.tables <- lapply.and.set.names(
    interesting.stocks, function(symbol) calc.size.vs.price(tx, symbol, 0, FALSE))
  
  # Size vs. time tables for options and miscellaneous stocks are not plotted directly, but are used
  #  as intermediate inputs to portfolio size vs. time tables and the portfolio size snapshot.
  size.vs.time.tables <- lapply.and.set.names(
    symbols, function(symbol) calc.size.vs.time(tx, symbol, price.provider))
  size.vs.time.tables <- get.size.vs.time.tables.with.opt.roots(
    price.provider, size.vs.time.tables, interesting.options, current.date)
  unit.values <- lapply.and.set.names(
    names(size.vs.time.tables),
    function(symbol) get.current.price(size.vs.time.tables[[symbol]]))
  for (options.root in names(interesting.options)) {
    options.for.root <- interesting.options[[options.root]]
    calls.for.root <- options.for.root[substr(options.for.root, 13, 13) == "C"]
    puts.for.root <- options.for.root[substr(options.for.root, 13, 13) == "P"]
    size.vs.time.tables[[paste(options.root, "calls")]] <- calc.portfolio.size.vs.time(
      tx, c(), price.provider, size.vs.time.tables[calls.for.root])
    size.vs.time.tables[[paste(options.root, "options")]] <- calc.portfolio.size.vs.time(
      tx, c(), price.provider, size.vs.time.tables[options.for.root])
    size.vs.time.tables[[paste(options.root, "puts")]] <- calc.portfolio.size.vs.time(
      tx, c(), price.provider, size.vs.time.tables[puts.for.root])
  }
  miscellaneous.symbols <- get.miscellaneous.symbols(
    symbols, interesting.stocks, interesting.options)
  size.vs.time.tables$Miscellaneous <- calc.portfolio.size.vs.time(
    tx, c(), price.provider, size.vs.time.tables[miscellaneous.symbols])
  # "$", interesting.stocks, do.call(c, interesting.options), and miscellaneous.symbols must be
  #  disjoint and the union of them must be equal to symbols. We can then reuse all the already
  #  calculated size vs. time tables to create the portfolio's.
  all.ex.cash.symbols <- interesting.stocks
  if (length(interesting.options) != 0) {
    all.ex.cash.symbols <- c(all.ex.cash.symbols, paste(names(interesting.options), "options"))
  }
  if (length(miscellaneous.symbols) != 0) {
    all.ex.cash.symbols <- c(all.ex.cash.symbols, "Miscellaneous")
  }
  all.ex.cash.size.vs.time.tables <- size.vs.time.tables[all.ex.cash.symbols]
  # 401k accounts can have no cash transactions if all contributions are directly invested into
  #  mutual funds and all purchases and sales are exchanges between two mutual funds. There's no
  #  need to distinguish size vs. time between Portfolio Ex-cash and Portfolio in this case.
  cash.sym <- if ("$" %in% symbols) "$" else c()
  stopifnot(
    sort(c(cash.sym, interesting.stocks, do.call(c, interesting.options), miscellaneous.symbols))
    == sort(symbols))
  if (length(all.ex.cash.size.vs.time.tables) == 0) {
    # Transaction history consists only of cash deposits. No purchases have been made yet.
    size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
      tx, cash.sym, price.provider, all.ex.cash.size.vs.time.tables)
  } else {
    size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
      tx, c(), price.provider, all.ex.cash.size.vs.time.tables)
    if (length(cash.sym) != 0) {
      size.vs.time.tables$`Portfolio Ex-cash` <- size.vs.time.tables$Portfolio
      size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
        tx, cash.sym, price.provider, size.vs.time.tables["Portfolio Ex-cash"])
    }
  }
  
  unit.costs <- lapply.and.set.names(symbols, function(symbol) get.unit.cost(tx, symbol))
  
  peak.prices <- lapply(price.vs.time.tables, function(price.vs.time) price.vs.time$peak.price)
  portfolio.size.snapshot <- calc.portfolio.size.snapshot.with.day.over.day.gain(
    tx, price.provider, current.date, size.vs.time.tables[symbols], peak.prices)
  misc.syms.for.port.sum <- get.misc.syms.for.port.sum(miscellaneous.symbols)
  portfolio.size.snapshot <- group.options.and.total.in.portfolio.size.snapshot(
    portfolio.size.snapshot, misc.syms.for.port.sum)
  recent.options <- get.recent.options(tx, portfolio.size.snapshot, misc.syms.for.port.sum)
  recent.symbols <- symbols[!is.options.symbol(symbols) | symbols %in% recent.options]
  recent.transactions <- lapply.and.set.names(
    recent.symbols, function(symbol) get.recent.transactions(tx, symbol))
  
  entry.price.and.quantity <- NULL
  tx.nearby <- NULL
  if (!is.null(target.net.pct.drawdown) && !is.null(target.portfolio.net.cost)) {
    # These are needed by plot.static.
    if (!obfuscate.cost) {
      entry.price.and.quantity <- solve.entry.price.and.quantity(
        tx, target.net.pct.drawdown, target.portfolio.net.cost)
    }
    tx.nearby <- lapply.and.set.names(
      interesting.stocks, function(symbol) identify.tx.nearby(tx, symbol, unit.values[[symbol]]))
  }
  
  list(
    interesting.options=interesting.options,
    interesting.stocks=interesting.stocks,
    miscellaneous.symbols=miscellaneous.symbols,
    current.dates.by.symbol=current.dates.by.symbol,
    current.date=current.date,
    price.vs.time.tables=price.vs.time.tables,
    size.vs.price.tables=size.vs.price.tables,
    size.vs.time.tables=size.vs.time.tables,
    unit.values=unit.values,
    unit.costs=unit.costs,
    portfolio.size.snapshot=portfolio.size.snapshot,
    recent.options=recent.options,
    recent.transactions=recent.transactions,
    entry.price.and.quantity=entry.price.and.quantity,
    tx.nearby=tx.nearby)
}
