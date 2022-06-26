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

# serialize.cache.structure <- function(struct, path) {
#   if (is.data.frame(struct)) {
#     write(
#       sprintf("%s=[%s]", basename(path), paste(unlist(lapply(struct, class)), collapse=",")),
#       file=file.path(dirname(path), "index"), append=TRUE)
#     write.csv(struct, path, row.names=FALSE)
#   } else if (is.atomic(struct)) {
#     write(
#       sprintf("%s=%s", basename(path), class(struct)),
#       file=file.path(dirname(path), "index"), append=TRUE)
#     write.table(struct, path, row.names=FALSE, col.names=FALSE)
#   } else if (is.list(struct)) {
#     dir.create(path, showWarnings=FALSE)
#     el.names <- names(struct)
#     # Check for conflicts with special names.
#     stopifnot(
#       sort(el.names) != sort(as.character(seq_len(length(struct)))) && !("index" %in% el.names))
#     if (is.null(el.names)) {
#       el.names <- as.character(seq_len(length(struct)))
#       names(struct) <- el.names
#     }
#     file.create(file.path(path, "index"))
#     for (el.name in el.names) {
#       serialize.cache.structure(struct[[el.name]], file.path(path, el.name))
#     }
#   }
# }

get.schema.version <- function() {
  script.source <- paste(
    paste(readLines("src/transaction_aggregators.R"), collapse="\n"),
    paste(readLines("src/aggregate_transactions_tables_cache.R"), collapse="\n"),
    sep="\n")
  digest(script.source, "md5", FALSE, raw=TRUE)
}

merge.tables <- function(incremental.tx, prev.agg.tx.tables) {
  topologically.sorted.table.dependency.graph <- list(
    interesting.options=function(incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables) {
      
    },
    interesting.stocks=function(incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables) {
      
    },
    price.vs.time.tables=function(incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables) {
      lapply.and.set.names(
        curr.agg.tx.tables$interesting.stocks, function(symbol) calc.price.vs.time(incremental.tx, symbol))
    }
  )
  curr.agg.tx.tables <- list()
  for (table.name in names(topologically.sorted.table.dependency.graph)) {
    curr.agg.tx.tables[table.name] <- dispatch[table.name](incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables)
  }
}

create.agg.tx.tables <- function(
    tx, price.provider=recent.transaction.price.provider, interesting.symbols=c(),
    cache.path="cache", obfuscate.cost=FALSE, target.net.pct.drawdown=NULL,
    target.portfolio.net.cost=NULL) {
  symbols <- unique(tx$Symbol)
  current.dates.by.symbol <- get.current.dates.by.symbol(symbols, price.provider)
  current.date <- max(do.call(c, current.dates.by.symbol))
  
  version <- get.schema.version()
  #version <- packBits(as.raw(c(1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,0,1,1,0,0,1,1,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,0,1,1,1,1,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,1,0,1,1,0,1,1,0,1,0,0,0,0)))
  if (file.exists(cache.path)) {
    all.prev.agg.tx.tables <- readRDS(cache.path)
    all.prev.agg.tx.tables <- all.prev.agg.tx.tables[
      unlist(lapply(all.prev.agg.tx.tables, function(prev.agg.tx.tables) (
        prev.agg.tx.tables$version == version
        && max(prev.agg.tx.tables$current.date, max(prev.agg.tx.tables$tx$Date))
        < max(current.date, max(tx$Date))
      )))
    ]
    all.prev.agg.tx.tables <- tail(all.prev.agg.tx.tables, 1)
  } else {
    all.prev.agg.tx.tables <- list()
  }
  
  input.tx <- tx
  if (length(all.prev.agg.tx.tables) != 0) {
    prev.agg.tx.tables <- all.prev.agg.tx.tables[[1]]
    prev.tx <- prev.agg.tx.tables$tx
    our.prev.tx <- tx[tx$Date <= max(prev.tx$Date), ]
    rownames(our.prev.tx) <- NULL
    rownames(prev.tx) <- NULL
    
    prev.price.provider <- prev.agg.tx.tables$price.provider
    our.prev.price.provider <- price.provider$dump()
    # TODO: make sure max(prev.tx$Date) and max(prev.price.provider$Date) are consistent.
    our.prev.price.provider <- our.prev.price.provider[
      our.prev.price.provider$Date <= max(prev.price.provider$Date), ]
    rownames(our.prev.price.provider) <- NULL
    rownames(prev.price.provider) <- NULL
    
    if (
      identical(prev.tx, our.prev.tx) && identical(prev.price.provider, our.prev.price.provider)
    ) {
      incremental.tx <- tx[tx$Date > max(prev.tx$Date), ]
    } else {
      prev.agg.tx.tables <- NULL
      incremental.tx <- tx
    }
  } else {
    prev.agg.tx.tables <- NULL
    incremental.tx <- tx
  }
  
  # TODO: if intermediate files exist, then load them and apply incremental updates from tx after
  #  the as-of date for the intermediate files. If interesting.symbols change, regenerate
  #  all of miscellaneous and introduce individual symbol tables for new interesting symbols.
  
  is.interesting.option <- grepl(" options$", interesting.symbols)
  interesting.stocks <- interesting.symbols[!is.interesting.option]
  interesting.options <- group.interesting.options(tx, interesting.symbols, is.interesting.option)
  
  tx <- coalesce.tx.by.date.symbol.price(tx)
  tx <- join.cost(tx)
  tx <- join.pct.drawdown(tx)
  
  # incremental.tx <- coalesce.tx.by.date.symbol.price(incremental.tx)
  # incremental.tx <- join.cost(incremental.tx)
  # prev.peak.prices <- lapply.and.set.names(names(prev.agg.tx.tables$size.vs.price.tables), function(symbol) prev.agg.tx.tables$size.vs.price.tables[[symbol]]$peak.price)
  # incremental.tx <- join.pct.drawdown(incremental.tx, prev.peak.prices)
  # TODO: use prev.agg.tx.tables$portfolio.size.snapshot and incremental.tx.
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
  
  price.vs.time.tables <- lapply.and.set.names(
    interesting.stocks, function(symbol) calc.price.vs.time(tx, symbol))
  size.vs.price.tables <- lapply.and.set.names(
    interesting.stocks, function(symbol) calc.size.vs.price(tx, symbol, 0, FALSE))
  size.vs.price.tables <- c(size.vs.price.tables, lapply.and.set.names(
    symbols[!(symbols %in% interesting.stocks)],
    function(symbol) list(peak.price=get.peak.price.from.pct.drawdown(tx))))
  
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
  
  agg.tx.tables <- list(
    version=version,
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
  
  #stop()
  
  # Keep yesterday's cache as well. That way, we still have a valid cache if we were to rerun this
  #  routine intraday before transactions and prices are finalized at EOD.
  saveRDS(
    c(
      all.prev.agg.tx.tables,
      list(c(agg.tx.tables, list(tx=input.tx, price.provider=price.provider$dump())))),
    cache.path)
  agg.tx.tables
}
