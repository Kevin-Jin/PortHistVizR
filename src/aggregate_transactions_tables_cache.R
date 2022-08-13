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

merge.tables <- function(
    incremental.tx,
    prev.agg.tx.tables,
    interesting.symbols,
    obfuscate.cost,
    symbols,
    price.provider,
    current.date,
    target.net.pct.drawdown,
    target.portfolio.net.cost) {
  # TODO: support building tables in more than one function and overwriting old one using data from
  # old version and other intervening tables built. List item names need to be unique but evaluator
  # can strip suffixes for table name.
  topologically.sorted.table.dependency.graph <- list(
    .is.interesting.option=function(tx, prev, curr.agg.tx.tables) {
      grepl(" options$", interesting.symbols)
    },
    interesting.options=function(tx, prev, curr) {
      curr$interesting.options <- group.interesting.options(
        tx, interesting.symbols, curr$is.interesting.option)
      for (options.root in names(prev$interesting.options)) {
        curr$interesting.options[option.root] <- unique(
          c(
            prev$interesting.options[[options.root]],
            curr$interesting.options[option.root]))
      }
      curr$interesting.options
    },
    interesting.stocks=function(tx, prev, curr) {
      interesting.symbols[!curr$is.interesting.option]
    },
    .cost.by.symbol=function(tx, prev, curr) {
      if (obfuscate.cost) {
        curr$cost.by.symbol <- reduce.on.factor(
          tx, "Symbol", function(tx.for.symbol) data.frame(Cost=sum(tx.for.symbol$Cost)))
      } else {
        curr$cost.by.symbol <- reduce.on.factor(
          tx[tx$Symbol %in% curr$interesting.stocks, ], "Symbol",
          function(tx.for.symbol) data.frame(Cost=sum(tx.for.symbol$Cost)))
      }
      curr$cost.by.symbol
    },
    interesting.stocks..sorted=function(tx, prev, curr) {
      stopifnot(
        is.null(prev)
        || curr$interesting.stocks == prev$interesting.stocks)

      if (obfuscate.cost) {
        curr$cost.by.symbol <- curr$cost.by.symbol[
          curr$cost.by.symbol$Symbol %in% curr$interesting.stocks, ]
      }
      if (!is.null(curr$cost.by.symbol) && nrow(curr$cost.by.symbol) != 0) {
        curr$interesting.stocks <- curr$cost.by.symbol$Symbol[
          order(curr$cost.by.symbol$Cost, decreasing=TRUE)]
      } else {
        curr$interesting.stocks <- c()
      }
      
      curr$interesting.stocks
    },
    .incremental.tx=function(tx, prev, curr) {
      if (obfuscate.cost) {
        tx$Cost <- 100 * tx$Cost / abs(
          curr$cost.by.symbol$Cost[match(tx$Symbol, curr$cost.by.symbol$Symbol)])
        tx$Quantity <- 100 * tx$Quantity / abs(
          curr$cost.by.symbol$Cost[match(tx$Symbol, curr$cost.by.symbol$Symbol)])
      }
      tx
    },
    price.vs.time.tables=function(tx, prev, curr) {
      lapply.and.set.names(
        curr$interesting.stocks, function(symbol) calc.price.vs.time(tx, symbol))
    },
    size.vs.price.tables=function(tx, prev, curr) {
      curr$size.vs.price.tables <- lapply.and.set.names(
        curr$interesting.stocks, function(symbol) calc.size.vs.price(tx, symbol, 0, FALSE))
      curr$size.vs.price.tables <- c(curr$size.vs.price.tables, lapply.and.set.names(
        symbols[!(symbols %in% curr$interesting.stocks)],
        function(symbol) list(peak.price=get.peak.price.from.pct.drawdown(tx))))
      curr$size.vs.price.tables
    },
    size.vs.time.tables..single.symbols=function(tx, prev, curr) {
      # Size vs. time tables for options and miscellaneous stocks are not plotted directly, but are
      #  used as intermediate inputs to portfolio size vs. time tables and the portfolio size
      #  snapshot.
      curr$size.vs.time.tables <- lapply.and.set.names(
        symbols, function(symbol) calc.size.vs.time(tx, symbol, price.provider))
      curr$size.vs.time.tables <- get.size.vs.time.tables.with.opt.roots(
        price.provider, curr$size.vs.time.tables, curr$interesting.options, current.date)
      curr$size.vs.time.tables
    },
    size.vs.time.tables..options=function(tx, prev, curr) {
      for (options.root in names(curr$interesting.options)) {
        options.for.root <- curr$interesting.options[[options.root]]
        calls.for.root <- options.for.root[substr(options.for.root, 13, 13) == "C"]
        puts.for.root <- options.for.root[substr(options.for.root, 13, 13) == "P"]
        curr$size.vs.time.tables[[paste(options.root, "calls")]] <- calc.portfolio.size.vs.time(
          tx, c(), price.provider, curr$size.vs.time.tables[calls.for.root])
        curr$size.vs.time.tables[[paste(options.root, "options")]] <- calc.portfolio.size.vs.time(
          tx, c(), price.provider, curr$size.vs.time.tables[options.for.root])
        curr$size.vs.time.tables[[paste(options.root, "puts")]] <- calc.portfolio.size.vs.time(
          tx, c(), price.provider, curr$size.vs.time.tables[puts.for.root])
      }
      curr$size.vs.time.tables
    },
    miscellaneous.symbols=function(tx, prev, curr) {
      get.miscellaneous.symbols(symbols, curr$interesting.stocks, curr$interesting.options)
    },
    size.vs.time.tables..misc.and.port.and.ex.cash=function(tx, prev, curr) {
      curr$size.vs.time.tables$Miscellaneous <- calc.portfolio.size.vs.time(
        tx, c(), price.provider, curr$size.vs.time.tables[curr$miscellaneous.symbols])
      # "$", interesting.stocks, do.call(c, interesting.options), and miscellaneous.symbols must be
      #  disjoint and the union of them must be equal to symbols. We can then reuse all the already
      #  calculated size vs. time tables to create the portfolio's.
      all.ex.cash.symbols <- curr$interesting.stocks
      if (length(curr$interesting.options) != 0) {
        all.ex.cash.symbols <- c(
          all.ex.cash.symbols, paste(names(curr$interesting.options), "options"))
      }
      if (length(curr$miscellaneous.symbols) != 0) {
        all.ex.cash.symbols <- c(all.ex.cash.symbols, "Miscellaneous")
      }
      all.ex.cash.size.vs.time.tables <- curr$size.vs.time.tables[all.ex.cash.symbols]
      # 401k accounts can have no cash transactions if all contributions are directly invested into
      #  mutual funds and all purchases and sales are exchanges between two mutual funds. There's no
      #  need to distinguish size vs. time between Portfolio Ex-cash and Portfolio in this case.
      cash.sym <- if ("$" %in% symbols) "$" else c()
      stopifnot(
        sort(
          c(
            cash.sym,
            curr$interesting.stocks,
            do.call(c, curr$interesting.options),
            curr$miscellaneous.symbols))
        == sort(symbols))
      if (length(all.ex.cash.size.vs.time.tables) == 0) {
        # Transaction history consists only of cash deposits. No purchases have been made yet.
        curr$size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
          tx, cash.sym, price.provider, all.ex.cash.size.vs.time.tables)
      } else {
        curr$size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
          tx, c(), price.provider, all.ex.cash.size.vs.time.tables)
        if (length(cash.sym) != 0) {
          curr$size.vs.time.tables$`Portfolio Ex-cash` <- curr$size.vs.time.tables$Portfolio
          curr$size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
            tx, cash.sym, price.provider, curr$size.vs.time.tables["Portfolio Ex-cash"])
        }
      }
      curr$size.vs.time.tables
    },
    unit.values=function(tx, prev, curr) {
      lapply.and.set.names(
        names(curr$size.vs.time.tables),
        function(symbol) get.current.price(curr$size.vs.time.tables[[symbol]]))
    },
    unit.costs=function(tx, prev, curr) {
      lapply.and.set.names(symbols, function(symbol) get.unit.cost(tx, symbol))
    },
    .misc.syms.for.port.sum=function(tx, prev, curr) {
      get.misc.syms.for.port.sum(curr$miscellaneous.symbols)
    },
    portfolio.size.snapshot=function(tx, prev, curr) {
      peak.prices <- lapply(
        curr$price.vs.time.tables, function(price.vs.time) price.vs.time$peak.price)
      curr$portfolio.size.snapshot <- calc.portfolio.size.snapshot.with.day.over.day.gain(
        tx, price.provider, current.date, curr$size.vs.time.tables[symbols], peak.prices)
      curr$portfolio.size.snapshot <- group.options.and.total.in.portfolio.size.snapshot(
        curr$portfolio.size.snapshot, curr$misc.syms.for.port.sum)
      curr$portfolio.size.snapshot
    },
    recent.options=function(tx, prev, curr) {
      get.recent.options(tx, curr$portfolio.size.snapshot, curr$misc.syms.for.port.sum)
    },
    recent.transactions=function(tx, prev, curr) {
      recent.symbols <- symbols[!is.options.symbol(symbols) | symbols %in% curr$recent.options]
      curr$recent.transactions <- lapply.and.set.names(
        recent.symbols, function(symbol) get.recent.transactions(tx, symbol))
      curr$recent.transactions
    },
    entry.price.and.quantity=function(tx, prev, curr) {
      # FIXME: NULL tables not supported in lists.
      curr$entry.price.and.quantity <- NULL
      if (!is.null(target.net.pct.drawdown) && !is.null(target.portfolio.net.cost)) {
        # This is needed by plot.static.
        if (!obfuscate.cost) {
          curr$entry.price.and.quantity <- solve.entry.price.and.quantity(
            tx, target.net.pct.drawdown, target.portfolio.net.cost)
        }
      }
      curr$entry.price.and.quantity
    },
    tx.nearby=function(tx, prev, curr) {
      # FIXME: NULL tables not supported in lists.
      curr$tx.nearby <- NULL
      if (!is.null(target.net.pct.drawdown) && !is.null(target.portfolio.net.cost)) {
        # This is needed by plot.static.
        curr$tx.nearby <- lapply.and.set.names(
          curr$interesting.stocks,
          function(symbol) identify.tx.nearby(tx, symbol, curr$unit.values[[symbol]]))
      }
      curr$tx.nearby
    }
  )
  curr.agg.tx.tables <- list()
  for (step.name in names(topologically.sorted.table.dependency.graph)) {
    # Discard "." prefixes in step.name. We use them to denote that the generated table is private
    # and should be deleted once all the steps have finished but before we return a result. There's
    # no need to include the prefix in the table name.
    # Discard suffixes in step.name starting with "..". The text that follows is for making unique
    # names for steps that overwrite tables with the same name, and acts as a comment of sorts to
    # describe the purpose of the step on the table.
    table.name <- sub("^\\.|\\.\\..+$", "", step.name)
    if (table.name == "incremental.tx") {
      incremental.tx <- topologically.sorted.table.dependency.graph[[step.name]](incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables)
    } else {
      curr.agg.tx.tables[[table.name]] <- topologically.sorted.table.dependency.graph[[step.name]](incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables)
    }
  }
  private.names <- names(topologically.sorted.table.dependency.graph)
  private.names <- private.names[grepl("^\\.", private.names)]
  private.names <- unique(
    unlist(lapply(private.names, function(step.name) sub("^\\.|\\.\\..+$", "", step.name))))
  curr.agg.tx.tables <- curr.agg.tx.tables[!(names(curr.agg.tx.tables) %in% private.names)]
  curr.agg.tx.tables
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
  
  prev.agg.tx.tables <- NULL
  incremental.tx <- tx
  tx <- NULL

  # TODO: if intermediate files exist, then load them and apply incremental updates from tx after
  #  the as-of date for the intermediate files. If interesting.symbols change, regenerate
  #  all of miscellaneous and introduce individual symbol tables for new interesting symbols.
  incremental.tx <- coalesce.tx.by.date.symbol.price(incremental.tx)
  incremental.tx <- join.cost(incremental.tx)
  # prev.peak.prices <- lapply.and.set.names(names(prev.agg.tx.tables$size.vs.price.tables), function(symbol) prev.agg.tx.tables$size.vs.price.tables[[symbol]]$peak.price)
  incremental.tx <- join.pct.drawdown(incremental.tx)  # join.pct.drawdown(incremental.tx, prev.peak.prices)
  agg.tx.tables <- merge.tables(
    incremental.tx,
    prev.agg.tx.tables,
    interesting.symbols,
    obfuscate.cost,
    symbols,
    price.provider,
    current.date,
    target.net.pct.drawdown,
    target.portfolio.net.cost)
  agg.tx.tables <- c(
    list(
      version=version,
      current.dates.by.symbol=current.dates.by.symbol,
      current.date=current.date
    ),
    agg.tx.tables
  )
  
  # Keep yesterday's cache as well. That way, we still have a valid cache if we were to rerun this
  #  routine intraday before transactions and prices are finalized at EOD.
  saveRDS(
    c(
      all.prev.agg.tx.tables,
      list(c(agg.tx.tables, list(tx=input.tx, price.provider=price.provider$dump())))),
    cache.path)
  agg.tx.tables
}
