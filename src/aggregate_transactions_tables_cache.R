get.miscellaneous.symbols <- function(symbols, interesting.stocks, interesting.options) {
  interesting.symbols.expanded <- c("$", interesting.stocks, do.call(c, interesting.options))
  symbols[!(symbols %in% interesting.symbols.expanded)]
}

get.misc.syms.for.port.sum <- function(miscellaneous.symbols) {
  # For now, always treat options as interesting symbols in the portfolio summary to avoid three
  #  level child rows.
  miscellaneous.symbols[!is.options.symbol(miscellaneous.symbols)]
}

is.single.symbol <- function(symbol) {
  !grepl("( (options|calls|puts)|Miscellaneous|Portfolio|Portfolio Ex-cash)$", symbol)
}

get.schema.version <- function() {
  script.source <- paste(
    paste(readLines("src/transaction_aggregators.R"), collapse="\n"),
    paste(readLines("src/aggregate_transactions_tables_cache.R"), collapse="\n"),
    sep="\n")
  digest(script.source, "md5", FALSE, raw=TRUE)
}

cbind.or.empty <- function(...) {
  columns <- list(...)
  if (any(unlist(lapply(columns, NROW)) == 0)) {
    columns <- lapply(columns, head, 0)
  }
  do.call(cbind, columns)
}

data.frame.or.empty <- function(...) {
  columns <- list(...)
  if (any(unlist(lapply(columns, NROW)) == 0)) {
    columns <- lapply(columns, head, 0)
  }
  do.call(data.frame, columns)
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
  topologically.sorted.table.dependency.graph <- list(
    .is.interesting.option=function(tx, prev, curr) {
      grepl(" options$", interesting.symbols)
    },
    interesting.stocks=function(tx, prev, curr) {
      interesting.symbols[!curr$is.interesting.option]
    },
    interesting.options=function(tx, prev, curr) {
      curr$interesting.options <- group.interesting.options(
        tx, interesting.symbols, curr$is.interesting.option)
      for (options.root in names(prev$interesting.options)) {
        curr$interesting.options[[options.root]] <- unique(
          c(
            prev$interesting.options[[options.root]],
            curr$interesting.options[[options.root]]))
      }
      curr$interesting.options
    },
    .cost.by.symbol=function(tx, prev, curr) {
      prev.symbols <- names(prev$size.vs.time.tables)
      if (obfuscate.cost) {
        # Note: this won't give exactly the same thing as a clean run because size.vs.time.tables
        # can include options underlyings that we never had positions in, but dependents don't care
        # if we produce more symbols than needed here.
        prev.symbols <- prev.symbols[is.single.symbol(prev.symbols)]
      } else {
        prev.symbols <- prev.symbols[prev.symbols %in% curr$interesting.stocks]
        tx <- tx[tx$Symbol %in% curr$interesting.stocks, ]
      }
      prev$cost.by.symbol <- do.call(
        rbind,
        lapply(
          prev.symbols,
          function(symbol) data.frame(
            Symbol=symbol, Cost=tail(prev$size.vs.time.tables[[symbol]]$Cost, 1))))
      curr$cost.by.symbol <- rbind(
        prev$cost.by.symbol[, c("Symbol", "Cost")], tx[, c("Symbol", "Cost")])
      # TODO: ROUND
      curr$cost.by.symbol <- reduce.on.factor(
        curr$cost.by.symbol, "Symbol", function(for.symbol) data.frame(Cost=sum(for.symbol$Cost)))
      curr$cost.by.symbol
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
    interesting.stocks..sorted=function(tx, prev, curr) {
      if (obfuscate.cost) {
        curr$cost.by.symbol <- curr$cost.by.symbol[
          curr$cost.by.symbol$Symbol %in% curr$interesting.stocks, ]
      }
      if (NROW(curr$cost.by.symbol) != 0) {
        curr$interesting.stocks <- curr$cost.by.symbol$Symbol[
          order(curr$cost.by.symbol$Cost, decreasing=TRUE)]
      } else {
        curr$interesting.stocks <- c()
      }
      curr$interesting.stocks
    },
    price.vs.time.tables=function(tx, prev, curr) {
      if (!is.null(prev) && !all(curr$interesting.stocks %in% prev$interesting.stocks)) {
        # TODO: run calc.price.vs.time on full.tx for newly added symbols.
        stop("Cache cannot be used when symbols are added to interesting.stocks")
      }
      curr$price.vs.time.tables <- lapply.and.set.names(
        curr$interesting.stocks, function(symbol) calc.price.vs.time(tx, symbol))
      # Store peak prices for all symbols, including miscellaneous ones, so that we can
      #  incrementally calculate Pct.Drawdown in the future.
      curr$price.vs.time.tables <- c(curr$price.vs.time.tables, lapply.and.set.names(
        symbols[!(symbols %in% curr$interesting.stocks)],
        function(symbol) list(
          peak.price=get.peak.price.from.pct.drawdown(tx[tx$Symbol == symbol, ]))))
      for (symbol in curr$interesting.stocks) {
        prev.for.symbol <- prev$price.vs.time.tables[[symbol]]
        curr.for.symbol <- curr$price.vs.time.tables[[symbol]]
        curr.for.symbol$purchase.prices <- rbind(
          prev.for.symbol$purchase.prices, curr.for.symbol$purchase.prices)
        curr.for.symbol$sale.prices <- rbind(
          prev.for.symbol$sale.prices, curr.for.symbol$sale.prices)
        curr$price.vs.time.tables[[symbol]] <- curr.for.symbol
      }
      for (symbol in symbols) {
        prev.for.symbol <- prev$price.vs.time.tables[[symbol]]
        curr.for.symbol <- curr$price.vs.time.tables[[symbol]]
        curr.for.symbol$peak.price <- max(
          curr.for.symbol$peak.price, prev.for.symbol$peak.price, na.rm=TRUE)
        curr$price.vs.time.tables[[symbol]] <- curr.for.symbol
      }
      curr$price.vs.time.tables
    },
    size.vs.price.tables=function(tx, prev, curr) {
      if (!is.null(prev) && !all(curr$interesting.stocks %in% prev$interesting.stocks)) {
        # TODO: run calc.price.vs.time on full.tx for newly added symbols.
        stop("Cache cannot be used when symbols are added to interesting.stocks")
      }
      curr$size.vs.price.tables <- lapply.and.set.names(
        curr$interesting.stocks, function(symbol) {
          for.symbol <- rbind(
            prev$size.vs.price.tables[[symbol]][, c("Pct.Drawdown", "Cost", "Quantity")],
            tx[tx$Symbol == symbol, c("Pct.Drawdown", "Cost", "Quantity")])
          for.symbol <- cbind.or.empty(Symbol=symbol, for.symbol)
          calc.size.vs.price(for.symbol, symbol, 0, FALSE)
        })
      curr$size.vs.price.tables
    },
    size.vs.time.tables..single.symbols=function(tx, prev, curr) {
      # Size vs. time tables for options and miscellaneous stocks are not plotted directly, but are
      #  used as intermediate inputs to portfolio size vs. time tables and the portfolio size
      #  snapshot.
      # TODO: have all earlier size.vs.time.table steps skip removing the seed.for.symbol and
      #  prepending prev$size.vs.time.tables. Once all portfolio size.vs.time.tables are generated,
      #  then remove the seed.for.symbol and prepend prev$size.vs.time.tables.
      curr$size.vs.time.tables <- lapply.and.set.names(
        symbols, function(symbol) {
          seed.for.symbol <- cbind.or.empty(
            tail(prev$size.vs.time.tables[[symbol]][, c("Date", "Cost")], 1),
            Quantity=sum(prev$size.vs.time.tables[[symbol]]$Quantity),
            Price=0,
            Reference.Symbol="")
          for.symbol <- rbind(
            seed.for.symbol,
            tx[tx$Symbol == symbol, c("Date", "Cost", "Quantity", "Price", "Reference.Symbol")])
          for.symbol <- cbind.or.empty(Symbol=symbol, for.symbol)
          for.symbol <- calc.size.vs.time(for.symbol, symbol, price.provider)
          if (NROW(seed.for.symbol) != 0) {
            stopifnot(
              all(
                seed.for.symbol[, c("Date", "Cost", "Quantity")]
                == head(for.symbol[, c("Date", "Cost", "Quantity")], 1)))
            for.symbol <- rbind(prev$size.vs.time.tables[[symbol]], tail(for.symbol, -1))
          }
          for.symbol
        })
      curr$size.vs.time.tables <- get.size.vs.time.tables.with.opt.roots(
        price.provider, curr$size.vs.time.tables, curr$interesting.options, current.date)
      curr$size.vs.time.tables
    },
    unit.values=function(tx, prev, curr) {
      lapply.and.set.names(
        names(curr$size.vs.time.tables),
        function(symbol) get.current.price(curr$size.vs.time.tables[[symbol]]))
    },
    size.vs.time.tables..options=function(tx, prev, curr) {
      # TODO: reuse prev$size.vs.time.tables[["___ calls|options|puts"]] for older dates.
      for (options.root in names(curr$interesting.options)) {
        options.for.root <- curr$interesting.options[[options.root]]
        calls.for.root <- options.for.root[substr(options.for.root, 13, 13) == "C"]
        puts.for.root <- options.for.root[substr(options.for.root, 13, 13) == "P"]
        curr$size.vs.time.tables[[paste(options.root, "calls")]] <- calc.portfolio.size.vs.time(
          NULL, c(), price.provider, curr$size.vs.time.tables[calls.for.root])
        curr$size.vs.time.tables[[paste(options.root, "options")]] <- calc.portfolio.size.vs.time(
          NULL, c(), price.provider, curr$size.vs.time.tables[options.for.root])
        curr$size.vs.time.tables[[paste(options.root, "puts")]] <- calc.portfolio.size.vs.time(
          NULL, c(), price.provider, curr$size.vs.time.tables[puts.for.root])
      }
      curr$size.vs.time.tables
    },
    miscellaneous.symbols=function(tx, prev, curr) {
      get.miscellaneous.symbols(symbols, curr$interesting.stocks, curr$interesting.options)
    },
    size.vs.time.tables..misc.and.port.and.ex.cash=function(tx, prev, curr) {
      # TODO: reuse prev$size.vs.time.tables$Miscellaneous for older dates. We have to be careful if
      #  interesting.stocks changes.
      curr$size.vs.time.tables$Miscellaneous <- calc.portfolio.size.vs.time(
        NULL, c(), price.provider, curr$size.vs.time.tables[curr$miscellaneous.symbols])
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
        # TODO: reuse prev$size.vs.time.tables$Portfolio for older dates.
        curr$size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
          NULL, c(), price.provider, curr$size.vs.time.tables[cash.sym])
      } else {
        # TODO: reuse prev$size.vs.time.tables$`Portfolio Ex-cash` for older dates.
        curr$size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
          NULL, c(), price.provider, all.ex.cash.size.vs.time.tables)
        if (length(cash.sym) != 0) {
          curr$size.vs.time.tables$`Portfolio Ex-cash` <- curr$size.vs.time.tables$Portfolio
          # TODO: reuse prev$size.vs.time.tables$Portfolio for older dates.
          curr$size.vs.time.tables$Portfolio <- calc.portfolio.size.vs.time(
            NULL, c(), price.provider, curr$size.vs.time.tables[c("Portfolio Ex-cash", cash.sym)])
        }
      }
      curr$size.vs.time.tables
    },
    unit.costs=function(tx, prev, curr) {
      lapply.and.set.names(symbols, function(symbol) {
        for.symbol <- curr$size.vs.time.tables[[symbol]]
        tail(for.symbol$Cost, 1) / sum(for.symbol$Quantity)
      })
    },
    .misc.syms.for.port.sum=function(tx, prev, curr) {
      get.misc.syms.for.port.sum(curr$miscellaneous.symbols)
    },
    portfolio.size.snapshot=function(tx, prev, curr) {
      peak.prices <- lapply(
        curr$price.vs.time.tables, function(price.vs.time) price.vs.time$peak.price)
      prev.divs.and.fees <- prev$portfolio.size.snapshot[, c("Symbol", "Dividends", "Fees")]
      if (!is.null(prev.divs.and.fees)) {
        prev.divs.and.fees$Price <- 1
        prev.divs.and.fees <- prev.divs.and.fees[is.single.symbol(prev.divs.and.fees$Symbol), ]
        names(prev.divs.and.fees)[names(prev.divs.and.fees) == "Symbol"] <- "Reference.Symbol"
        prev.divs <- prev.divs.and.fees[
          prev.divs.and.fees$Dividends != 0, c("Reference.Symbol", "Dividends", "Price")]
        names(prev.divs)[names(prev.divs) == "Dividends"] <- "Quantity"
        prev.fees <- prev.divs.and.fees[
          prev.divs.and.fees$Fees != 0, c("Reference.Symbol", "Fees", "Price")]
        names(prev.fees)[names(prev.fees) == "Fees"] <- "Quantity"
        divs.and.fees <- rbind(prev.divs, prev.fees)
        divs.and.fees <- cbind.or.empty(Date=current.date, divs.and.fees)
      } else {
        divs.and.fees <- NULL
      }
      # Make sure we pass in dates for forward-dated transactions (e.g. dividends) and some date
      #  before current.date, if any.
      divs.and.fees <- rbind(
        divs.and.fees,
        tx[, c("Date", "Reference.Symbol", "Quantity", "Price")],
        data.frame(
          Date=range(curr$size.vs.time.tables$Portfolio$Date),
          Reference.Symbol="",
          Quantity=0,
          Price=0))
      curr$portfolio.size.snapshot <- calc.portfolio.size.snapshot.with.day.over.day.gain(
        divs.and.fees, price.provider, current.date, curr$size.vs.time.tables[symbols], peak.prices)
      curr$portfolio.size.snapshot <- group.options.and.total.in.portfolio.size.snapshot(
        curr$portfolio.size.snapshot, curr$misc.syms.for.port.sum)
      curr$portfolio.size.snapshot
    },
    recent.options=function(tx, prev, curr) {
      tx <- rbind(prev$recent.options, tx[, c("Symbol", "Date")])
      get.recent.options(tx, curr$portfolio.size.snapshot, curr$misc.syms.for.port.sum)
    },
    recent.transactions=function(tx, prev, curr) {
      recent.symbols <- symbols[
        !is.options.symbol(symbols) | symbols %in% curr$recent.options$Symbol]
      curr$recent.transactions <- lapply.and.set.names(recent.symbols, function(symbol) {
        for.symbol <- cbind.or.empty(
          prev$recent.transactions[[symbol]], Reference.Symbol=if (symbol == "$") "$" else "")
        for.symbol <- rbind(
          for.symbol,
          tx[tx$Symbol == symbol, c("Date", "Price", "Quantity", "Cost", "Reference.Symbol")])
        for.symbol <- cbind.or.empty(Symbol=symbol, for.symbol)
        get.recent.transactions(for.symbol, symbol)
      })
      curr$recent.transactions
    },
    entry.price.and.quantity=function(tx, prev, curr) {
      curr$entry.price.and.quantity <- NULL
      if (!is.null(target.net.pct.drawdown) && !is.null(target.portfolio.net.cost)) {
        # This is needed by plot.static.
        if (!obfuscate.cost) {
          if (!is.null(prev)) {
            # TODO: support this.
            stop("Cache cannot be used for entry.price.and.quantity")
          }
          curr$entry.price.and.quantity <- solve.entry.price.and.quantity(
            tx, target.net.pct.drawdown, target.portfolio.net.cost)
        }
      }
      curr$entry.price.and.quantity
    },
    tx.nearby=function(tx, prev, curr) {
      curr$tx.nearby <- NULL
      if (!is.null(target.net.pct.drawdown) && !is.null(target.portfolio.net.cost)) {
        # This is needed by plot.static.
        if (!is.null(prev)) {
          # TODO: support this.
          stop("Cache cannot be used for entry.price.and.quantity")
        }
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
      incremental.tx <- topologically.sorted.table.dependency.graph[[step.name]](
        incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables)
    } else {
      curr.agg.tx.tables[[table.name]] <- topologically.sorted.table.dependency.graph[[step.name]](
        incremental.tx, prev.agg.tx.tables, curr.agg.tx.tables)
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
    full.tx, price.provider=recent.transaction.price.provider, interesting.symbols=c(),
    cache.path="cache", obfuscate.cost=FALSE, target.net.pct.drawdown=NULL,
    target.portfolio.net.cost=NULL) {
  symbols <- unique(full.tx$Symbol)
  current.dates.by.symbol <- get.current.dates.by.symbol(symbols, price.provider)
  current.date <- max(do.call(c, current.dates.by.symbol))
  
  version <- get.schema.version()
  version <- packBits(as.raw(c(1,1,1,1,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,0,1,1,0,0,1,1,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,0,1,1,1,1,0,0,0,1,0,1,1,1,1,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,0,1,1,1,1,0,0,1,1,1,1,0,1,0,1,1,0,1,1,0,1,0,0,0,0)))
  if (file.exists(cache.path)) {
    all.prev.agg.tx.tables <- readRDS(cache.path)
    all.prev.agg.tx.tables <- all.prev.agg.tx.tables[
      unlist(lapply(all.prev.agg.tx.tables, function(prev.agg.tx.tables) (
        prev.agg.tx.tables$version == version
        && max(prev.agg.tx.tables$current.date, max(prev.agg.tx.tables$tx$Date))
        < max(current.date, max(full.tx$Date))
      )))
    ]
  } else {
    all.prev.agg.tx.tables <- list()
  }
  
  prev.agg.tx.tables <- NULL
  incremental.tx <- full.tx
  # Try to find previous cached tables that are compatible, from most recent to most distant so that
  #  we maximize the amount of rows reused. Once such a set of tables is found, filter
  #  all.prev.agg.tx.tables to just that one set so that the new cache we write only contains the
  #  two most recent sets of tables. If no such set of tables is found, set all.prev.agg.tx.tables
  #  to an empty list.
  while (is.null(prev.agg.tx.tables) && length(all.prev.agg.tx.tables) != 0) {
    prev.agg.tx.tables <- tail(all.prev.agg.tx.tables, 1)[[1]]
    prev.tx <- prev.agg.tx.tables$tx
    our.prev.tx <- full.tx[full.tx$Date <= max(prev.tx$Date), ]
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
      incremental.tx <- full.tx[full.tx$Date > max(prev.tx$Date), ]
      all.prev.agg.tx.tables <- tail(all.prev.agg.tx.tables, 1)
    } else {
      prev.agg.tx.tables <- NULL
      all.prev.agg.tx.tables <- head(all.prev.agg.tx.tables, -1)
    }
  }
  
  incremental.tx <- coalesce.tx.by.date.symbol.price(incremental.tx)
  incremental.tx <- join.cost(incremental.tx)
  prev.peak.prices <- lapply(
    prev.agg.tx.tables$price.vs.time.tables, function(price.vs.time) price.vs.time$peak.price)
  # TODO: maybe it would be more robust to just select Symbol and Price from incremental.tx when
  #  passing to join.pct.drawdown and then cbind the Pct.Drawdown column to incremental.tx after
  #  removing the prev.peak.prices rows since row order is conserved.
  prev.peak.prices <- do.call(
    rbind, lapply(unique(incremental.tx$Symbol), function(symbol) data.frame.or.empty(
      Date=as.Date(NA),
      Symbol=symbol,
      Reference.Symbol=NA_character_,
      Price=as.numeric(prev.agg.tx.tables$price.vs.time.tables[[symbol]]$peak.price),
      Quantity=NA_real_,
      Cost=NA_real_)))
  incremental.tx <- join.pct.drawdown(rbind(prev.peak.prices, incremental.tx))
  if (NROW(prev.peak.prices) != 0) {
    stopifnot(
      all(
        head(
          incremental.tx, nrow(prev.peak.prices)
        )[, c("Symbol", "Price")] == prev.peak.prices[, c("Symbol", "Price")]))
    incremental.tx <- tail(incremental.tx, -NROW(prev.peak.prices))
  }
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
      list(c(agg.tx.tables, list(tx=full.tx, price.provider=price.provider$dump())))),
    cache.path)
  agg.tx.tables
}
