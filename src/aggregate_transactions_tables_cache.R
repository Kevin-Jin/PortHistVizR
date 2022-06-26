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

merge.tables <- function(
    tx,
    interesting.symbols,
    obfuscate.cost,
    symbols,
    price.provider,
    current.date,
    target.net.pct.drawdown,
    target.portfolio.net.cost) {
  topologically.sorted.table.dependency.graph <- list(
    .is.interesting.option=function(tx, curr) {
      grepl(" options$", interesting.symbols)
    },
    interesting.stocks=function(tx, curr) {
      interesting.symbols[!curr$is.interesting.option]
    },
    interesting.options=function(tx, curr) {
      group.interesting.options(tx, interesting.symbols, curr$is.interesting.option)
    },
    .cost.by.symbol=function(tx, curr) {
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
    .tx=function(tx, curr) {
      if (obfuscate.cost) {
        tx$Cost <- 100 * tx$Cost / abs(
          curr$cost.by.symbol$Cost[match(tx$Symbol, curr$cost.by.symbol$Symbol)])
        tx$Quantity <- 100 * tx$Quantity / abs(
          curr$cost.by.symbol$Cost[match(tx$Symbol, curr$cost.by.symbol$Symbol)])
      }
      tx
    },
    interesting.stocks..sorted=function(tx, curr) {
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
    price.vs.time.tables=function(tx, curr) {
      lapply.and.set.names(
        curr$interesting.stocks, function(symbol) calc.price.vs.time(tx, symbol))
    },
    size.vs.price.tables=function(tx, curr) {
      lapply.and.set.names(
        curr$interesting.stocks, function(symbol) calc.size.vs.price(tx, symbol, 0, FALSE))
    },
    size.vs.time.tables..single.symbols=function(tx, curr) {
      # Size vs. time tables for options and miscellaneous stocks are not plotted directly, but are
      #  used as intermediate inputs to portfolio size vs. time tables and the portfolio size
      #  snapshot.
      curr$size.vs.time.tables <- lapply.and.set.names(
        symbols, function(symbol) calc.size.vs.time(tx, symbol, price.provider))
      curr$size.vs.time.tables <- get.size.vs.time.tables.with.opt.roots(
        price.provider, curr$size.vs.time.tables, curr$interesting.options, current.date)
      curr$size.vs.time.tables
    },
    unit.values=function(tx, curr) {
      lapply.and.set.names(
        names(curr$size.vs.time.tables),
        function(symbol) get.current.price(curr$size.vs.time.tables[[symbol]]))
    },
    size.vs.time.tables..options=function(tx, curr) {
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
    miscellaneous.symbols=function(tx, curr) {
      get.miscellaneous.symbols(symbols, curr$interesting.stocks, curr$interesting.options)
    },
    size.vs.time.tables..misc.and.port.and.ex.cash=function(tx, curr) {
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
    unit.costs=function(tx, curr) {
      lapply.and.set.names(symbols, function(symbol) get.unit.cost(tx, symbol))
    },
    .misc.syms.for.port.sum=function(tx, curr) {
      get.misc.syms.for.port.sum(curr$miscellaneous.symbols)
    },
    portfolio.size.snapshot=function(tx, curr) {
      peak.prices <- lapply(
        curr$price.vs.time.tables, function(price.vs.time) price.vs.time$peak.price)
      curr$portfolio.size.snapshot <- calc.portfolio.size.snapshot.with.day.over.day.gain(
        tx, price.provider, current.date, curr$size.vs.time.tables[symbols], peak.prices)
      curr$portfolio.size.snapshot <- group.options.and.total.in.portfolio.size.snapshot(
        curr$portfolio.size.snapshot, curr$misc.syms.for.port.sum)
      curr$portfolio.size.snapshot
    },
    recent.options=function(tx, curr) {
      get.recent.options(tx, curr$portfolio.size.snapshot, curr$misc.syms.for.port.sum)
    },
    recent.transactions=function(tx, curr) {
      recent.symbols <- symbols[!is.options.symbol(symbols) | symbols %in% curr$recent.options]
      curr$recent.transactions <- lapply.and.set.names(
        recent.symbols, function(symbol) get.recent.transactions(tx, symbol))
      curr$recent.transactions
    },
    entry.price.and.quantity=function(tx, curr) {
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
    tx.nearby=function(tx, curr) {
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
    if (table.name == "tx") {
      tx <- topologically.sorted.table.dependency.graph[[step.name]](tx, curr.agg.tx.tables)
    } else {
      curr.agg.tx.tables[[table.name]] <- topologically.sorted.table.dependency.graph[[step.name]](
        tx, curr.agg.tx.tables)
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
    obfuscate.cost=FALSE, target.net.pct.drawdown=NULL, target.portfolio.net.cost=NULL) {
  symbols <- unique(tx$Symbol)
  current.dates.by.symbol <- get.current.dates.by.symbol(symbols, price.provider)
  current.date <- max(do.call(c, current.dates.by.symbol))

  tx <- coalesce.tx.by.date.symbol.price(tx)
  tx <- join.cost(tx)
  tx <- join.pct.drawdown(tx)
  agg.tx.tables <- merge.tables(
    tx,
    interesting.symbols,
    obfuscate.cost,
    symbols,
    price.provider,
    current.date,
    target.net.pct.drawdown,
    target.portfolio.net.cost)
  agg.tx.tables <- c(
    list(
      current.dates.by.symbol=current.dates.by.symbol,
      current.date=current.date
    ),
    agg.tx.tables
  )
  agg.tx.tables
}
