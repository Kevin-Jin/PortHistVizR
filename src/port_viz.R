setwd(dirname(parent.frame(2)$ofile))
stopifnot(basename(getwd()) == "src")
setwd(dirname(getwd()))

library(DT)
library(highcharter)
library(htmltools)
library(htmlwidgets)
library(manipulateWidget)
library(shiny)
library(timeDate)

source("src/price_providers.R")
source("src/broker_history_parsers.R")
source("src/transaction_aggregators.R")
source("src/aggregate_transactions_tables_cache.R")
source("src/static_plotters.R")
source("src/interactive_plotters.R")

port.viz <- function(
    transaction.file.loaders, price.file.loaders=list(), interesting.symbols=NULL,
    portfolio.name=NULL, alpha.vantage.key=NULL, day.lag=0, from.date=as.Date("0001-01-01"),
    to.date=as.Date("9999-12-31"), obfuscate.cost=FALSE, target.net.pct.drawdown=30,
    target.portfolio.net.cost=120000, static=FALSE) {
  if (!is.null(alpha.vantage.key)) {
    refresh.alphavantage.prices(transaction.file.loaders, alpha.vantage.key, day.lag)
  }
  
  tx <- do.call(rbind, lapply(
    names(transaction.file.loaders),
    function(file.name) transaction.file.loaders[[file.name]](file.name)))
  tx <- filter.tx(tx, NULL, from.date, to.date)
  price.provider <- Reduce(
    function(fallback, file.name) price.file.loaders[[file.name]](file.name, fallback),
    names(price.file.loaders),
    recent.transaction.price.provider)
  dir.create("output", showWarnings=FALSE)
  file.name <- if (is.null(portfolio.name)) "PortViz" else sprintf("PortViz_%s", portfolio.name)
  
  if (static) {
    agg.tx.tables <- create.agg.tx.tables(
      tx, price.provider, interesting.symbols, obfuscate.cost, target.net.pct.drawdown,
      target.portfolio.net.cost)
    plot.static(agg.tx.tables, file.path("output", sprintf("%s.pdf", file.name)))
  } else {
    agg.tx.tables <- create.agg.tx.tables(tx, price.provider, interesting.symbols, obfuscate.cost)
    plot.interactive(agg.tx.tables, file.path("output", sprintf("%s.html", file.name)))
  }
  
  print(Sys.time())
}
