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
source("src/static_plotters.R")
source("src/interactive_plotters.R")

port.viz <- function(
    transaction.file.loaders, price.file.loaders=list(), interesting.symbols=NULL,
    portfolio.name=NULL, alpha.vantage.key=NULL, day.lag=0, from.date=as.Date("0001-01-01"),
    to.date=as.Date("9999-12-31"), obfuscate.cost=FALSE, target.net.pct.drawdown=30,
    target.portfolio.net.cost=120000) {
  if (!is.null(alpha.vantage.key)) {
    refresh.alphavantage.prices(transaction.file.loaders, alpha.vantage.key, day.lag)
  }
  
  tx <- do.call(rbind, lapply(
    names(transaction.file.loaders),
    function(file.name) transaction.file.loaders[[file.name]](file.name)))
  tx <- coalesce.tx.by.date.symbol.price(tx)
  tx <- join.cost(tx)
  tx <- join.pct.drawdown(tx)
  tx <- filter.tx(tx, NULL, from.date, to.date)
  tx.by.symbol <- reduce.on.factor(
    tx, "Symbol", function(tx.for.symbol) data.frame(Cost=sum(tx.for.symbol$Cost)))
  if (obfuscate.cost) {
    tx$Cost <- 100 * tx$Cost / abs(tx.by.symbol$Cost[match(tx$Symbol, tx.by.symbol$Symbol)])
    tx$Quantity <- 100 * tx$Quantity / abs(tx.by.symbol$Cost[match(tx$Symbol, tx.by.symbol$Symbol)])
  }
  
  price.provider <- Reduce(
    function(fallback, file.name) price.file.loaders[[file.name]](file.name, fallback),
    names(price.file.loaders),
    recent.transaction.price.provider)
  
  is.stock <- !grepl(" options$", interesting.symbols)
  ordered.symbols <- tx.by.symbol[tx.by.symbol$Symbol %in% interesting.symbols[is.stock], ]
  ordered.symbols <- ordered.symbols$Symbol[order(ordered.symbols$Cost, decreasing=TRUE)]
  dir.create("output", showWarnings=FALSE)
  file.name <- if (is.null(portfolio.name)) "PortViz" else sprintf("PortViz_%s", portfolio.name)
  interesting.symbols <- c(ordered.symbols, interesting.symbols[!is.stock])
  # plot.static(
  #   tx, interesting.symbols, obfuscate.cost, target.net.pct.drawdown, target.portfolio.net.cost,
  #   price.provider, file.path("output", sprintf("%s.pdf", file.name)))
  plot.interactive(
    tx, interesting.symbols, price.provider, file.path("output", sprintf("%s.html", file.name)))
}
