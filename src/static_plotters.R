plot.price.vs.time <- function(agg.tx.tables, symbol, pct.drawdown.major=TRUE) {
  tx <- agg.tx.tables$price.vs.time.tables[[symbol]]
  peak.price <- tx$peak.price
  purchase.prices <- tx$purchase.prices[, c("Date", "Average")]
  sale.prices <- tx$sale.prices[, c("Date", "Average")]
  
  if (nrow(sale.prices) > 0) {
    # Consider Sales in axis bounds when first plotting only Purchases.
    purchase.prices <- rbind(purchase.prices, data.frame(Date=range(sale.prices$Date), Average=NA))
    purchase.prices <- rbind(purchase.prices, data.frame(
      Date=NA, Average=range(sale.prices$Average)))
  }
  
  if (pct.drawdown.major) {
    purchase.prices$Average <- 100 * (1 - purchase.prices$Average / peak.price)
    sale.prices$Average <- 100 * (1 - sale.prices$Average / peak.price)
    ylab <- "Pct.Drawdown"
    if (all(is.na(purchase.prices$Average)))
      stop(symbol)
    ylim <- rev(range(purchase.prices$Average, na.rm=TRUE))
  } else {
    ylab <- "Price"
    ylim <- NULL
  }
  
  plot(
    purchase.prices, type="o", col=1, cex=0.6, ylab=ylab, main=paste(symbol, "Price vs. Time"),
    ylim=ylim, las=1)
  if (pct.drawdown.major) {
    axis(4, at=axTicks(2), labels=round((1 - axTicks(2) / 100) * peak.price, 2))
    mtext("Price", side=4, line=3, cex=0.8)
  } else {
    axis(4, at=axTicks(2), labels=round(100 * (1 - axTicks(2) / peak.price), 2))
    mtext("Pct.Drawdown", side=4, line=3, cex=0.8)
  }
  lines(sale.prices, type="o", col=2, cex=0.6)
  grid(nx=20, ny=NULL, col="gray")
  legend("bottomleft", legend=c("Purchase", "Sale"), lty=1, pch=1, col=1:2, pt.cex=0.6, bg="white")
}

plot.size.vs.price <- function(agg.tx.tables, symbol) {
  # Objective: cumulative cost should be quasiconvex with respect to % drawdown. The only local
  #  minimum is the global minimum and the only local maxima are at 0% drawdown and max drawdown.
  #  In other words, the function is weakly monotonically decreasing with percent drawdown up to
  #  some vertex and then becomes weakly monotonically increasing.
  # Optimal cumulative cost function is a step function that has an upwards impulse at max drawdown,
  #  has a downwards impulse at 0% drawdown, and is flat everywhere else, i.e. all purchases are
  #  concentrated at the bottom and all sales are concentrated at the top.
  # To achieve that goal, we should sell at all concavities and steepen local convexities.
  costs <- agg.tx.tables$size.vs.price.tables[[symbol]][, c("Pct.Drawdown", "Cum.Cost")]
  costs <- rbind(data.frame(Pct.Drawdown=0, Cum.Cost=0), costs)
  ylim <- range(costs$Cum.Cost)
  plot(
    costs$Pct.Drawdown, costs$Cum.Cost, xlab="Pct.Drawdown", ylab="Cost",
    main=paste(symbol, "Size vs. Price"), type="o", col=2, cex=0.6, ylim=ylim, xaxt="n")
  axis(1, at=costs$Pct.Drawdown)
  legend <- "Cum.Cost"
  grid(nx=NA, ny=NULL, col="gray")
  abline(v=costs$Pct.Drawdown, col="gray", lty="dotted")
  legend("top", legend=legend, lty=1:2, pch=1, col=2:3, pt.cex=0.6, bg="white")
}

plot.size.vs.time <- function(agg.tx.tables, symbol) {
  costs <- agg.tx.tables$size.vs.time.tables[[symbol]][, c("Date", "Cost", "Value")]
  
  # Consider Value in axis bounds when first plotting only Cost.
  costs <- rbind(costs, data.frame(Date=NA, Cost=range(costs$Value), Value=NA))
  plot(
    costs[, c("Date", "Cost")], type="o", col=1, cex=0.6, ylab="Position Size",
    main=paste(symbol, "Size vs. Time"))
  lines(costs[, c("Date", "Value")], type="o", col=2, cex=0.6)
  grid(nx=20, ny=NULL, col="gray")
  legend("topleft", legend=c("Cost", "Value"), lty=1, pch=1, col=1:2, pt.cex=0.6, bg="white")
}

plot.text <- function(text) {
  backup_options <- options()
  options(scipen=999)
  tryCatch({
    plot(c(0, 1), c(0, 1), ann=FALSE, bty="n", type="n", xaxt="n", yaxt="n")
    text(x=0.5, y=0.5, text, cex=1, col="black", family="mono")
  }, finally = {
    options(backup_options)
  })
}

plot.portfolio.summary <- function(agg.tx.tables) {
  # The net price is the breakeven price for the symbol. The net cost is the current size of the
  #  position in the symbol.
  agg.all <- paste(
    capture.output(print(agg.tx.tables$portfolio.size.snapshot, row.names=FALSE)), collapse="\n")
  cat(agg.all)
  cat("\n")
  plot.text(agg.all)
  if (!is.null(agg.tx.tables$entry.price.and.quantity)) {
    targets <- paste(
      capture.output(print(agg.tx.tables$entry.price.and.quantity, row.names=FALSE)), collapse="\n")
    cat(targets)
    cat("\n")
    plot.text(targets)
  }
}

plot.for.symbol <- function(agg.tx.tables, symbol) {
  par(mfrow=c(2, 2), mar=c(5, 4, 4, 4) + 0.1)
  plot.price.vs.time(agg.tx.tables, symbol)
  plot.size.vs.price(agg.tx.tables, symbol)
  plot.size.vs.time(agg.tx.tables, symbol)
  tx.nearby <- paste(
    capture.output(print(agg.tx.tables$tx.nearby[[symbol]], row.names=FALSE)),
    collapse="\n")
  plot.text(tx.nearby)
}

plot.static <- function(
    agg.tx.tables, output.path=file.path("output", "PortViz.pdf")) {
  pdf(output.path, width=14, height=8.5, pointsize=9)
  tryCatch({
    plot.portfolio.summary(agg.tx.tables)
    for (symbol in agg.tx.tables$interesting.stocks) {
      plot.for.symbol(agg.tx.tables, symbol)
    }
  }, finally={
    dev.off()
  })
}
