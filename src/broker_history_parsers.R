# Mutual funds allow 3 digits of precision after the decimal place for quantities.
# Schwab fractional shares allow for 4 digits of precision after the decimal place.
# Robinhood fractional shares allow for 6 digits of precision after the decimal place.
# Make sure to round any quantity sums to avoid problems when checking whether a quantity is zero,
#  negative, or positive and to avoid unnecessary digits when formatting the number as a string.
# Floating point error is well below 1e-6, so this level of precision should not introduce numerical
#  noise for brokers with lower precision quantities.
QTY_FRAC_DIGITS <- 6

is.options.symbol <- function(symbol) {
  regexpr("^[\\w ]{6}\\d{6}[CP]\\d{8}$", symbol, perl=TRUE) != -1
}

adjust.options.prices <- function(prices.for.symbols, contract.multiplier=100, commission=0.65) {
  # Price can only be 0 for options expiration transactions. Expiration has no commissions.
  is.opt <- is.options.symbol(prices.for.symbols$Symbol) & prices.for.symbols$Price != 0
  prices.for.symbols$Price[is.opt] <- prices.for.symbols$Price[is.opt] * contract.multiplier
  if (commission != 0) {
    prices.for.symbols$Price[is.opt] <- (
      prices.for.symbols$Price[is.opt] + sign(prices.for.symbols$Quantity[is.opt]) * commission)
  }
  
  prices.for.symbols
}

adjust.for.schwab.corporate.actions <- function(tx, commission=0.65) {
  rev.split.orig.symbols <- tx[
    tx$Action == "Reverse Split" & tx$Quantity < 0, c("Date", "Quantity")]
  rev.split.new.symbols <- tx[
    tx$Action == "Reverse Split" & tx$Quantity > 0, c("Date", "Symbol", "Quantity")]
  rev.split.new.symbols <- rev.split.new.symbols[order(rev.split.new.symbols$Date), ]
  for (i in seq_len(nrow(rev.split.new.symbols))) {
    is.pre.split <- (
      tx$Symbol == rev.split.new.symbols$Symbol[i] & tx$Date < rev.split.new.symbols$Date[i])
    orig.quantity <- sum(tx$Quantity[is.pre.split])
    # TODO: this fails on splits where we have a remainder, e.g. a 10 for 1 split when we used to
    #  have 11 shares. We lose 11 old shares but only gain 1 new share plus cash in lieu of the 0.1
    #  fractional shares.
    stopifnot(nrow(rev.split.orig.symbols[
      rev.split.orig.symbols$Date == rev.split.new.symbols$Date[i]
      & -rev.split.orig.symbols$Quantity == orig.quantity, ]) > 0)
    tx$Quantity[is.pre.split] <- (
      tx$Quantity[is.pre.split] * rev.split.new.symbols$Quantity[i] / orig.quantity)
    tx$Price[is.pre.split] <- (
      tx$Price[is.pre.split] * orig.quantity / rev.split.new.symbols$Quantity[i])
  }
  tx <- tx[tx$Action != "Reverse Split", ]
  
  # Expired options are worthless.
  is.expired <- tx$Action %in% c("Expired", "Assigned")
  stopifnot(all(is.na(tx[is.expired, c("Price", "Amount")])))
  stopifnot(all(tx[is.expired, "Fees & Comm"] == 0))
  stopifnot(all(tx[tx$Action == "Expired", "Quantity"] < 0))
  stopifnot(all(tx[tx$Action == "Assigned", "Quantity"] > 0))
  tx[is.expired, c("Price", "Fees & Comm", "Amount")] <- 0
  
  # 0.65 options commission will be added to tx$Price by adjust.options.prices,
  #  so remove it from the fees to avoid double counting.
  # Price can only be 0 for options expiration transactions. Expiration has no commissions.
  is.opt <- is.options.symbol(tx$Symbol) & tx$Price != 0
  tx$`Fees & Comm`[is.opt] <- tx$`Fees & Comm`[is.opt] - commission * abs(tx$Quantity[is.opt])
  
  tx
}

normalize.cash.transactions <- function(tx) {
  tx$Symbol[is.na(tx$Quantity)] <- "$"
  tx$Price[is.na(tx$Quantity)] <- 1
  tx$Quantity[is.na(tx$Quantity)] <- tx$Amount[is.na(tx$Quantity)]
  
  tx
}

normalize.schwab.option.symbol <- function(tx) {
  option.matches <- regexpr(
    "^(\\w+) (\\d{2}/\\d{2}/\\d{4}) (\\d+\\.\\d+) ([CP])$", tx$Symbol, perl=TRUE)
  matched.symbols <- tx$Symbol[option.matches != -1]
  capture.start <- attr(option.matches, "capture.start")[option.matches != -1, ]
  capture.stop <- capture.start + attr(option.matches, "capture.length")[option.matches != -1, ] - 1

  root.symbol <- substr(matched.symbols, capture.start[, 1], capture.stop[, 1])
  expiration.date <- as.Date(
    substr(matched.symbols, capture.start[, 2], capture.stop[, 2]), "%m/%d/%Y")
  strike <- as.numeric(substr(matched.symbols, capture.start[, 3], capture.stop[, 3]))
  claim.type <- substr(matched.symbols, capture.start[, 4], capture.stop[, 4])

  stopifnot(all(nchar(root.symbol) <= 6 & !is.na(expiration.date) & strike < 100000 & as.integer(
    strike * 1000) == strike * 1000))
  occ.code <- sprintf("%-6s%s%s%08d", root.symbol, as.character(
    expiration.date, "%y%m%d"), claim.type, as.integer(strike * 1000))

  tx$Symbol[option.matches != -1] <- occ.code
  tx
}

append.contra.tx <- function(tx) {
  # Options expirations are cashless transactions. Exclude those to avoid division by 0 in a day's
  #  aggregate cash change when options expirations are the only transactions for the day.
  contra.tx <- tx[tx$Symbol != "$" & tx$Amount != 0, ]
  if (nrow(contra.tx) > 0) {
    fees.and.comm <- contra.tx
    # Amount is not exactly Quantity * Price because Amount is rounded to two decimal places after
    #  Price, which has four decimal places of precision, is multiplied with Quantity. Allocate the
    #  rounding error as a fee rather than sneaking it into the cash contra transaction to make sure
    #  changes in cash costs are exactly equal and opposite to changes in securities costs.
    fees.and.comm$Amount <- round(
      fees.and.comm$Quantity * fees.and.comm$Price + fees.and.comm$Amount, 4)
    fees.and.comm <- fees.and.comm[fees.and.comm$Amount != 0, ]
    fees.and.comm$Quantity <- fees.and.comm$Amount
    fees.and.comm$Reference.Symbol <- fees.and.comm$Symbol
    fees.and.comm$Symbol <- "$"
    fees.and.comm$Price <- 1
    fees.and.comm$`Fees & Comm` <- 0
    
    # Fees impact the value of cash positions but not the cost of cash positions. Contra
    #  transactions impact both the value and the cost of cash positions. Fee transactions are
    #  distinguished from contra transactions by the presence of a non-empty reference symbol.
    contra.tx$Amount <- round(-contra.tx$Quantity * contra.tx$Price, 4)
    contra.tx$Quantity <- contra.tx$Amount
    contra.tx$Symbol <- "$"
    contra.tx$Price <- 1
    contra.tx$`Fees & Comm` <- 0
    
    contra.tx <- rbind(fees.and.comm, contra.tx)
  }
  
  rbind(tx, contra.tx)
}

load.schwab.transactions <- function(export.file) {
  security.actions <- data.frame(
    Action=c(
      "Buy", "Buy to Open", "Buy to Close",
      "Sell", "Sell to Open", "Sell to Close", "Sell Short",
      "Reverse Split", "Expired", "Assigned"),
    Sign=c(
      +1, +1, +1,
      -1, -1, -1, -1,
      +1, +1, +1))
  cash.actions <- c(
    "Journal", "MoneyLink Transfer",
    "Bank Interest", "Cash Dividend", "Pr Yr Cash Div", "Service Fee")
  
  tx <- read.csv(export.file, skip=1, stringsAsFactors=FALSE, check.names=FALSE)
  stopifnot(tail(tx$Date, 1) == "Transactions Total")
  tx <- head(tx, -1)
  
  tx$Date <- unlist(
    lapply(strsplit(tx$Date, " as of ", fixed=TRUE), function(parts) tail(parts, 1)))
  tx$Date <- as.Date(tx$Date, "%m/%d/%Y")
  # read.csv interprets blanks in a string column as empty strings, but read.csv can't infer Symbol
  #  is a string column if it's blank in every row so give it a hint here. This happens if only cash
  #  transactions exist in the history.
  tx$Symbol[is.na(tx$Symbol)] <- ""
  # This happens if only small buys and sells below the fee threshold exist in the history.
  tx$`Fees & Comm`[is.na(tx$`Fees & Comm`)] <- ""
  tx <- normalize.schwab.option.symbol(tx)
  tx$Quantity <- security.actions$Sign[match(tx$Action, security.actions$Action)] * tx$Quantity
  tx$Price <- as.numeric(gsub("^(-?)\\$", "\\1", tx$Price))
  tx$`Fees & Comm`[tx$`Fees & Comm` == ""] <- "$0"
  tx$`Fees & Comm` <- as.numeric(gsub("^(-?)\\$", "\\1", tx$`Fees & Comm`))
  tx$Amount <- as.numeric(gsub("^(-?)\\$", "\\1", tx$Amount))
  
  tx$Reference.Symbol <- ifelse(tx$Action %in% c("Cash Dividend", "Pr Yr Cash Div"), tx$Symbol, "")
  tx$Reference.Symbol[tx$Action == "Bank Interest"] <- "$"
  tx$Reference.Symbol[tx$Action == "Service Fee"] <- tx$Description[tx$Action == "Service Fee"]
  borrow.fee.matches <- regexpr("^STOCK BORROW FEE/(.+)$", tx$Reference.Symbol, perl=TRUE)
  matched.reference.symbols <- tx$Reference.Symbol[borrow.fee.matches != -1]
  capture.start <- attr(borrow.fee.matches, "capture.start")[borrow.fee.matches != -1, , drop=FALSE]
  capture.stop <- capture.start + attr(borrow.fee.matches, "capture.length")[
    borrow.fee.matches != -1, , drop=FALSE] - 1
  tx$Reference.Symbol[borrow.fee.matches != -1] <- substr(
    matched.reference.symbols, capture.start[, 1], capture.stop[, 1])
  
  stopifnot(all(!is.na(tx$Date)))
  stopifnot(all(tx$Symbol != "$"))
  stopifnot(all(tx$Action %in% cash.actions == is.na(tx$Quantity)))
  stopifnot(all(!is.na(tx$Symbol) & tx$Symbol != "" | is.na(tx$Quantity)))
  tx <- normalize.cash.transactions(tx)
  tx <- adjust.for.schwab.corporate.actions(tx)
  tx <- adjust.options.prices(tx)
  tx <- append.contra.tx(tx)
  # Make sure Quantity, Price, Fees & Comm, and Amount are consistent before
  #  removing Fees & Comm and Amount information.
  tx$Amount[tx$Symbol != "$"] <- -tx$Amount[tx$Symbol != "$"]
  stopifnot(max(abs(tx$Quantity * tx$Price + tx$`Fees & Comm` - tx$Amount)) < 0.01)
  
  tx <- tx[, c("Date", "Symbol", "Quantity", "Price", "Reference.Symbol")]
  stopifnot(all(!is.na(tx$Price)))
  tx
}

adjust.for.fidelity.corporate.actions <- function(tx.part, commission=0.65) {
  # 0.65 options commission will be added to tx.part$Price by adjust.options.prices,
  #  so remove it from the fees to avoid double counting.
  # Price can only be 0 for options expiration transactions. Expiration has no commissions.
  is.opt <- is.options.symbol(tx.part$Symbol) & tx.part$Price != 0
  tx.part$`Fees & Comm`[is.opt] <- (
    tx.part$`Fees & Comm`[is.opt] - commission * abs(tx.part$Quantity[is.opt]))
  
  tx.part
}

normalize.fidelity.option.symbol <- function(tx.part) {
  option.matches <- regexpr(
    "^-(\\w+)(\\d{6})([CP])(\\d+(?:\\.\\d+)?)$", tx.part$Symbol, perl=TRUE)
  matched.symbols <- tx.part$Symbol[option.matches != -1]
  capture.start <- attr(option.matches, "capture.start")[option.matches != -1, ]
  capture.stop <- capture.start + attr(option.matches, "capture.length")[option.matches != -1, ] - 1

  root.symbol <- substr(matched.symbols, capture.start[, 1], capture.stop[, 1])
  expiration.date <- as.Date(
    substr(matched.symbols, capture.start[, 2], capture.stop[, 2]), "%y%m%d")
  claim.type <- substr(matched.symbols, capture.start[, 3], capture.stop[, 3])
  strike <- as.numeric(substr(matched.symbols, capture.start[, 4], capture.stop[, 4]))
  
  stopifnot(all(nchar(root.symbol) <= 6 & !is.na(expiration.date) & strike < 100000 & as.integer(
    strike * 1000) == strike * 1000))
  occ.code <- sprintf("%-6s%s%s%08d", root.symbol, as.character(
    expiration.date, "%y%m%d"), claim.type, as.integer(strike * 1000))
  
  tx.part$Symbol[option.matches != -1] <- occ.code
  tx.part
}

normalize.fidelity.option.expirations <- function(tx.part) {
  expired.tx <- grepl("^EXPIRED", tx.part$Action)
  stopifnot(all(!is.na(tx.part$Quantity[expired.tx])))
  stopifnot(all(is.na(tx.part[expired.tx, c("Price", "Amount")])))
  tx.part[expired.tx, c("Price", "Amount")] <- 0
  
  as.of.matches <- regexpr(
    "as of (\\d{2}/\\d{2}/\\d{4})", tx.part$Action[expired.tx], perl=TRUE)
  matched.actions <- tx.part$Action[expired.tx][as.of.matches != -1]
  capture.start <- attr(as.of.matches, "capture.start")[as.of.matches != -1, , drop=FALSE]
  capture.stop <- capture.start + attr(
    as.of.matches, "capture.length")[as.of.matches != -1, , drop=FALSE] - 1
  as.of.dates <- substr(matched.actions, capture.start[, 1], capture.stop[, 1])
  tx.part$Date[expired.tx][as.of.matches != -1] <- as.Date(as.of.dates, "%m/%d/%Y")
  
  tx.part
}

load.fidelity.transactions <- function(directory, cash.symbol="SPAXX") {
  security.actions <- data.frame(
    Action=c("YOU BOUGHT", "REINVESTMENT", "YOU SOLD", "EXPIRED"),
    Sign=c(+1, +1, -1, -1))
  cash.actions <- c(
    "DIVIDEND RECEIVED", "ROTH CONVERSION", "ROLLOVER", "JOURNALED JNL VS A/C TYPES")
  
  # Fidelity limits the date range in each file to one quarter, so we need to join the older files
  #  to the most recent files to get the full history.
  tx <- NULL
  for (download.file in sort(list.files(directory))) {
    # Fidelity inconsistently prefixes CSVs with either "\n\n\nBrokerage\n\n" or "\n\n\n".
    con <- file(file.path(directory, download.file), "r")
    skip <- 0
    tryCatch({
      while (length(line <- readLines(con, n=1)) != 0 && !grepl(",", line, fixed=TRUE)) {
        stopifnot(line %in% c("", "Brokerage"))
        skip <- skip + 1
      }
    }, finally={
      close(con)
    })
    
    tx.part <- read.csv(
      file.path(directory, download.file), skip=skip, stringsAsFactors=FALSE, check.names=FALSE,
      strip.white=TRUE)
    tx.part <- tx.part[
      !is.na(tx.part[, "Amount ($)"]) | !is.na(tx.part[, "Quantity"]),
      c("Run Date", "Action", "Symbol", "Quantity", "Price ($)", "Commission ($)", "Fees ($)",
        "Amount ($)")]
    names(tx.part) <- c(
      "Date", "Action", "Symbol", "Quantity", "Price", "Commission", "Fees", "Amount")
    
    tx.part$Date <- as.Date(tx.part$Date, "%m/%d/%Y")
    # read.csv interprets blanks in a string column as empty strings, but read.csv can't infer
    #  Symbol is a string column if it's blank in every row so give it a hint here. This happens if
    #  only cash transactions exist in the history.
    tx.part$Symbol[is.na(tx.part$Symbol)] <- ""
    tx.part <- normalize.fidelity.option.symbol(tx.part)
    tx.part$Fees[is.na(tx.part$Fees)] <- 0
    tx.part$Commission[is.na(tx.part$Commission)] <- 0
    tx.part$`Fees & Comm` <- tx.part$Fees + tx.part$Commission
    tx.part <- tx.part[, -which(colnames(tx.part) %in% c("Commission", "Fees"))]
    tx.part$Reference.Symbol <- ""
    # Expect each file to be sorted in descending order.
    #stopifnot(diff(tx.part$Date) <= 0)
    
    tx.part$Reference.Symbol <- ifelse(
      grepl("^DIVIDEND RECEIVED", tx.part$Action), tx.part$Symbol, "")
    tx.part$Reference.Symbol[tx.part$Reference.Symbol %in% cash.symbol] <- "$"
    
    stopifnot(all(tx.part$Symbol != "$"))
    tx.part <- normalize.fidelity.option.expirations(tx.part)
    stopifnot(all(is.na(tx.part$Price) == is.na(tx.part$Quantity)))
    stopifnot(all(grepl(
      paste("^(", paste(cash.actions, collapse="|"), ")", sep=""),
      tx.part$Action[is.na(tx.part$Quantity)])))
    stopifnot(all(grepl(
      paste("^(", paste(security.actions$Action[security.actions$Sign > 0], collapse="|"), ")", sep=""),
      tx.part$Action[!is.na(tx.part$Quantity) & tx.part$Quantity > 0])))
    stopifnot(all(grepl(
      paste("^(", paste(security.actions$Action[security.actions$Sign < 0], collapse="|"), ")", sep=""),
      tx.part$Action[!is.na(tx.part$Quantity) & tx.part$Quantity < 0])))
    stopifnot(all(tx.part$Symbol[startsWith(tx.part$Action, "REINVESTMENT")] %in% cash.symbol))
    tx.part <- normalize.cash.transactions(tx.part)
    stopifnot(all(startsWith(tx.part$Action[tx.part$Symbol %in% cash.symbol], "REINVESTMENT")))
    tx.part <- tx.part[!(tx.part$Symbol %in% cash.symbol), ]
    tx.part <- adjust.for.fidelity.corporate.actions(tx.part)
    tx.part <- adjust.options.prices(tx.part)
    tx.part <- append.contra.tx(tx.part)
    # Make sure Quantity, Price, Fees & Comm, and Amount are consistent before removing Fees & Comm
    #  and Amount information.
    # Fidelity rounds unit prices to 2 decimal places even though it calculates the transaction
    #  amount using the unit price up to 4 decimal places.
    tx.part$Amount[tx.part$Symbol != "$"] <- -tx.part$Amount[tx.part$Symbol != "$"]
    stopifnot(
      max(abs(tx.part$Price - (tx.part$Amount - tx.part$`Fees & Comm`) / tx.part$Quantity)) < 0.01)
    tx.part <- tx.part[, c("Date", "Symbol", "Quantity", "Price", "Reference.Symbol")]
    
    # Make tx.part and tx disjoint.
    overlap <- tx.part$Date[tx.part$Date %in% tx$Date]
    if (length(overlap) > 0) {
      overlap.tx.part <- tx.part[tx.part$Date %in% overlap, ]
      rownames(overlap.tx.part) <- NULL
      overlap.tx <- tx[tx$Date %in% overlap, ]
      rownames(overlap.tx) <- NULL
      stopifnot(identical(overlap.tx.part, overlap.tx))
      tx.part <- tx.part[!(tx.part$Date %in% overlap), ]
    }
    
    # Splicing a tx.part into the middle of tx isn't currently supported, so the sorted transaction
    #  file names better be in the same order as the contained dates.
    is.earlier.part <- max(tx.part$Date) < min(tx$Date, as.Date("9999-12-31"))
    is.later.part <- min(tx.part$Date) > max(tx$Date, as.Date("0001-01-01"))
    stopifnot(is.earlier.part || is.later.part)
    if (is.earlier.part)
      tx <- rbind(tx, tx.part)
    else
      tx <- rbind(tx.part, tx)
  }
  tx
}

load.netbenefits.transactions <- function(directory) {
  tx <- read.csv(
    file.path(directory, "history.csv"), skip=5, stringsAsFactors=FALSE, check.names=FALSE,
    strip.white=TRUE)
  tx <- tx[, c("Date", "Transaction Type", "Investment", "Shares/Unit", "Amount")]
  names(tx) <- c("Date", "Action", "Symbol", "Quantity", "Amount")
  
  tx$Date <- as.Date(tx$Date, "%m/%d/%Y")
  tx$Quantity <- as.numeric(gsub(",", "", tx$Quantity))
  tx$Amount <- as.numeric(gsub(",", "", tx$Amount))
  tx$Price <- tx$Amount / tx$Quantity
  stopifnot(all(tx[tx$Action == "Transfer", c("Quantity", "Amount")] == 0))
  stopifnot(all(tx[tx$Action == "REALIZED G/L", c("Quantity")] == 0))
  
  symbol.mapping <- read.csv(file.path(directory, "symbol_mapping.csv"), stringsAsFactors=FALSE)
  tx$Symbol <- symbol.mapping$Symbol[match(tx$Symbol, symbol.mapping$Investment)]
  tx$Reference.Symbol <- ""
  
  # Dividends are immediately reinvested, so the value of a holding stays the same but its cost
  #  increases since we're technically buying more of the symbol. Split up the dividend reinvestment
  #  into a cash inflow, a purchase, and a cash outflow contra to the purchase.
  tx$Row.Order <- seq_len(nrow(tx))
  dividends <- tx[tx$Action == "DIVIDEND", ]
  if (nrow(dividends) != 0) {
    last.dividend.row <- 0
    new.tx <- NULL
    for (i in seq_len(nrow(dividends))) {
      cash.inflow <- dividends[i, ]
      cash.inflow$Symbol <- "$"
      cash.inflow$Quantity <- cash.inflow$Amount
      cash.inflow$Price <- 1
      cash.inflow$Reference.Symbol <- dividends$Symbol[i]
      cash.outflow <- dividends[i, ]
      cash.outflow$Symbol <- "$"
      cash.outflow$Amount <- -cash.outflow$Amount
      cash.outflow$Quantity <- cash.outflow$Amount
      cash.outflow$Price <- 1
      # tx[dividends$Row.Order[i]] is the purchase, so we just need to insert the cash inflow from
      #  the dividend and the cash contra transaction for the purchase.
      new.tx <- rbind(
        new.tx, tx[(last.dividend.row + 1):dividends$Row.Order[i], ], cash.outflow, cash.inflow)
      last.dividend.row <- dividends$Row.Order[i]
    }
    if (last.dividend.row != nrow(tx)) {
      new.tx <- rbind(new.tx, tx[(last.dividend.row + 1):nrow(tx), ])
    }
    tx <- new.tx
  }
  
  tx <- tx[
    !(tx$Action %in% c("Transfer", "REALIZED G/L")),
    c("Date", "Symbol", "Quantity", "Price", "Reference.Symbol")]
  tx$Price <- round(tx$Price, 4)
  
  tx
}

load.schwab.position.prices <- function(export.file, fallback) {
  metadata <- read.table(file=export.file, stringsAsFactors=FALSE, header=FALSE, nrows=1)[1, 1]
  
  metadata.match <- regexpr(
    "^Positions for account .+ as of \\d{2}:\\d{2} [AP]M .+, (\\d{2}/\\d{2}/\\d{4})$", metadata,
    perl=TRUE)
  stopifnot(metadata.match != -1)
  capture.start <- attr(metadata.match, "capture.start")
  capture.stop <- capture.start + attr(metadata.match, "capture.length") - 1
  date <- as.Date(substr(metadata, capture.start, capture.stop), "%m/%d/%Y")
  
  pos <- read.csv(export.file, skip=1, stringsAsFactors=FALSE, na.strings="--")[, c(
    "Symbol", "Price", "Quantity")]
  pos <- pos[!(pos$Symbol %in% c("Cash & Cash Investments", "Account Total")), ]
  pos$Price <- as.numeric(gsub("^\\$|,", "", pos$Price))
  pos <- normalize.schwab.option.symbol(pos)
  # Convert position quantity into the quantity of a full liquidation trade. Sell when long and buy
  #  when short.
  pos$Quantity <- -pos$Quantity
  pos <- adjust.options.prices(pos)
  
  create.price.provider.with.overrides(data.frame(
    Symbol=pos$Symbol, Date=date, Close=pos$Price, High=pos$Price, Low=pos$Price), fallback)
}

load.alphavantage.prices <- function(directory, fallback) {
  price.overrides <- NULL
  for (file in list.files(directory)) {
    stopifnot(endsWith(file, ".csv"))
    symbol <- substr(file, 1, nchar(file) - 4)
    for.symbol <- read.csv(file.path(directory, file), stringsAsFactors=FALSE)
    for.symbol$timestamp <- as.Date(for.symbol$timestamp)
    # Calculate our own adjusted close that only takes into account splits and stock dividends, but
    #  not cash dividends. Otherwise there will be discrepancies between cost (prices of past
    #  transactions, see adjust.for.schwab.corporate.actions) and value (prices today). Cash
    #  dividends are accounted for as cash transactions in the portfolio, so we want to avoid double
    #  counting them.
    corp.actions <- which(for.symbol$split_coefficient != 1)
    corp.actions <- corp.actions[corp.actions != nrow(for.symbol)]
    for (corp.action in corp.actions) {
      past.indices <- (corp.action + 1):nrow(for.symbol)
      for.symbol$close[past.indices] <- (
        for.symbol$close[past.indices] / for.symbol$split_coefficient[corp.action])
      for.symbol$high[past.indices] <- (
        for.symbol$high[past.indices] / for.symbol$split_coefficient[corp.action])
      for.symbol$low[past.indices] <- (
        for.symbol$low[past.indices] / for.symbol$split_coefficient[corp.action])
    }
    
    if (nrow(for.symbol) > 0) {
      for.symbol <- for.symbol[, c("timestamp", "high", "low", "close")]
      colnames(for.symbol) <- c("Date", "High", "Low", "Close")
      price.overrides <- rbind(
        price.overrides, cbind(Symbol=symbol, for.symbol, stringsAsFactors=FALSE))
    }
  }
  
  create.price.provider.with.overrides(price.overrides, fallback)
}

load.simple.prices <- function(override.file, fallback) {
  price.overrides <- read.csv(override.file, stringsAsFactors=FALSE)
  stopifnot(is.character(price.overrides$Symbol))
  stopifnot(is.numeric(price.overrides$Price))
  price.overrides$Date <- as.Date(price.overrides$Date)
  price.overrides <- adjust.options.prices(price.overrides, commission=0)
  names(price.overrides)[names(price.overrides) == "Price"] <- "Close"
  price.overrides$High <- price.overrides$Close
  price.overrides$Low <- price.overrides$Close
  create.price.provider.with.overrides(price.overrides, fallback)
}

refresh.alphavantage.prices <- function(
    transaction.file.loaders, alpha.vantage.key, day.lag=0, exch.tzone="America/New_York",
    # US exchanges close at 16:00 America/New_York, but allow 30 minutes for Alpha Vantage to
    #  disseminate the official closing auction prices.
    exch.close=c(16, 30), output.folder=file.path("input", "alphavantage"), requests.per.minute=5,
    requests.per.day=500) {
  tx <- do.call(rbind, lapply(
    names(transaction.file.loaders),
    function(file.name) transaction.file.loaders[[file.name]](file.name)))
  tx <- join.cost(tx)
  tx.by.symbol <- reduce.on.factor(tx, "Symbol", function(tx.for.symbol) data.frame(
    Cost=sum(tx.for.symbol$Cost),
    First.Date=min(tx.for.symbol$Date),
    Last.Date=max(tx.for.symbol$Date),
    Quantity=round(sum(tx.for.symbol$Quantity), QTY_FRAC_DIGITS)))
  ordered.symbols <- tx.by.symbol$Symbol[order(tx.by.symbol$Cost, decreasing=TRUE)]
  ordered.symbols <- ordered.symbols[!is.options.symbol(ordered.symbols) & ordered.symbols != "$"]
  
  dir.create(output.folder, showWarnings=FALSE)
  download.times <- NULL
  for (symbol in ordered.symbols) {
    tx.last.date <- tx.by.symbol$Last.Date[tx.by.symbol$Symbol == symbol]
    tx.first.date <- tx.by.symbol$First.Date[tx.by.symbol$Symbol == symbol]
    if (tx.by.symbol$Quantity[tx.by.symbol$Symbol == symbol] != 0) {
      tx.last.date <- Sys.Date()
    }
    
    file.name <- file.path(output.folder, sprintf("%s.csv", symbol))
    if (file.exists(file.name)) {
      # TODO: read last-modified timestamps from existing files for cooldown.
      now <- Sys.time()
      attr(now, "tzone") <- exch.tzone
      now <- as.POSIXlt(now)
      # Alpha Vantage publishes mutual funds NAVs on a T+1 basis.
      now$mday <- now$mday - day.lag
      now <- as.POSIXct(now)
      
      existing.price.dates <- as.Date(read.csv(file.name, stringsAsFactors=FALSE)$timestamp)
      if (length(existing.price.dates) > 0) {
        px.last.date <- max(existing.price.dates)
        px.first.date <- min(existing.price.dates)
        
        px.next.date <- px.last.date + 1
        while (!isBizday(as.timeDate(px.next.date), holidayNYSE())) {
          px.next.date <- px.next.date + 1
        }
        px.next.time <- as.POSIXct(format(px.next.date))
        attr(px.next.time, "tzone") <- exch.tzone
        px.next.time <- as.POSIXlt(px.next.time)
        px.next.time$hour <- exch.close[1]
        px.next.time$min <- exch.close[2]
        px.next.time <- as.POSIXct(px.next.time)
        
        if ((px.next.time > now || px.last.date >= tx.last.date)
            && px.first.date <= tx.first.date) {
          print(paste(Sys.time(), "-", symbol, "- prices are already up to date"))
          next
        }
      }
    }
    
    print(paste(Sys.time(), "-", symbol, "- downloading prices"))
    prices <- read.csv(sprintf(
      "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=%s&apikey=%s&outputsize=full&datatype=csv",
      symbol, alpha.vantage.key))
    existing.price.dates <- as.Date(prices$timestamp)
    prices <- prices[existing.price.dates >= tx.first.date & existing.price.dates <= tx.last.date, ]
    write.csv(prices, file.name, row.names=FALSE)
    
    now <- as.numeric(Sys.time())
    download.times <- tail(c(download.times, now), max(requests.per.minute, requests.per.day))
    download.times <- download.times[download.times >= now - 60 * 60 * 24]
    minute.window <- tail(download.times, requests.per.minute)
    if (length(minute.window) >= requests.per.minute && head(minute.window, 1) - (now - 60) >= 0) {
      Sys.sleep(head(minute.window, 1) - (now - 60))
    }
  }
}
