colors <- list(
  blue="#7CB5EC", # "#000000"
  green="#90ED7D", # "#80FFC0"
  red="#F15C80", # "#FF80C0"
  yellow="#E4D354"
)

get.interactive.price.vs.time.plot <- function(
    tx, symbol, price.provider=recent.transaction.price.provider, arearange=FALSE,
    pct.drawdown.major=TRUE) {
  tx <- calc.price.vs.time(tx, symbol)
  
  tx$purchase.prices$Average.Pct.Drawdown <- 100 * (1 - tx$purchase.prices$Average / tx$peak.price)
  tx$purchase.prices$Low.Pct.Drawdown <- 100 * (1 - tx$purchase.prices$Low / tx$peak.price)
  tx$purchase.prices$High.Pct.Drawdown <- 100 * (1 - tx$purchase.prices$High / tx$peak.price)
  tx$sale.prices$Average.Pct.Drawdown <- 100 * (1 - tx$sale.prices$Average / tx$peak.price)
  tx$sale.prices$Low.Pct.Drawdown <- 100 * (1 - tx$sale.prices$Low / tx$peak.price)
  tx$sale.prices$High.Pct.Drawdown <- 100 * (1 - tx$sale.prices$High / tx$peak.price)
  
  hc <- highchart() %>%
    hc_chart(zoomType="xy") %>%
    hc_title(text=paste(symbol, "Price vs. Time")) %>%
    hc_tooltip(shared=TRUE, valueDecimals=2, borderColor=colors$blue) %>%
    hc_xAxis(title=list(text="Date"), type="datetime", gridLineWidth=1)
  
  if (pct.drawdown.major) {
    hc <- hc %>% hc_yAxis_multiples(
      list(title=list(text="Pct.Drawdown"), reversed=TRUE),
      list(
        title=list(text="Price"), opposite=TRUE, linkedTo=0,
        labels=list(formatter=JS(sprintf(
          "function() { return Highcharts.numberFormat((1 - this.value / 100) * %.17g, 2); }",
          tx$peak.price)))))
  } else {
    hc <- hc %>% hc_yAxis_multiples(
      list(title=list(text="Price")),
      list(
        title=list(text="Pct.Drawdown"), opposite=TRUE, linkedTo=0,
        labels=list(formatter=JS(sprintf(
          "function() { return Highcharts.numberFormat(100 * (1 - this.value / %.17g), 2); }",
          tx$peak.price)))))
  }
  
  data.labels <- list(
    "LowPrice", "HighPrice", "LowPctDrawdown", "HighPctDrawdown", "Price", "PctDrawdown")
  data.labels <- setNames(data.labels, data.labels)
  if (pct.drawdown.major)
    data.labels[c("LowPctDrawdown", "HighPctDrawdown", "PctDrawdown")] <- c("low", "high", "y")
  else
    data.labels[c("LowPrice", "HighPrice", "Price")] <- c("low", "high", "y")
  range.tooltip <- list(pointFormat=sprintf(
    "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>${point.%s:0.2f}</b> - <b>${point.%s:0.2f}</b> = {point.%s:0.2f}%% - {point.%s:0.2f}%%<br/>",
    data.labels$LowPrice, data.labels$HighPrice, data.labels$HighPctDrawdown,
    data.labels$LowPctDrawdown))
  point.tooltip <- list(pointFormat=sprintf(
    "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>${point.%s:0.2f}</b> = {point.%s:0.2f}%%<br/>",
    data.labels$Price, data.labels$PctDrawdown))
  
  for (series in list(
      list(name="Purchase", data=tx$purchase.prices, color=colors$red),
      list(name="Sale", data=tx$sale.prices, color=colors$green))) {
    if (nrow(series$data) > 0) {
      range.data <- data.frame(
        x=datetime_to_timestamp(series$data$Date), LowPrice=series$data$Low,
        HighPrice=series$data$High, LowPctDrawdown=series$data$Low.Pct.Drawdown,
        HighPctDrawdown=series$data$High.Pct.Drawdown)
      point.data <- data.frame(
        x=datetime_to_timestamp(series$data$Date), Price=series$data$Average,
        PctDrawdown=series$data$Average.Pct.Drawdown)
      date.bound <- max(as.Date("0001-01-01"), price.provider$available.dates(symbol))
      if (!(date.bound %in% series$data$Date) && date.bound >= min(series$data$Date)) {
        point.data <- rbind(point.data, data.frame(
          x=datetime_to_timestamp(date.bound), Price=NA, PctDrawdown=NA))
      }
      
      colnames(range.data)[match(
        names(data.labels)[names(data.labels) %in% colnames(range.data)],
        colnames(range.data))] <- data.labels[names(data.labels) %in% colnames(range.data)]
      colnames(point.data)[match(
        names(data.labels)[names(data.labels) %in% colnames(point.data)],
        colnames(point.data))] <- data.labels[names(data.labels) %in% colnames(point.data)]
      hc <- hc %>% hc_add_series(
        name=paste(series$name, "intraday range"), data=range.data,
        type=if (arearange) "arearange" else "errorbar", linkedTo=series$name, color=series$color,
        fillOpacity=0.1, tooltip=range.tooltip)
      hc <- hc %>% hc_add_series(
        name=series$name, id=series$name, data=point.data, marker=list(symbol="diamond", radius=2),
        color=series$color, tooltip=point.tooltip)
    }
  }
  
  hc
}

get.interactive.size.vs.price.plot <- function(
    tx, symbol, price.provider=recent.transaction.price.provider, tick.size=0.01) {
  # TODO: pct.drawdown.major == FALSE case.
  # TODO: cum.quantity.major == TRUE case. Optimal cumulative quantity vs. % drawdown function is
  #  symmetric, whereas optimal cumulative cost vs. % drawdown function is skewed such that lower
  #  prices have lower optimal cumulative costs due to the fact that quantity times price is lower.
  size.vs.price <- calc.size.vs.price(tx, symbol, 0, FALSE)[, c(
    "Pct.Drawdown", "Cost", "Cum.Cost", "Quantity")]
  size.vs.price <- size.vs.price[size.vs.price$Quantity != 0, ]
  size.vs.price$Price <- size.vs.price$Cost / size.vs.price$Quantity
  peak.price <- sum(size.vs.price$Price) / sum(1 - size.vs.price$Pct.Drawdown / 100)
  unit.value <- get.current.price(tx, symbol, price.provider)
  current.pct.drawdown <- 100 * (1 - unit.value / peak.price)
  unit.cost <- sum(tx$Cost[tx$Symbol == symbol]) / sum(tx$Quantity[tx$Symbol == symbol])
  average.pct.drawdown <- 100 * (1 - unit.cost / peak.price)
  
  size.vs.price$Raw.Price <- size.vs.price$Price
  # Round fractions of cents down for sales and up for purchases. The idea is that if we want to
  #  offset the sale, we should buy at the nearest lower price increment and if we want to offset
  #  the purchase we should sell at the nearest higher price increment. We need to round off the
  #  price before applying the floor and ceiling in order to smooth out floating point rounding
  #  errors on base 10 fractions, e.g. ceiling(17.35 * 100) / 100 == 17.36 while
  #  ceiling(round(17.35 * 100, 4)) / 100 == 17.35 and floor(17.4 * 100) / 100 == 17.39 while
  #  floor(round(17.4 * 100, 4)) / 100 == 17.4.
  # Using the reciprocal of the tick.size results in less rounding error since 100 is exactly
  #  representable in binary floating point but 0.01 is not.
  # There is no counterpart to trunc for rounding away from 0, so roll our own implementation.
  size.vs.price$Price <- ifelse(
    size.vs.price$Quantity < 0,
    floor(round(size.vs.price$Price * (1 / tick.size), 4)) / (1 / tick.size),
    ceiling(round(size.vs.price$Price * (1 / tick.size), 4)) / (1 / tick.size))
  # Double check that we didn't round any already tick-aligned price or a price to the wrong direction.
  round.off.range <- round(
    range(c(
      (size.vs.price$Price - size.vs.price$Raw.Price)[size.vs.price$Quantity > 0],
      (size.vs.price$Raw.Price - size.vs.price$Price)[size.vs.price$Quantity < 0])),
    4)
  stopifnot(round.off.range[1] >= 0 && round.off.range[2] < tick.size)
  size.vs.price <- reduce.on.factor(size.vs.price, "Price", function(for.price)
    data.frame(
      Cost=sum(for.price$Cost),
      Cum.Cost=tail(for.price$Cum.Cost[order(for.price$Pct.Drawdown)], 1),
      Quantity=round(sum(for.price$Quantity), QTY_FRAC_DIGITS)))
  size.vs.price$Pct.Drawdown <- 100 * (1 - size.vs.price$Price / peak.price)
  size.vs.price <- size.vs.price[order(size.vs.price$Pct.Drawdown), ]
  
  data <- data.frame(
    x=size.vs.price$Pct.Drawdown, y=size.vs.price$Cum.Cost, Price=size.vs.price$Price,
    Quantity=round(size.vs.price$Quantity, QTY_FRAC_DIGITS))
  data <- rbind(data.frame(x=-1, y=0, Price=peak.price, Quantity=0), data)
  data <- cbind(data, CumQuantity=round(cumsum(data$Quantity), QTY_FRAC_DIGITS))
  data <- cbind(data, Prev=c(0, head(data$y, -1)), PrevCumQuantity=c(0, head(data$CumQuantity, -1)))
  
  hc <- highchart() %>%
    hc_chart(zoomType="xy") %>%
    hc_title(text=paste(symbol, "Size vs. Price")) %>%
    hc_tooltip(
      shared=TRUE,
      headerFormat="<span style=\"font-size: 10px\">Drawdown: {point.x:0.2f}%</span><br/>",
      borderColor=colors$blue) %>%
    hc_xAxis(title=list(text="Pct.Drawdown"), gridLineWidth=1) %>%
    hc_yAxis_multiples(
      list(title=list(text="Cost")), list(title=list(text="Cost"), opposite=TRUE, linkedTo=0))
  
  hc <- hc %>%
    hc_add_series(
      name="Cum.Cost",
      step="left",
      data=data,
      marker=list(symbol="diamond", radius=2),
      tooltip=list(pointFormat=paste(
        "<span style=\"color:{point.color}\">\u25CF</span> {series.name} ($): <b>{point.y:0.4f}</b> = {point.Quantity} \u00D7 ${point.Price:0.2f} + {point.Prev:0.4f}",
        "<span style=\"color:{point.color}\">\u25CF</span> Cum.Quantity (shares): <b>{point.CumQuantity}</b> = {point.Quantity} + {point.PrevCumQuantity}",
        "", sep="<br/>")),
      color=colors$blue)
  
  interp.cum.cost.at.price <- stepfun(
    x=size.vs.price$Pct.Drawdown, y=c(0, size.vs.price$Cum.Cost), right=FALSE, f=0)
  for (series in list(
      list(name="Unit.Cost", price=unit.cost, pct.drawdown=average.pct.drawdown, color=colors$red),
      list(
        name="Unit.Value", price=unit.value, pct.drawdown=current.pct.drawdown,
        color=colors$green))) {
    hc <- hc %>%
      hc_add_series(
        name=series$name,
        custom=series$price,
        data=data.frame(
          x=rep(series$pct.drawdown, 3),
          y=c(interp.cum.cost.at.price(series$pct.drawdown), range(data$y))),
        dashStyle="Dash",
        tooltip=list(
          pointFormat="<span style=\"color:{point.color}\">\u25CF</span> {series.name}: <b>${series.options.custom:0.4f}</b><br/>"),
        color=series$color,
        visible=series$price >= min(data$Price) - 0.01 && series$price <= max(data$Price) + 0.01)
  }
  
  hc
}

get.interactive.size.vs.time.plot <- function(
    tx, symbol, price.provider=recent.transaction.price.provider, name=symbol, arearange=FALSE,
    hide.initially=TRUE) {
  # Size vs. time plots are the densest charts since they plot an observation per business day
  #  rather than an observation per transaction.
  # All series are hidden initially to cut down on browser rendering time on page load.
  # Users are expected to manually toggle the size vs. time plots they're interested in.
  if (length(symbol) == 1) {
    costs <- calc.size.vs.time(tx, symbol, price.provider)
  } else if (length(symbol) == 0) {
    costs <- data.frame(Date=as.Date(character()))
  } else {
    costs <- calc.portfolio.size.vs.time(tx, symbol, price.provider)
  }
  costs$Gain <- costs$Value - costs$Cost
  costs$Cum.Quantity <- round(cumsum(costs$Quantity), QTY_FRAC_DIGITS)
  
  hc <- highchart() %>%
    hc_plotOptions(
      series=list(
        events=list(
          legendItemClick=JS("
            function() {
              if (this.name == 'Show all' || this.name == 'Hide all') {
                this.chart.series.forEach(
                  series => this.chart.showHideFlag ? series.hide() : series.show());
                this.chart.showHideFlag = !this.chart.showHideFlag;
                this.chart.series[this.index].update({name: this.chart.showHideFlag ? 'Hide all' : 'Show all'})
              }
            }")
        )
      )
    ) %>%
    hc_chart(
      animation=FALSE,
      zoomType="xy",
      events=list(load=JS("function() { this.showHideFlag = false; }"))
    ) %>%
    hc_title(text=paste(name, "Size vs. Time")) %>%
    hc_tooltip(shared=TRUE, valueDecimals=2, borderColor=colors$blue) %>%
    hc_xAxis(title=list(text="Date"), type="datetime", gridLineWidth=1) %>%
    hc_yAxis_multiples(
      list(title=list(text="Position Size")), list(title=list(text="Gain"), opposite=TRUE))
  
  hc <- hc %>%
    hc_add_series(
      name="Show all",
      visible=FALSE,
      marker=list(enabled=FALSE),
      lineWidth=0
    )
  
  hc <- hc %>%
    hc_add_series(
      name="Gain",
      id="PNL",
      data=data.frame(
        x=datetime_to_timestamp(costs$Date),
        y=costs$Value - costs$Cost,
        DayOverDayGain=if (nrow(costs) == 0) c() else c(0, diff(costs$Value - costs$Cost))),
      marker=list(symbol="diamond", radius=2),
      color=colors$yellow,
      type="area",
      dashStyle="Dash",
      fillColor=list(
        linearGradient=list(x1=0, y1=0, x2=0, y2=1),
        stops=list(
          list(0, colors$yellow),
          list(1, "#F4F4F4"))),
      yAxis=1,
      tooltip=list(pointFormat=paste(
        "<span style=\"color:{point.color}\">\u25CF</span> {series.name} ($): <b>{point.y:0.2f}</b>",
        "<span style=\"color:{point.color}\">\u25CF</span> Day over day gain ($): <b>{point.DayOverDayGain:0.2f}</b>",
        "",
        sep="<br/>")),
      visible=!hide.initially)
  
  hc <- hc %>%
    hc_add_series(
      name="Cost",
      custom=colors$blue,
      data=data.frame(
        x=datetime_to_timestamp(costs$Date),
        y=costs$Cost,
        Quantity=costs$Cum.Quantity,
        UnitCost=costs$Cost / costs$Cum.Quantity,
        Gain=costs$Gain),
      marker=list(symbol="diamond", radius=2),
      color=colors$red,
      tooltip=list(pointFormat=paste(
        "<span style=\"color:{series.options.custom}\">\u25CF</span> Quantity (shares): <b>{point.Quantity}</b>",
        "<span style=\"color:{point.color}\">\u25CF</span> {series.name} ($): <b>{point.y}</b>",
        "<span style=\"color:{point.color}\">\u25CF</span> Unit cost ($): <b>{point.UnitCost:0.2f}</b>",
        "", sep="<br/>")),
      visible=!hide.initially)
  
  hc <- hc %>%
    hc_add_series(
      name="Value intraday range",
      data=data.frame(
        x=datetime_to_timestamp(costs$Date),
        low=costs$Low.Value,
        high=costs$High.Value),
      type=if (arearange) "arearange" else "errorbar",
      linkedTo="Value",
      color=colors$green,
      fillOpacity=0.1,
      tooltip=list(pointFormat=paste(
        "<span style=\"color:{point.color}\">\u25CF</span> {series.name} ($): <b>{point.low}</b> - <b>{point.high}</b>",
        "", sep="<br/>")),
      visible=!hide.initially)
  
  hc <- hc %>%
    hc_add_series(
      name="Value",
      id="Value",
      data=data.frame(
        x=datetime_to_timestamp(costs$Date),
        y=costs$Value,
        Price=costs$Average.Price),
      marker=list(symbol="diamond", radius=2),
      color=colors$green,
      tooltip=list(pointFormat=paste(
        "<span style=\"color:{point.color}\">\u25CF</span> {series.name} ($): <b>{point.y}</b>",
        "<span style=\"color:{point.color}\">\u25CF</span> Unit value ($): <b>{point.Price:0.2f}</b>",
        "",
        sep="<br/>")),
      visible=!hide.initially)
  
  hc
}

set.full.page.sizing.policy <- function(widget) {
  widget$sizingPolicy$browser$padding <- 0
  widget$sizingPolicy$browser$fill <- FALSE
  widget$sizingPolicy$browser$defaultHeight <- "100%"
  widget$sizingPolicy$browser$defaultWidth <- "100%"
  widget
}

get.interactive.plots.for.symbol <- function(
    tx, symbol, price.provider=recent.transaction.price.provider) {
  symbol.plot <- combineWidgets(
    get.interactive.price.vs.time.plot(tx, symbol, price.provider),
    get.interactive.size.vs.price.plot(tx, symbol, price.provider),
    get.interactive.size.vs.time.plot(tx, symbol, price.provider))
  
  symbol.plot <- set.full.page.sizing.policy(symbol.plot)
  symbol.plot$elementId <- paste(symbol, "plots", sep="-")
  
  symbol.plot
}

plot.interactive.for.symbol <- function(
    tx, symbol, price.provider=recent.transaction.price.provider) {
  symbol.plot <- get.interactive.plots.for.symbol(tx, symbol, price.provider)
  print(symbol.plot)
}

plot.interactive <- function(
    tx, interesting.symbols, price.provider=recent.transaction.price.provider,
    output.path=file.path("output", "PortViz.html")) {
  is.interesting.option <- grepl(" options$", interesting.symbols)
  interesting.stocks <- interesting.symbols[!is.interesting.option]
  
  symbol.plots <- lapply(interesting.stocks, function(symbol)
    get.interactive.plots.for.symbol(tx, symbol, price.provider))
  
  interesting.options <- interesting.symbols[is.interesting.option]
  interesting.options <- substr(
    interesting.options, 1, nchar(interesting.options) - nchar(" options"))
  interesting.options <- setNames(
    lapply(interesting.options, function(options.root) unique(tx$Symbol[
      nchar(tx$Symbol) == 21 & sub("\\s+$", "", substr(tx$Symbol, 1, 6)) == options.root])),
    interesting.options)
  
  for (options.root in names(interesting.options)) {
    options.for.root <- interesting.options[[options.root]]
    calls.for.root <- options.for.root[substr(options.for.root, 13, 13) == "C"]
    puts.for.root <- options.for.root[substr(options.for.root, 13, 13) == "P"]
    options.plot <- combineWidgets(
      get.interactive.size.vs.time.plot(
        tx, calls.for.root, price.provider, paste(options.root, "calls")),
      get.interactive.size.vs.time.plot(
        tx, options.for.root, price.provider, paste(options.root, "options")),
      get.interactive.size.vs.time.plot(
        tx, puts.for.root, price.provider, paste(options.root, "puts")))
    
    options.plot <- set.full.page.sizing.policy(options.plot)
    options.plot$elementId <- paste(options.root, "options-plots", sep="-")
    symbol.plots <- c(symbol.plots, list(options.plot))
  }
  
  interesting.symbols <- c("$", interesting.stocks, do.call(c, interesting.options))
  miscellaneous.symbols <- unique(tx$Symbol[!(tx$Symbol %in% interesting.symbols)])
  if (length(miscellaneous.symbols) > 0) {
    miscellaneous.plot <- get.interactive.size.vs.time.plot(
      tx, miscellaneous.symbols, price.provider, "Miscellaneous")
    miscellaneous.plot <- set.full.page.sizing.policy(miscellaneous.plot)
    miscellaneous.plot$elementId <- "miscellaneous-plot"
    symbol.plots <- c(symbol.plots, list(miscellaneous.plot))
  }
  
  if ("$" %in% tx$Symbol) {
    ex.cash.symbols <- unique(tx$Symbol[tx$Symbol != "$"])
    portfolio.ex.cash.plot <- get.interactive.size.vs.time.plot(
      tx, ex.cash.symbols, price.provider, "Portfolio Ex-cash")
    portfolio.ex.cash.plot <- set.full.page.sizing.policy(portfolio.ex.cash.plot)
    portfolio.ex.cash.plot$elementId <- "portfolio-ex-cash-plot"
    symbol.plots <- c(symbol.plots, list(portfolio.ex.cash.plot))
  }
  
  portfolio.plot <- get.interactive.size.vs.time.plot(
    tx, unique(tx$Symbol), price.provider, "Portfolio")
  portfolio.plot <- set.full.page.sizing.policy(portfolio.plot)
  portfolio.plot$elementId <- "portfolio-plot"
  symbol.plots <- c(symbol.plots, list(portfolio.plot))
  
  formatted.tx <- aggregate.purchases.and.sales.with.day.over.day.gain(tx, price.provider)
  formatted.tx$Symbol <- gsub(" ", "&nbsp;", formatted.tx$Symbol)
  formatted.tx$Cost.Pct.Drawdown <- formatted.tx$Cost.Pct.Drawdown / 100
  formatted.tx$Value.Pct.Drawdown <- formatted.tx$Value.Pct.Drawdown / 100
  portfolio.summary <- datatable(
    formatted.tx,
    options=list(pageLength=20, scrollCollapse=TRUE, paging=FALSE, scrollY=100, scrollResize=TRUE),
    plugins=c("scrollResize"),
    rownames=FALSE,
    escape=FALSE)
  portfolio.summary <- portfolio.summary %>% formatCurrency(
    c(
      "Unit.Cost", "Cost", "Peak.Price", "Full.Rebound.Gain", "Unit.Value", "Value", "Gain",
      "Day.Over.Day.Gain"),
    digits=4)
  portfolio.summary <- portfolio.summary %>% formatPercentage(
    c("Cost.Pct.Drawdown", "Value.Pct.Drawdown"), 2)
  portfolio.summary <- set.full.page.sizing.policy(portfolio.summary)
  portfolio.summary$elementId <- "portfolio-summary"
  symbol.plots <- c(list(portfolio.summary), symbol.plots)
  
  hc <- combineWidgets(list=symbol.plots, ncol=1)
  
  hc$dependencies <- c(hc$dependencies, list(htmlDependency(
    name="onepage-scroll",
    version="1.3.1",
    src=c(href=file.path(getwd(), "resources")), # c(href="https://cdnjs.cloudflare.com/ajax/libs/onepage-scroll/1.3.1/")
    script="jquery.onepage-scroll.js",
    stylesheet="onepage-scroll.css")))
  
  hc$sizingPolicy$browser$padding <- 0
  hc$sizingPolicy$browser$fill <- FALSE
  hc$sizingPolicy$browser$defaultHeight <- paste(length(symbol.plots) * 100, "%", sep="")
  hc$sizingPolicy$browser$defaultWidth <- "100%"
  
  styler <- paste(
    as.character(outer(
      c("document.documentElement", "document.body", "document.getElementById(\"htmlwidget_container\")"),
      c(".style.width=\"100%\"", ".style.height=\"100%\""),
      paste,
      sep="")),
    collapse=";")
  hc <- prependContent(hc, tags$script(HTML(paste("(function(){", styler, "})()", sep=""))))
  hc <- prependContent(hc, onStaticRenderComplete(
    "$(\"#htmlwidget_container\").addClass(\"main\").onepage_scroll({sectionContainer: \"#htmlwidget_container > .combineWidgets > .cw-container > .cw-subcontainer > .cw-content > .cw-row\", loop: false, animationTime: 250, quietPeriod: -125})"))
  
  # There is a weird bug when using datatable with combineWidgets. The
  #  window.HTMLWidgets.staticRender function calls the initialize function for the datatables
  #  HTMLWidgets.widget twice since combineWidgets first calls initialize on the datatables widget
  #  and then staticRender calls initialize again on the same widget. The problem is that the
  #  renderValue function for the datatables widget is not called after the second call to
  #  initialize. The initialize function for the datatables widget wipes out the HTML for
  #  #portfolio-summary, so the div will be left empty unless renderValue is called again. The
  #  staticRender function will skip over any widgets with the class 'html-widget-static-bound', so
  #  have combineWidgets add that class when it is done to prevent staticRender from iterating over
  #  the datatables widget and calling initialize a second time.
  hc <- htmlwidgets::onRender(
    hc,
    "function(el, x) { $(\".datatables.cw-widget\").addClass(\"html-widget-static-bound\") }")
  
  # Specify selfcontained=FALSE for easier debugging of JavaScript files.
  prev.wd <- getwd()
  tryCatch({
    setwd(dirname(output.path))
    saveWidget(hc, basename(output.path))
  }, finally={
    setwd(prev.wd)
  })
}
