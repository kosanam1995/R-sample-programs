library(shiny)
library(tidyverse)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(ggplot2)
library(GA)

ui <- fluidPage(sidebarPanel(
  fluidRow(
    column(7,textInput("stock1", "Stock 1", "GE")),
    column(5,numericInput("w1", "Portf. %", 25, min = 1, max = 100))),
  fluidRow(
    column(7,textInput("stock2", "Stock 2", "AMD")),
    column(5,numericInput("w2", "Portf. %", 25, min = 1, max = 100))),
  fluidRow(
    column(7,textInput("stock3", "Stock 3", "BAC")),
    column(5,numericInput("w3", "Portf. %", 20, min = 1, max = 100))),
  fluidRow(
    column(7,textInput("stock4", "Stock 4", "QQQ")),
    column(5,numericInput("w4", "Portf. %", 20, min = 1, max = 100))),
  fluidRow(
    column(7,textInput("stock5", "Stock 5", "NBR")),
    column(5,numericInput("w5", "Portf. %", 10, min = 1, max = 100))),
  fluidRow(
    column(6,dateInput("date", "Starting Date", value = "2010-01-01",format="yyyy-mm-dd"))),
  fluidRow(
     column(5,numericInput("mar", "MAR%", 0.8, min = 0, max = 3, step = .01)),
    column(5
           ,numericInput("window", "Window", 6, min = 3, max = 20, step = 1))),
   fluidRow(
     actionButton("go", "Submit"))),
  
  mainPanel(
    tabsetPanel(
     tabPanel("risk_plot",h5("Risk plot for all Stocks"), plotOutput('plot1'))),
     tabsetPanel(
     tabPanel("hist",h5("histogram of all Stocks"),plotOutput('plot2')),
     tabPanel("Density",h5("Density of all Stocks"),plotOutput('plot3')),
     tabPanel("ga", h5("Predected percentage of optimal investment for stock-1,stock-2,stock-3,stock-4,stock-5"), textOutput("ga"))
    
  )))


server<- function(input, output) {
  
  prices <- eventReactive(input$go, {
    
   symbols <- c(input$stock1,input$stock2,input$stock3,input$stock4,input$stock5)
   
   getSymbols(symbols, src = 'yahoo', from = "2012-12-31",
               auto.assign = TRUE, warnings = FALSE) %>%
      map(~Ad(get(.))) %>%
      reduce(merge) %>%
      `colnames<-`(symbols)
  
  })
  
  mar<- eventReactive(input$go,{
  input$mar
  })
  
  rolling_sortino <- eventReactive(input$go, {
    validate(need(input$w1 + input$w2 + input$w3 + input$w4 + input$w5 == 100))
    
    prices <- prices()
    w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100, input$w5/100)
    MAR <- mar()
    window <- input$window
    prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
    asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))
    
  portfolio_returns_xts_rebalanced_monthly <- Return.portfolio(asset_returns_xts, weights = w, rebalance_on = "months",weights=weights)

  rolling_sortino <-
    rollapply(portfolio_returns_xts_rebalanced_monthly, window,
            function(x) SortinoRatio(x, MAR = MAR)) %>%
    `colnames<-`(paste(window, "-month rolling Sortino", sep=""))
  })
  
  portfolio_returns_tq_rebalanced_monthly <- eventReactive(input$go, {
    validate(need(input$w1 + input$w2 + input$w3 + input$w4 + input$w5 == 100,
                "100%!"))

prices <- prices()
w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100, input$w5/100)

asset_returns_long <-
      prices %>%
      to.monthly(indexAt = "last", OHLC = FALSE) %>%
      tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
      gather(asset, returns, -date) %>%
      group_by(asset) %>%
      mutate(returns = (log(returns) - log(lag(returns))))

MAR <- mar()

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months") %>%
  mutate(returns_below_MAR = ifelse(returns < MAR, returns, NA)) %>%
  mutate(returns_above_MAR = ifelse(returns > MAR, returns, NA))

})
  output$plot1<- 
    renderPlot({
      
      validate(need(input$go != 0,
                    "Please choose your portfolio assets, weights, MAR, rolling window and start date and click submit."))
      
      MAR <- mar()
      portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()
      
      print( portfolio_returns_tq_rebalanced_monthly %>%
               ggplot(aes(x = date)) +
               geom_point(aes(y = returns_below_MAR), colour = "red") +
               geom_point(aes(y = as.single(returns_above_MAR)), colour = "green") + 
               geom_vline(xintercept = as.numeric(as.Date("2016-11-30")), color = "blue") +
               geom_hline(yintercept = MAR, color = "purple", linetype = "dotted") +
               annotate(geom="text", x=as.Date("2016-11-30"), 
                        y = -.15, label = "Election", fontface = "plain", 
                        angle = 90, alpha = .5, vjust =  1.5) +
               ylab("percent monthly returns")
             
      )})
  

output$plot2<-
renderPlot({
  validate(need(input$go != 0, "Please choose your portfolio assets, weights, MAR, rolling window and start date and click submit."))
  MAR <- mar()
  portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()

  portfolio_returns_tq_rebalanced_monthly %>%
    ggplot(aes(x = returns)) +
    geom_histogram(alpha = 0.25, binwidth = .01, fill = "cornflowerblue") +
    geom_vline(xintercept = MAR, color = "green") +
    annotate(geom = "text", x = MAR,
             y = 10, label = "MAR", fontface = "plain",
             angle = 90, alpha = .5, vjust =  1)
})

output$plot3<-
renderPlot({
  validate(need(input$go != 0, "Please choose your portfolio assets, weights, MAR, rolling window and start date and click submit."))
  MAR <- mar()
  portfolio_returns_tq_rebalanced_monthly <- portfolio_returns_tq_rebalanced_monthly()

  sortino_density_plot <- portfolio_returns_tq_rebalanced_monthly %>%
    ggplot(aes(x = returns)) +
    stat_density(geom = "line", size = 1, color = "cornflowerblue")

  shaded_area_data <- ggplot_build(sortino_density_plot)$data[[1]] %>%
    filter(x < MAR)

  sortino_density_plot +
  geom_area(data = shaded_area_data, aes(x = x, y = y), fill="pink", alpha = 0.5) +
  geom_segment(data = shaded_area_data, aes(x = MAR, y = 0, xend = MAR, yend = y),
               color = "red", linetype = "dotted") +
  annotate(geom = "text", x = MAR, y = 5, label = paste("MAR =", MAR, sep = ""),
           fontface = "plain", angle = 90, alpha = .8, vjust =  -1) +
  annotate(geom = "text", x = (MAR - .02), y = .1, label = "Downside",
           fontface = "plain", alpha = .8, vjust =  -1)

})

output$ga<- 
renderText({
  P <- NULL
  for(ticker in symbols) {
    tmp <- Cl(to.monthly(eval(parse(text = ticker))))
    P <- cbind(P, tmp)
  }
  colnames(P) <- symbols
  R <- diff(log(P))
  R <- R[-1,]
  mu <- colMeans(R)
  sigma <- cov(R)
  pContribCVaR <- ES(weights = rep(0.2, 5), method = "gaussian", portfolio_method = "component", mu = mu, sigma = sigma)$pct_contrib_ES
  obj <- function(w) {
    fn.call <<- fn.call + 1
    if (sum(w) == 0) { w <- w + 1e-2 }
    w <- w / sum(w)
    CVaR <- ES(weights = w, method = "gaussian", portfolio_method = "component", mu = mu, sigma = sigma)
    tmp1 <- CVaR$ES
    tmp2 <- max(CVaR$pct_contrib_ES - 0.225, 0)
    out <- tmp1 - 1e+3 * tmp2
    return(out)
  }
  
  
  

  set.seed(1234)
  fn.call <<- 0
  gap <- ga(type = "real-valued",fitness=obj,lower=rep(0,5),upper=rep(1,5), popSize = 50,maxiter = 30, pcrossover = 0.75, pmutation = 0.1)
  nsol <- gap@solution
  nsol <- nsol / sum(nsol)
  fn.call.gap <- fn.call
  
  nsol
  


})
  
}
  
  shinyApp(ui = ui, server = server) 
