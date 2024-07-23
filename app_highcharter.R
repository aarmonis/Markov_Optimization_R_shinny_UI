# Load required libraries
library(shiny)
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(highcharter)

# Define the plotting function
plot_efficient_frontier_highcharter <- function(portfolios, efficient_frontier, gmvp, optimum_portfolio, tangent_line, risk_free_rate) {
  highchart() %>%
    hc_add_series(data = portfolios, type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "Portfolios", marker = list(symbol = "circle", fillOpacity = 0.3), color = "gray") %>%
    hc_add_series(data = efficient_frontier, type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "Efficient Frontier", marker = list(symbol = "circle"), color = "blue") %>%
    hc_add_series(data = gmvp, type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "GMVP", marker = list(symbol = "triangle", radius = 5), color = "red") %>%
    hc_add_series(data = optimum_portfolio, type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "Optimum Portfolio", marker = list(symbol = "diamond", radius = 5), color = "green") %>%
    hc_add_series(data = tangent_line, type = "line", hcaes(x = Risk, y = Return), 
                  name = "Tangent Line", color = "purple", dashStyle = "Dash") %>%
    hc_xAxis(title = list(text = "Portfolio Risk (Standard Deviation)"), min = 0, max = max(portfolios$Risk) * 1.1) %>%
    hc_yAxis(title = list(text = "Portfolio Return"), min = min(risk_free_rate, min(portfolios$Return)) * 0.9, 
             max = max(portfolios$Return) * 1.1) %>%
    hc_add_series(data = list(list(x = 0, y = risk_free_rate), list(x = max(portfolios$Risk) * 1.1, y = risk_free_rate)), 
                  type = "line", name = "Risk-free Rate", color = "darkgreen", dashStyle = "Dot") %>%
    hc_legend(enabled = TRUE) %>%
    hc_title(text = "Efficient Frontier")
}

# UI
ui <- fluidPage(
  titlePanel("Portfolio Optimization and Efficient Frontier"),
  sidebarLayout(
    sidebarPanel(
      textInput("tickers", "Enter stock tickers (comma-separated)", "AAPL,GOOGL,MSFT,AMZN"),
      dateRangeInput("dates", "Select date range",
                     start = Sys.Date() - 365*5,
                     end = Sys.Date()),
      numericInput("num_portfolios", "Number of portfolios to simulate", 5000, min = 1000, max = 10000),
      actionButton("run", "Run Optimization"),
      actionButton("stop", "Stop App")
    ),
    mainPanel(
      highchartOutput("efficient_frontier_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to get stock data
  stock_data <- reactive({
    req(input$tickers, input$dates)
    tickers <- strsplit(input$tickers, ",")[[1]]
    getSymbols(tickers, src = "yahoo", 
               from = input$dates[1], 
               to = input$dates[2], 
               auto.assign = TRUE)
    do.call(merge, lapply(tickers, function(x) Ad(get(x))))
  })
  
  # Reactive expression to calculate returns
  returns <- reactive({
    req(stock_data())
    returns <- na.omit(ROC(stock_data()))
    colnames(returns) <- gsub(".Adjusted", "", colnames(returns))
    returns
  })
  
  # Function to generate random portfolios
  generate_portfolios <- function(returns, num_portfolios) {
    set.seed(42)
    n <- ncol(returns)
    
    # Generate random weights
    weights <- matrix(runif(n * num_portfolios), nrow = num_portfolios, ncol = n)
    weights <- weights / rowSums(weights)
    
    # Number of trading days in a year
    trading_days <- 252
    
    # Risk-free rate (assume a small positive number, for example 0.01)
    risk_free_rate <- 0.01
    
    # Calculate annualized portfolio returns and risks
    port_returns <- weights %*% colMeans(returns) * trading_days
    port_risks <- sqrt(diag(weights %*% cov(returns) %*% t(weights))) * sqrt(trading_days)
    
    # Calculate Sharpe Ratio
    sharpe_ratios <- (port_returns - risk_free_rate) / port_risks
    
    portfolios <- data.frame(Return = port_returns, Risk = port_risks, SharpeRatio = sharpe_ratios, weights = I(split(weights, row(weights))))
    
    # Find GMVP
    gmvp <- portfolios[which.min(portfolios$Risk), ]
    
    # Find Optimum Portfolio (Maximum Sharpe Ratio)
    optimum_portfolio <- portfolios[which.max(portfolios$SharpeRatio), ]
    
    # Identify efficient frontier
    efficient_frontier <- portfolios[order(portfolios$Risk), ]
    efficient_frontier <- efficient_frontier[!duplicated(cummax(efficient_frontier$Return)), ]
    
    list(portfolios = portfolios, gmvp = gmvp, optimum_portfolio = optimum_portfolio, efficient_frontier = efficient_frontier, risk_free_rate = risk_free_rate)
  }
  
  # Reactive expression to generate portfolios
  portfolios_data <- eventReactive(input$run, {
    generate_portfolios(returns(), input$num_portfolios)
  })
  
  # Plot efficient frontier
  output$efficient_frontier_plot <- renderHighchart({
    req(portfolios_data())
    
    portfolios <- portfolios_data()$portfolios
    gmvp <- portfolios_data()$gmvp
    optimum_portfolio <- portfolios_data()$optimum_portfolio
    efficient_frontier <- portfolios_data()$efficient_frontier
    risk_free_rate <- portfolios_data()$risk_free_rate
    
    # Calculate the tangent line
    slope <- (optimum_portfolio$Return - risk_free_rate) / optimum_portfolio$Risk
    tangent_line <- data.frame(
      Risk = c(0, max(portfolios$Risk)),
      Return = c(risk_free_rate, risk_free_rate + slope * max(portfolios$Risk))
    )
    
    plot_efficient_frontier_highcharter(portfolios, efficient_frontier, gmvp, optimum_portfolio, tangent_line, risk_free_rate)
  })

  # Stop the app when the stop button is clicked
  observeEvent(input$stop, {
    stopApp()
  })
}

# Run the app
shinyApp(ui, server)
