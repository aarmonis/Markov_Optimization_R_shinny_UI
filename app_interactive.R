# Load required libraries
library(shiny)
library(ggplot2)
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(plotly)

# Define the plotting function
plot_efficient_frontier <- function(portfolios, efficient_frontier, gmvp, optimum_portfolio, tangent_line, risk_free_rate) {
  # Calculate the range of risk values
  risk_range <- range(portfolios$Risk)
  
  # Set the x-axis limits
  x_min <- max(0, risk_range[1] * 0.9)  # 90% of the minimum risk, but not less than 0
  x_max <- risk_range[2] * 1.1  # 110% of the maximum risk
  
  # Set the y-axis limits
  y_min <- min(risk_free_rate, min(portfolios$Return)) * 0.9
  y_max <- max(portfolios$Return) * 1.1
  
  p <- ggplot() +
    geom_point(data = portfolios, aes(x = Risk, y = Return), alpha = 0.3, color = "gray") +
    geom_point(data = efficient_frontier, aes(x = Risk, y = Return), color = "blue", size = 2) +
    geom_point(data = gmvp, aes(x = Risk, y = Return), color = "red", size = 3, shape = 17) +
    geom_point(data = optimum_portfolio, aes(x = Risk, y = Return), color = "green", size = 3, shape = 18) +
    geom_line(data = tangent_line, aes(x = Risk, y = Return), color = "purple", linetype = "dashed") +
    theme_minimal() +
    labs(title = "Efficient Frontier",
         x = "Portfolio Risk (Standard Deviation)",
         y = "Portfolio Return") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(xlim = c(x_min, x_max), 
                    ylim = c(y_min, y_max)) +
    geom_hline(yintercept = risk_free_rate, linetype = "dotted", color = "darkgreen") +
    annotate("text", x = x_min, y = risk_free_rate, label = "Risk-free rate", 
             vjust = -0.5, hjust = 0, color = "darkgreen")
  
  ggplotly(p)
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
      plotlyOutput("efficient_frontier_plot")
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
  output$efficient_frontier_plot <- renderPlotly({
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
    
    plot_efficient_frontier(portfolios, efficient_frontier, gmvp, optimum_portfolio, tangent_line, risk_free_rate)
  })

  # Stop the app when the stop button is clicked
  observeEvent(input$stop, {
    stopApp()
  })
}

# Run the app
shinyApp(ui, server)
