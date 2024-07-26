# Load required libraries
library(shiny)
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(highcharter)
library(shinydashboard)
library(DT)

plot_efficient_frontier_highcharter <- function(portfolios, efficient_frontier, gmvp, optimum_portfolio, tangent_line, risk_free_rate) {
  # Define axis limits
  gmvp_risk <- gmvp$Risk
  max_risk <- max(portfolios$Risk) * 1.1
  x_min <- floor(gmvp_risk * 10) / 10  # Closest risk divisible by 0.1 below GMVP risk
  
  # Create a flag to differentiate efficient frontier points
  portfolios$is_efficient <- portfolios$Risk %in% efficient_frontier$Risk
  
  highchart() %>%
    hc_add_series(data = portfolios[portfolios$is_efficient == FALSE, ], type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "Portfolios", marker = list(symbol = "circle", radius = 2, fillOpacity = 0.3), color = "gray") %>%
    hc_add_series(data = efficient_frontier, type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "Efficient Frontier", marker = list(symbol = "circle", radius = 5), color = "blue") %>%
    hc_add_series(data = gmvp, type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "GMVP", marker = list(symbol = "triangle", radius = 7), color = "red") %>%
    hc_add_series(data = optimum_portfolio, type = "scatter", hcaes(x = Risk, y = Return), 
                  name = "Optimum Portfolio", marker = list(symbol = "diamond", radius = 7), color = "green") %>%
    hc_add_series(data = tangent_line, type = "line", hcaes(x = Risk, y = Return), 
                  name = "Tangent Line", color = "purple", dashStyle = "Dash") %>%
    hc_xAxis(title = list(text = "Portfolio Risk (Standard Deviation)"), 
             gridLineWidth = 1, 
             min = x_min, 
             max = max_risk,
             startOnTick = TRUE, 
             endOnTick = TRUE) %>%
    hc_yAxis(title = list(text = "Portfolio Return"), 
             gridLineWidth = 1, 
             min = 0, 
             max = max(portfolios$Return) * 1.1,
             startOnTick = TRUE, 
             endOnTick = TRUE) %>%
    hc_add_series(data = list(list(x = 0, y = risk_free_rate), list(x = max_risk, y = risk_free_rate)), 
                  type = "line", name = "Risk-free Rate", color = "darkgreen", dashStyle = "Dot") %>%
    hc_legend(enabled = TRUE) %>%
    hc_title(text = "Efficient Frontier")
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Optimization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Efficient Frontier", tabName = "frontier", icon = icon("chart-line")),
      menuItem("Portfolio Weights", tabName = "weights", icon = icon("balance-scale")),
      menuItem("Input Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "frontier",
        fluidRow(
          box(
            title = "Input Parameters", status = "primary", solidHeader = TRUE,
            textInput("tickers", "Enter stock tickers (comma-separated)", "AAPL,GOOGL,MSFT,AMZN"),
            dateRangeInput("dates", "Select date range",
                           start = Sys.Date() - 365*5,
                           end = Sys.Date()),
            numericInput("num_portfolios", "Number of portfolios to simulate", 5000, min = 1000, max = 10000),
            numericInput("risk_free_rate", "Risk-free rate (%)", 1, min = 0, max = 10, step = 0.1),
            actionButton("run", "Run Optimization", class = "btn-success"),
            width = 4
          ),
          box(
            title = "Efficient Frontier", status = "primary", solidHeader = TRUE,
            highchartOutput("efficient_frontier_plot"),
            width = 8
          )
        )
      ),
      tabItem(tabName = "weights",
        fluidRow(
          box(
            title = "Optimal Portfolio Weights", status = "info", solidHeader = TRUE,
            DTOutput("weights_table"),
            verbatimTextOutput("optimum_metrics"),
            width = 12
          ),
          box(
            title = "GMVP Weights", status = "info", solidHeader = TRUE,
            DTOutput("gmvp_weights_table"),
            verbatimTextOutput("gmvp_metrics"),
            width = 12
          )
        )
      ),
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Stock Returns", status = "warning", solidHeader = TRUE,
            DTOutput("returns_table"),
            width = 12
          )
        )
      )
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
  generate_portfolios <- function(returns, num_portfolios, risk_free_rate) {
    set.seed(42)
    n <- ncol(returns)
    
    # Generate random weights
    weights <- matrix(runif(n * num_portfolios), nrow = num_portfolios, ncol = n)
    weights <- weights / rowSums(weights)
    
    # Number of trading days in a year
    trading_days <- 252
    
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
    generate_portfolios(returns(), input$num_portfolios, input$risk_free_rate / 100)
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

  # Render optimal portfolio weights table
  output$weights_table <- renderDT({
    req(portfolios_data())
    optimum_portfolio <- portfolios_data()$optimum_portfolio
    weights <- data.frame(
      Stock = names(returns()),
      Weight = round(optimum_portfolio$weights[[1]] * 100, 2)
    )
    datatable(weights, options = list(pageLength = 10))
  })

  # Render GMVP weights table
  output$gmvp_weights_table <- renderDT({
    req(portfolios_data())
    gmvp <- portfolios_data()$gmvp
    weights <- data.frame(
      Stock = names(returns()),
      Weight = round(gmvp$weights[[1]] * 100, 2)
    )
    datatable(weights, options = list(pageLength = 10))
  })
  
  # Render optimum portfolio metrics
  output$optimum_metrics <- renderPrint({
    req(portfolios_data())
    optimum_portfolio <- portfolios_data()$optimum_portfolio
    cat("Optimum Portfolio Metrics:\n")
    cat("Return: ", round(optimum_portfolio$Return, 4), "\n")
    cat("Risk: ", round(optimum_portfolio$Risk, 4), "\n")
    cat("Sharpe Ratio: ", round(optimum_portfolio$SharpeRatio, 4), "\n")
  })
  
  # Render GMVP metrics
  output$gmvp_metrics <- renderPrint({
    req(portfolios_data())
    gmvp <- portfolios_data()$gmvp
    cat("GMVP Metrics:\n")
    cat("Return: ", round(gmvp$Return, 4), "\n")
    cat("Risk: ", round(gmvp$Risk, 4), "\n")
    cat("Sharpe Ratio: ", round(gmvp$SharpeRatio, 4), "\n")
  })

  # Render returns table
  output$returns_table <- renderDT({
    req(returns())
    returns_data <- as.data.frame(returns())
    returns_data$Date <- index(returns())
    returns_data <- returns_data[, c(ncol(returns_data), 1:(ncol(returns_data)-1))]
    datatable(returns_data, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run the app
shinyApp(ui, server)
