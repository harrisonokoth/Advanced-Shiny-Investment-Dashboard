# Advanced Shiny Investment Dashboard
# WealthGrow Pro Analytics Platform

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Sample investment data generation
set.seed(123)
generate_sample_data <- function() {
  dates <- seq.Date(from = as.Date("2023-01-01"), to = as.Date("2024-12-31"), by = "month")
  
  data.frame(
    date = dates,
    principal = cumsum(c(10000, round(runif(length(dates)-1, 1000, 5000)))),
    interest_rate = 0.012, # 1.2% monthly
    interest_earned = round(cumsum(c(0, runif(length(dates)-1, 50, 200)))),
    withdrawals = c(0, 0, 500, 0, 0, 0, 1000, 0, 0, 0, 500, 0, 0, 0, 750, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    investment_type = sample(c("Fixed Deposit", "Growth Fund", "Premium Bonds"), 
                             length(dates), replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    risk_level = sample(c("Low", "Medium", "High"), length(dates), replace = TRUE, prob = c(0.5, 0.3, 0.2))
  ) %>%
    mutate(
      total_balance = principal + interest_earned - withdrawals,
      net_growth = interest_earned - withdrawals,
      month = format(date, "%b %Y")
    )
}

investment_data <- generate_sample_data()

# Custom CSS for professional styling
custom_css <- "
/* Custom Dashboard Styling */
.skin-blue .main-header .logo {
  background-color: #2c3e50;
  font-weight: bold;
}

.skin-blue .main-header .navbar {
  background-color: #34495e;
}

.content-wrapper, .right-side {
  background-color: #ecf0f5;
}

/* Value Box Styling */
.small-box {
  border-radius: 10px;
  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
  transition: transform 0.3s ease;
}

.small-box:hover {
  transform: translateY(-5px);
}

/* Custom Colors */
.bg-blue { background-color: #3498db !important; }
.bg-green { background-color: #2ecc71 !important; }
.bg-orange { background-color: #e67e22 !important; }
.bg-purple { background-color: #9b59b6 !important; }

/* Plot Styling */
.plot-container {
  background: white;
  border-radius: 10px;
  padding: 15px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
"

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fa fa-chart-line"), 
      "WealthGrow Pro Analytics"
    ),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications",
      badgeStatus = "warning",
      notificationItem(
        text = "New investment opportunity available",
        icon = icon("bullhorn")
      ),
      notificationItem(
        text = "Your portfolio gained 1.2% this month",
        icon = icon("chart-line"),
        status = "success"
      )
    )
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Portfolio Analytics", tabName = "analytics", icon = icon("chart-bar")),
      menuItem("Investment Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Transaction History", tabName = "transactions", icon = icon("history")),
      menuItem("Performance Reports", tabName = "reports", icon = icon("file-alt")),
      
      # Advanced Filters
      br(),
      div(style = "padding: 15px;",
          h4("Investment Filters", style = "color: #2c3e50;"),
          dateRangeInput("date_range", "Date Range:",
                         start = min(investment_data$date),
                         end = max(investment_data$date)),
          selectInput("investment_type", "Investment Type:",
                      choices = c("All", unique(investment_data$investment_type)),
                      selected = "All"),
          selectInput("risk_level", "Risk Level:",
                      choices = c("All", unique(investment_data$risk_level)),
                      selected = "All"),
          sliderInput("balance_range", "Balance Range (KES):",
                      min = 0, max = max(investment_data$total_balance) * 1.1,
                      value = c(0, max(investment_data$total_balance) * 1.1),
                      pre = "KES ", sep = ",")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                # Key Metrics Value Boxes
                valueBoxOutput("total_balance", width = 3),
                valueBoxOutput("total_interest", width = 3),
                valueBoxOutput("monthly_growth", width = 3),
                valueBoxOutput("active_investments", width = 3)
              ),
              
              fluidRow(
                # Main Charts
                box(
                  title = "Portfolio Growth Over Time", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("growth_timeline", height = "400px")
                ),
                
                box(
                  title = "Portfolio Allocation", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  plotlyOutput("allocation_pie", height = "400px")
                )
              ),
              
              fluidRow(
                # Additional Metrics
                box(
                  title = "Risk Distribution", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 4,
                  plotlyOutput("risk_distribution", height = "300px")
                ),
                
                box(
                  title = "Monthly Performance", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("monthly_performance", height = "300px")
                )
              )
      ),
      
      # Analytics Tab
      tabItem(tabName = "analytics",
              fluidRow(
                box(
                  title = "Advanced Portfolio Analytics",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tabBox(
                    width = 12,
                    tabPanel("Return Analysis", plotlyOutput("return_analysis")),
                    tabPanel("Risk vs Return", plotlyOutput("risk_return")),
                    tabPanel("Growth Forecast", plotlyOutput("growth_forecast"))
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Comparative Analysis",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(6, plotlyOutput("type_comparison")),
                    column(6, plotlyOutput("risk_comparison"))
                  )
                )
              )
      ),
      
      # Calculator Tab
      tabItem(tabName = "calculator",
              fluidRow(
                box(
                  title = "Investment Calculator",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("initial_investment", "Initial Investment (KES):", 
                               value = 10000, min = 1000, max = 1000000),
                  numericInput("monthly_contribution", "Monthly Contribution (KES):", 
                               value = 5000, min = 0, max = 100000),
                  sliderInput("interest_rate", "Annual Interest Rate (%):",
                              min = 1, max = 20, value = 14.4, step = 0.1),
                  sliderInput("investment_period", "Investment Period (Years):",
                              min = 1, max = 30, value = 5),
                  selectInput("compound_frequency", "Compound Frequency:",
                              choices = c("Monthly" = 12, "Quarterly" = 4, "Annually" = 1),
                              selected = 12),
                  actionButton("calculate", "Calculate Returns", 
                               icon = icon("calculator"), 
                               class = "btn-success btn-lg")
                ),
                
                box(
                  title = "Investment Projection",
                  status = "info",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("projection_chart", height = "400px"),
                  br(),
                  fluidRow(
                    valueBoxOutput("final_amount", width = 4),
                    valueBoxOutput("total_contributions", width = 4),
                    valueBoxOutput("total_interest", width = 4)
                  )
                )
              )
      ),
      
      # Transactions Tab
      tabItem(tabName = "transactions",
              fluidRow(
                box(
                  title = "Transaction History",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("transaction_table")
                )
              )
      ),
      
      # Reports Tab
      tabItem(tabName = "reports",
              fluidRow(
                box(
                  title = "Generate Performance Report",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("report_type", "Report Type:",
                              choices = c("Monthly Performance" = "monthly",
                                          "Quarterly Analysis" = "quarterly",
                                          "Annual Summary" = "annual",
                                          "Custom Period" = "custom")),
                  conditionalPanel(
                    condition = "input.report_type == 'custom'",
                    dateRangeInput("custom_period", "Select Period:")
                  ),
                  selectInput("report_format", "Format:",
                              choices = c("PDF" = "pdf", "HTML" = "html", "Excel" = "excel")),
                  actionButton("generate_report", "Generate Report", 
                               icon = icon("download"),
                               class = "btn-info btn-lg")
                ),
                
                box(
                  title = "Report Preview",
                  status = "info",
                  solidHeader = TRUE,
                  width = 8,
                  htmlOutput("report_preview")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- investment_data
    
    # Apply filters
    if (input$investment_type != "All") {
      data <- data %>% filter(investment_type == input$investment_type)
    }
    
    if (input$risk_level != "All") {
      data <- data %>% filter(risk_level == input$risk_level)
    }
    
    data <- data %>% 
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(total_balance >= input$balance_range[1] & total_balance <= input$balance_range[2])
    
    return(data)
  })
  
  # Value Boxes
  output$total_balance <- renderValueBox({
    data <- filtered_data()
    current_balance <- tail(data$total_balance, 1)
    
    valueBox(
      paste("KES", format(round(current_balance), big.mark = ",")),
      "Current Portfolio Value",
      icon = icon("wallet"),
      color = "blue"
    )
  })
  
  output$total_interest <- renderValueBox({
    data <- filtered_data()
    total_interest <- sum(data$interest_earned)
    
    valueBox(
      paste("KES", format(round(total_interest), big.mark = ",")),
      "Total Interest Earned",
      icon = icon("coins"),
      color = "green"
    )
  })
  
  output$monthly_growth <- renderValueBox({
    data <- filtered_data()
    if (nrow(data) > 1) {
      growth_rate <- ((tail(data$total_balance, 1) / head(data$total_balance, 1)) - 1) * 100
    } else {
      growth_rate <- 0
    }
    
    valueBox(
      paste0(round(growth_rate, 1), "%"),
      "Portfolio Growth Rate",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  output$active_investments <- renderValueBox({
    data <- filtered_data()
    active_count <- nrow(data)
    
    valueBox(
      active_count,
      "Active Investment Periods",
      icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
  # Growth Timeline Chart
  output$growth_timeline <- renderPlotly({
    data <- filtered_data()
    
    p <- plot_ly(data, x = ~date) %>%
      add_trace(y = ~total_balance, type = 'scatter', mode = 'lines+markers',
                name = 'Total Balance', line = list(color = '#3498db', width = 3),
                marker = list(size = 8)) %>%
      add_trace(y = ~principal, type = 'scatter', mode = 'lines',
                name = 'Principal', line = list(color = '#2ecc71', width = 2, dash = 'dot')) %>%
      layout(title = 'Portfolio Growth Over Time',
             xaxis = list(title = 'Date'),
             yaxis = list(title = 'Amount (KES)', tickformat = ',.0f'),
             hovermode = 'x unified')
    
    p
  })
  
  # Portfolio Allocation Pie Chart
  output$allocation_pie <- renderPlotly({
    data <- filtered_data()
    
    allocation <- data %>%
      group_by(investment_type) %>%
      summarise(total = sum(total_balance)) %>%
      mutate(percentage = total / sum(total) * 100)
    
    plot_ly(allocation, labels = ~investment_type, values = ~total, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c('#3498db', '#2ecc71', '#e74c3c', '#f39c12'))) %>%
      layout(title = 'Portfolio Allocation by Type')
  })
  
  # Risk Distribution
  output$risk_distribution <- renderPlotly({
    data <- filtered_data()
    
    risk_summary <- data %>%
      group_by(risk_level) %>%
      summarise(count = n(), total_value = sum(total_balance))
    
    plot_ly(risk_summary, x = ~risk_level, y = ~total_value, type = 'bar',
            marker = list(color = c('#2ecc71', '#f39c12', '#e74c3c'))) %>%
      layout(title = 'Investment Distribution by Risk',
             xaxis = list(title = 'Risk Level'),
             yaxis = list(title = 'Total Value (KES)', tickformat = ',.0f'))
  })
  
  # Monthly Performance
  output$monthly_performance <- renderPlotly({
    data <- filtered_data()
    
    monthly_summary <- data %>%
      mutate(month_year = format(date, "%b %Y")) %>%
      group_by(month_year) %>%
      summarise(
        net_growth = sum(net_growth),
        deposits = sum(principal - lag(principal, default = 0)),
        withdrawals = sum(withdrawals)
      )
    
    plot_ly(monthly_summary, x = ~month_year) %>%
      add_trace(y = ~net_growth, type = 'bar', name = 'Net Growth',
                marker = list(color = '#27ae60')) %>%
      add_trace(y = ~deposits, type = 'bar', name = 'Deposits',
                marker = list(color = '#3498db')) %>%
      add_trace(y = ~withdrawals, type = 'bar', name = 'Withdrawals',
                marker = list(color = '#e74c3c')) %>%
      layout(title = 'Monthly Performance Breakdown',
             xaxis = list(title = 'Month'),
             yaxis = list(title = 'Amount (KES)', tickformat = ',.0f'),
             barmode = 'group')
  })
  
  # Investment Calculator Logic
  projection_data <- eventReactive(input$calculate, {
    initial <- input$initial_investment
    monthly <- input$monthly_contribution
    annual_rate <- input$interest_rate / 100
    years <- input$investment_period
    compound_freq <- as.numeric(input$compound_frequency)
    
    periods <- years * compound_freq
    monthly_rate <- annual_rate / compound_freq
    
    future_value <- function(n) {
      initial * (1 + monthly_rate)^n + 
        monthly * (((1 + monthly_rate)^n - 1) / monthly_rate)
    }
    
    data.frame(
      period = 1:periods,
      balance = sapply(1:periods, future_value),
      contributions = initial + (monthly * (1:periods)),
      interest = sapply(1:periods, future_value) - (initial + (monthly * (1:periods)))
    )
  })
  
  output$projection_chart <- renderPlotly({
    data <- projection_data()
    
    plot_ly(data, x = ~period) %>%
      add_trace(y = ~balance, type = 'scatter', mode = 'lines', 
                name = 'Total Balance', line = list(color = '#3498db', width = 3)) %>%
      add_trace(y = ~contributions, type = 'scatter', mode = 'lines',
                name = 'Total Contributions', line = list(color = '#2ecc71', width = 2)) %>%
      layout(title = 'Investment Growth Projection',
             xaxis = list(title = 'Periods'),
             yaxis = list(title = 'Amount (KES)', tickformat = ',.0f'))
  })
  
  # Calculator Value Boxes
  output$final_amount <- renderValueBox({
    data <- projection_data()
    final_balance <- tail(data$balance, 1)
    
    valueBox(
      paste("KES", format(round(final_balance), big.mark = ",")),
      "Projected Final Amount",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$total_contributions <- renderValueBox({
    data <- projection_data()
    total_contrib <- tail(data$contributions, 1)
    
    valueBox(
      paste("KES", format(round(total_contrib), big.mark = ",")),
      "Total Contributions",
      icon = icon("hand-holding-usd"),
      color = "blue"
    )
  })
  
  output$total_interest <- renderValueBox({
    data <- projection_data()
    total_int <- tail(data$interest, 1)
    
    valueBox(
      paste("KES", format(round(total_int), big.mark = ",")),
      "Projected Interest",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  # Transaction Table
  output$transaction_table <- renderDT({
    data <- filtered_data() %>%
      mutate(
        transaction_type = ifelse(withdrawals > 0, "Withdrawal", "Deposit"),
        amount = ifelse(withdrawals > 0, -withdrawals, principal - lag(principal, default = 0))
      ) %>%
      select(date, investment_type, risk_level, amount, transaction_type, total_balance)
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      rownames = FALSE,
      colnames = c('Date', 'Type', 'Risk', 'Amount', 'Transaction', 'Balance')
    ) %>%
      formatCurrency('amount', currency = "KES ", digits = 0) %>%
      formatCurrency('total_balance', currency = "KES ", digits = 0)
  })
}

# Run the application
shinyApp(ui = ui, server = server)