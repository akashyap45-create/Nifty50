# ui.R
library(shiny)
library(shinydashboard)

# Define the list of Nifty 50 companies (you might need to update this periodically)
nifty50_companies <- c(
  "RELIANCE.NS", "TCS.NS", "HDFC.NS", "INFY.NS", "ICICIBANK.NS",
  "HUL.NS", "SBIN.NS", "BHARTIARTL.NS", "KOTAKBANK.NS", "LT.NS",
  "AXISBANK.NS", "ITC.NS", "HDFCBANK.NS", "MARUTI.NS", "ASIANPAINT.NS",
  "NTPC.NS", "POWERGRID.NS", "BAJFINANCE.NS", "TITAN.NS", "M&M.NS",
  "ULTRACEMCO.NS", "NESTLEIND.NS", "HCLTECH.NS", "WIPRO.NS", "SUNPHARMA.NS",
  "ADANIENT.NS", "JSWSTEEL.NS", "TATAMOTORS.NS", "ADANIPORTS.NS", "SRF.NS",
  "DIVISLAB.NS", "CIPLA.NS", "ONGC.NS", "GRASIM.NS", "HINDALCO.NS",
  "APOLLOHOSP.NS", "HEROMOTOCO.NS", "BAJAJ-AUTO.NS", "INDUSINDBK.NS", "SBILIFE.NS",
  "TECHM.NS", "UPL.NS", "DRREDDY.NS", "EICHERMOT.NS", "BPCL.NS",
  "IOC.NS", "COALINDIA.NS", "GAIL.NS", "TATACONSUM.NS", "LTIM.NS"
)

ui <- dashboardPage(
  dashboardHeader(title = "Nifty 50 Stock Data"),
  dashboardSidebar(
    selectInput(
      "company",
      "Select Nifty 50 Company:",
      choices = nifty50_companies
    ),
    dateRangeInput(
      "date_range",
      "Select Date Range:",
      start = Sys.Date() - 365,
      end = Sys.Date()
    ),
    actionButton("fetch_data", "Fetch Stock Data")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Stock Data",
               h4("Real-time (Most Recent Available) Stock Data:"),
               verbatimTextOutput("realtime_data"),
               br(),
               h4("Historical Stock Price Chart:"),
               dygraphOutput("stock_chart"),
               br(),
               h4("Historical Stock Data (Table):"),
               dataTableOutput("stock_table")
      ),
      tabPanel("Relative Performance",
               h4("Stock vs. Nifty 50 - Percentage Change:"),
               dygraphOutput("relative_performance_chart")
      ),
      tabPanel("Stock vs Nifty",
               h4("Stock Price vs. Nifty 50 Price"),
               dygraphOutput("stock_vs_nifty_chart")
      )
    )
  )
)
