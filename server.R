# server.R
library(shiny)
library(quantmod)
library(xts)
library(dygraphs)
library(dplyr)

server <- function(input, output, session) {

  stock_data_raw <- eventReactive(input$fetch_data, {
    req(input$company)
    req(input$date_range)

    start_date <- input$date_range[1]
    end_date <- input$date_range[2]

    tryCatch({
      getSymbols(input$company,
                 from = start_date,
                 to = end_date,
                 auto.assign = FALSE)
    }, error = function(e) {
      showNotification(paste("Error fetching data for", input$company, ":", e$message), type = "error")
      NULL
    })
  })

  nifty_data_raw <- eventReactive(input$fetch_data, {
    req(input$date_range)

    start_date <- input$date_range[1]
    end_date <- input$date_range[2]

    tryCatch({
      getSymbols("^NSEI", # Ticker symbol for Nifty 50
                 from = start_date,
                 to = end_date,
                 auto.assign = FALSE)
    }, error = function(e) {
      showNotification(paste("Error fetching Nifty 50 data:", e$message), type = "error")
      NULL
    })
  })

  stock_data <- reactive({
    Ad(stock_data_raw()) # Use adjusted closing prices
  })

  nifty_data <- reactive({
    Ad(nifty_data_raw()) # Use adjusted closing prices
  })

  # Display Real-time (Most Recent) Data
  output$realtime_data <- renderPrint({
    stock <- stock_data_raw()
    if (!is.null(stock) && nrow(stock) > 0) {
      tail(stock, 1)
    } else {
      "No data available or error fetching stock data."
    }
  })

  # Display Historical Stock Price Chart
  output$stock_chart <- renderDygraph({
    stock <- stock_data()
    if (!is.null(stock) && nrow(stock) > 0) {
      dygraph(stock, main = paste(input$company, "Adjusted Close Price")) %>%
        dySeries(names(stock), label = names(stock)) %>%
        dyOptions(labelsUTC = TRUE, strokeWidth = 2, drawGrid = TRUE)
    } else {
      NULL
    }
  })

  # Display Historical Stock Data Table
  output$stock_table <- renderDataTable({
    stock <- stock_data_raw()
    if (!is.null(stock) && nrow(stock) > 0) {
      as.data.frame(stock)
    } else {
      NULL
    }
  })

  # Display Relative Performance Chart
  output$relative_performance_chart <- renderDygraph({
    stock <- stock_data()
    nifty <- nifty_data()

    if (!is.null(stock) && nrow(stock) > 1 && !is.null(nifty) && nrow(nifty) > 1) {
      # Align the dates if necessary
      merged_data <- merge(stock, nifty, join = "inner")
      stock_aligned <- merged_data[, 1]
      nifty_aligned <- merged_data[, 2]

      # Calculate percentage change
      stock_change <- (stock_aligned / lag(stock_aligned) - 1) * 100
      nifty_change <- (nifty_aligned / lag(nifty_aligned) - 1) * 100

      # Combine into a single xts object
      relative_data <- cbind(stock_change, nifty_change)
      colnames(relative_data) <- c(input$company, "^NSEI")
      relative_data <- relative_data[-1,] # Remove the first NA row

      dygraph(relative_data, main = "Relative Performance (% Change)") %>%
        dySeries(colnames(relative_data)[1], label = colnames(relative_data)[1]) %>%
        dySeries(colnames(relative_data)[2], label = colnames(relative_data)[2]) %>%
        dyAxis("y", label = "Percentage Change") %>%
        dyOptions(labelsUTC = TRUE, strokeWidth = 2, drawGrid = TRUE)
    } else {
      NULL
    }
  })

  # Display Stock vs Nifty Chart with Secondary Axis
  output$stock_vs_nifty_chart <- renderDygraph({
    stock <- stock_data()
    nifty <- nifty_data()

    if (!is.null(stock) && nrow(stock) > 1 && !is.null(nifty) && nrow(nifty) > 1) {
      # Align the dates
      merged_prices <- merge(stock, nifty, join = "inner")
      colnames(merged_prices) <- c(input$company, "^NSEI")

      dygraph(merged_prices, main = paste(input$company, "vs. Nifty 50")) %>%
        dySeries(colnames(merged_prices)[1], axis = "y2", color = "blue") %>%
        dySeries(colnames(merged_prices)[2], axis = "y", color = "red") %>%
        dyAxis("y", label = "Nifty 50 Price", axisLabelColor = "red") %>%
        dyAxis("y2", label = paste(colnames(merged_prices)[1], "Price"), axisLabelColor = "blue", independentTicks = TRUE) %>%
        dyOptions(labelsUTC = TRUE, strokeWidth = 2, drawGrid = TRUE)
    } else {
      NULL
    }
  })
}
