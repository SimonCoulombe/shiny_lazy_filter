
# ===== myapp/R/modules/plot_module.R =====
library(shiny)
library(ggplot2)
library(logger)

plotModuleUI <- function(id) {
  ns <- NS(id)
  div(
    style = "position: relative;",
    div(
      class = "plot-controls",
      style = "margin-bottom: 15px;",
      actionButton(ns("refresh"), "Apply Filters", class = "btn-primary"),
      downloadButton(ns("download_data"), "Download Data", class = "btn-info")
    ),
    div(
      class = "plot-container",
      style = "position: relative; min-height: 400px;",
      plotOutput(ns("timeseries_plot"), height = "400px"),
      uiOutput(ns("loading_indicator"))
    )
  )
}
plotModule <- function(input, output, session, filters) {
  ns <- session$ns

  # Reactive values
  query_state <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  current_data <- reactiveVal(NULL)

  # Loading indicator
  output$loading_indicator <- renderUI({
    if (!is_loading()) return(NULL)

    div(
      class = "loading-spinner",
      tags$div(
        class = "progress",
        style = "width: 200px;",
        tags$div(
          class = "progress-bar progress-bar-striped active",
          role = "progressbar",
          style = "width: 100%"
        )
      ),
      h4("Loading data...")
    )
  })

  # Update query state when refresh is clicked
  observeEvent(input$refresh, {
    log_info("Refresh button clicked")
    is_loading(TRUE)

    # Build the query based on the filters
    query <- build_query(filters())
    log_info("Query:", query)

    # Update the query state
    query_state(query)

    # Simulate data fetching (replace with actual data fetching logic)
    data <- tryCatch({
      log_info("Fetching data with query:", query)
      # Replace this with your actual data fetching logic
      # Example: dbGetQuery(pool, query)
      data.frame(
        date = seq.Date(Sys.Date() - 29, Sys.Date(), by = "day"),
        value = rnorm(30, mean = 50, sd = 10)
      )
    }, error = function(e) {
      showNotification(
        "Error fetching data. Please try again.",
        type = "error"
      )
      return(NULL)
    })

    if (is.null(data)) {
      is_loading(FALSE)
      return()
    }

    # Update the current data
    current_data(data)
    is_loading(FALSE)
  })

  # Data download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("timeseries-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(current_data())
      write.csv(current_data(), file, row.names = FALSE)
    }
  )

  # Plot with progress indicator
  output$timeseries_plot <- renderPlot({
    req(current_data())  # Ensure data is available

    # Create plot
    p <- ggplot(current_data(), aes(x = date, y = value)) +
      geom_line(color = "#2c3e50", size = 1) +
      geom_point(color = "#2c3e50", size = 2, alpha = 0.5) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "#ecf0f1"),
        panel.grid.minor = element_line(color = "#f7f9f9")
      ) +
      labs(
        title = "Time Series Data",
        x = "Date",
        y = "Value"
      )

    return(p)
  })
}
