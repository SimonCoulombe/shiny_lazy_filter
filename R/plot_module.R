

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
    message("Refresh button clicked")
    is_loading(TRUE)
    query <- build_query(filters())
    message("query:", query)
    is_loading(FALSE)
    #query_state(query)
  })

  # Data download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("timeseries-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(current_data())
      write.csv(current_data(), file, row.names = FALSE)
    }
  )

  # Plot with progress indicator
  output$timeseries_plot <- renderPlot({
    #req(query_state())

    # Check cache staleness
    # if (is_cache_stale()) {
    #   showNotification(
    #     "Data cache is stale. Results may not reflect latest data.",
    #     type = "warning",
    #     duration = 10
    #   )
    # }

    withProgress(
      message = 'Fetching data...',
      detail = 'This may take a few seconds...',
      value = 0,
      {
        # Get data
        data <- tryCatch({
          message("getting data with query", query)
          #get_data_with_progress(query_state(), session)
          dbGetQuery(pool, query)
        }, error = function(e) {
          #log_error("Error fetching data: {str(e)}")
          showNotification(
            "Error fetching data. Please try again.",
            type = "error"
          )
          return(NULL)
        })

        if (is.null(data)) return(NULL)

        current_data(data)
        is_loading(FALSE)

        # Create plot
        p <- ggplot(data, aes(x = date, y = value)) +
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
      }
    )
  })
}
