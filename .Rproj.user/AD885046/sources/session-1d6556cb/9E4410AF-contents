# ===== myapp/R/modules/filter_module.R =====
library(shiny)
library(logger)

filterModuleUI <- function(id) {
  ns <- NS(id)
  div(
    class = "filter-module",
    style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
    div(
      class = "filter-header",
      style = "display: flex; justify-content: space-between; align-items: center;",
      selectInput(ns("column"), "Select Column", choices = get_column_names()),
      actionButton(ns("remove"), "×", class = "btn-danger",
                  style = "border-radius: 50%; width: 30px; height: 30px;")
    ),
    uiOutput(ns("filter_ui"))
  )
}

filterModule <- function(input, output, session) {
  message("Initializing filter module")
  filter_state <- reactiveVal(NULL)

  output$filter_ui <- renderUI({
    ns <- session$ns
    req(input$column)

    if(get_column_type(input$column) == "numeric") {
      range <- get_range(input$column)
      sliderInput(ns("range"), "Select Range",
                 min = range[1], max = range[2],
                 value = range,
                 step = (range[2] - range[1])/100)
    } else {
      choices <- get_distinct_values(input$column)
      selectizeInput(ns("categories"), "Select Categories",
                    choices = choices,
                    multiple = TRUE,
                    selected = choices,
                    options = list(plugins = list('remove_button')))
    }
  })

  observe({
    req(input$column)
    col_type <- get_column_type(input$column)

    if(col_type == "numeric") {
      req(input$range)
      filter_state(list(
        column = input$column,
        type = "numeric",
        range = input$range
      ))
    } else {
      req(input$categories)
      filter_state(list(
        column = input$column,
        type = "categorical",
        categories = input$categories
      ))
    }
  })

  return(filter_state)
}

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
