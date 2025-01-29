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

filterModule <- function(input, output, session, filter_state) {
  ns <- session$ns

  # Initialize the filter state if it doesn't exist
  if (is.null(filter_state$column)) {
    filter_state$column <- NULL
    filter_state$type <- NULL
    filter_state$range <- NULL
    filter_state$categories <- NULL
  }

  # Update the column selection
  observe({
    updateSelectInput(session, "column", selected = filter_state$column)
  })

  output$filter_ui <- renderUI({
    req(input$column)

    if (get_column_type(input$column) == "numeric") {
      range <- get_range(input$column)
      sliderInput(ns("range"), "Select Range",
                  min = range[1], max = range[2],
                  value = filter_state$range %||% range,
                  step = (range[2] - range[1]) / 100)
    } else {
      choices <- get_distinct_values(input$column)
      selectizeInput(ns("categories"), "Select Categories",
                     choices = choices,
                     multiple = TRUE,
                     selected = filter_state$categories %||% choices,
                     options = list(plugins = list('remove_button')))
    }
  })

  # Update the filter state when inputs change
  observe({
    req(input$column)
    col_type <- get_column_type(input$column)

    if (col_type == "numeric") {
      req(input$range)
      filter_state$column <- input$column
      filter_state$type <- "numeric"
      filter_state$range <- input$range
    } else {
      req(input$categories)
      filter_state$column <- input$column
      filter_state$type <- "categorical"
      filter_state$categories <- input$categories
    }
  })

  return(filter_state)
}
