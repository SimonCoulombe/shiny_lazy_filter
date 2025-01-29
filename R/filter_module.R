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
  log_info("Initializing filter module")
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
