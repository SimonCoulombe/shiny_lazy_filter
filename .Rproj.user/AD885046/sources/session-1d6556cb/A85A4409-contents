# ===== myapp/R/app.R =====
library(shiny)
library(pool)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(logger)

# Initialize logging
#log_threshold(INFO)
#log_appender(appender_file("logs/app.log"))

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .loading-spinner {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
        background: rgba(255, 255, 255, 0.9);
        padding: 20px;
        border-radius: 5px;
        text-align: center;
      }
    "))
  ),
  titlePanel("Time Series Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_filter", "Add Filter", class = "btn-primary"),
      hr(),
      uiOutput("filters")
    ),
    mainPanel(
      plotModuleUI("plot")
    )
  )
)

# Server
server <- function(input, output, session) {

#  message("Initializing app...")
  initialize_pool()
  # Initialize connection pool on startup
  #onStart(function() {
  #})

  # Clean up on app exit
  onStop(function() {
 #   message("Shutting down app...")
    if (!is.null(pool)) poolClose(pool)
  })

  # Dynamic filters management
  filter_count <- reactiveVal(0)
  filters <- reactiveVal(list())

  observeEvent(input$add_filter, {
    message("Adding new filter")
    current_count <- filter_count()
    filter_count(current_count + 1)

    filters_list <- filters()
    filters_list[[length(filters_list) + 1]] <- callModule(
      filterModule,
      paste0("filter_", current_count)
    )
    filters(filters_list)
  })

  output$filters <- renderUI({
    lapply(seq_len(filter_count()), function(i) {
      filterModuleUI(paste0("filter_", i-1))
    })
  })

  # Initialize plot module
  callModule(plotModule, "plot", filters = filters)
}

# Run the app

shinyApp(ui = ui, server = server)

# ===== myapp/R/db_utils.R =====
library(DBI)
library(pool)
library(RSQLite)
library(logger)

pool <- NULL

initialize_pool <- function() {
  message("Initializing connection pool")
  pool <<- dbPool(
    RSQLite::SQLite(),
    dbname = "cache.db",
    maxSize = 10,
    idleTimeout = 3600
  )
}

get_pool <- function() {
  if (is.null(pool)) {
    initialize_pool()
  }
  pool
}

build_query <- function(filters) {
  query <- "SELECT * FROM timeseries"

  if (length(filters) > 0) {
    valid_conditions <- c()

    for (filter in filters) {
      filter_data <- filter()
      if (!is.null(filter_data)) {
        if (filter_data$type == "numeric") {
          valid_conditions <- c(valid_conditions,
                              sprintf("%s BETWEEN %f AND %f",
                                    filter_data$column,
                                    filter_data$range[1],
                                    filter_data$range[2]))
        } else {
          categories <- paste(sprintf("'%s'", filter_data$categories),
                            collapse = ",")
          if (categories != "") {
            valid_conditions <- c(valid_conditions,
                                sprintf("%s IN (%s)",
                                      filter_data$column,
                                      categories))
          }
        }
      }
    }

    if (length(valid_conditions) > 0) {
      query <- paste(query, "WHERE",
                    paste(valid_conditions, collapse = " AND "))
    }
  }

  message("Generated query: {query}")
  query
}

