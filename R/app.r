# ===== myapp/R/app.R =====
library(shiny)
library(pool)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(logger)

source(here:::here("R/cache-utils.R"))
source(here:::here("R/db_utils.R"))
source(here:::here("R/filter_module.R"))
source(here:::here("R/plot_module.R"))

# Initialize logging
log_threshold(INFO)

t <- tempfile()
log_appender(appender_tee(t))
#log_appender(appender_file(here::here("logs/app.log")))

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

#  log_info("Initializing app...")
  initialize_pool()
  # Initialize connection pool on startup
  #onStart(function() {
  #})

  # Clean up on app exit
  onStop(function() {
 #   log_info("Shutting down app...")
    if (!is.null(pool)) poolClose(pool)
  })

  # Dynamic filters management
  filter_count <- reactiveVal(0)
  filters <- reactiveVal(list())

  observeEvent(input$add_filter, {
    log_info("Adding new filter")
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

