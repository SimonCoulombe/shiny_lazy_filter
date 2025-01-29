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

  pool <- get_pool()

  # Dynamic filters management
  filter_count <- reactiveVal(0)
  filters <- reactiveVal(list())

  observeEvent(input$add_filter, {
    log_info("Adding new filter")
    current_count <- filter_count()
    new_count <- current_count + 1
    filter_count(new_count)
    filters_list <- filters()
    filters_list[[new_count]] <- filterModule(paste0("filter_", new_count))
    message(str(filters_list))
    filters(filters_list)
  })

  output$filters <- renderUI({
    lapply(seq_len(filter_count()), function(i) {
      filterModuleUI(paste0("filter_", i))
    })
  })

  # Initialize plot module
  plotModule("plot", filters = filters)
}

# Run the app
shinyApp(ui = ui, server = server)
