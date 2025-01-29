#!/usr/bin/env Rscript

# Set working directory to the script location
#setwd(dirname(sys.frame(1)$ofile))

library(DBI)
library(RSQLite)
library(logger)
library(jsonlite)
library(lubridate)

# Source cache utilities
#source("../R/cache_utils.R")

#' Generate sample time series data
#' @param n Number of rows to generate
#' @return Data frame with sample data
generate_sample_data <- function(n = 1000) {
  # Create date sequence
  dates <- seq(as_datetime("2023-01-01"), as_datetime("2024-12-31"), length.out = n)

  # Generate sample data
  data.frame(
    date = dates,
    value = sin(1:n / 50) * 100 + rnorm(n, sd = 10),
    category = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE)
  )
}

#' Sync metadata for a single column
#' @param pool Database connection pool
#' @param column_name Name of the column
#' @param column_type Type of the column (numeric or categorical)
sync_column_metadata <- function(pool, column_name, column_type) {
#  message("Syncing metadata for column: {column_name}")

  metadata <- list(column_type = column_type)

  if (column_type == "numeric") {
    # Get range for numeric columns
    range_query <- sprintf(
      "SELECT MIN(%s) as min_val, MAX(%s) as max_val FROM %s",
      column_name, column_name, CACHE_CONFIG$data_table
    )
    range_result <- dbGetQuery(pool, range_query)

    metadata$min_value <- range_result$min_val
    metadata$max_value <- range_result$max_val
    metadata$distinct_values <- NULL
  } else {
    # Get distinct values for categorical columns
    distinct_query <- sprintf(
      "SELECT DISTINCT %s FROM %s WHERE %s IS NOT NULL",
      column_name, CACHE_CONFIG$data_table, column_name
    )
    distinct_result <- dbGetQuery(pool, distinct_query)

    metadata$distinct_values <- distinct_result[[1]]
    metadata$min_value <- NULL
    metadata$max_value <- NULL
  }

  update_column_metadata(column_name, metadata)
}

#' Main sync function
main <- function() {
  #log_threshold(INFO)
  #log_appender(appender_file("../logs/sync.log"))

  #message("Starting cache sync")

  tryCatch({
    # Initialize pool
    initialize_pool()
    pool <- get_pool()

    # Initialize schema
    initialize_cache_schema(pool)

    # Generate and insert sample data if table is empty
    if (dbGetQuery(pool, sprintf("SELECT COUNT(*) as count FROM %s",
                                 CACHE_CONFIG$data_table))$count == 0) {
      message("Generating sample data")
      sample_data <- generate_sample_data()
      dbWriteTable(pool, CACHE_CONFIG$data_table, sample_data,
                   append = TRUE, row.names = FALSE)
    }

    # Get column information using PRAGMA table_info instead of dbColumnInfo
    columns <- dbGetQuery(pool, sprintf("PRAGMA table_info(%s)",
                                        CACHE_CONFIG$data_table))

    # Sync metadata for each column
    for (col in columns$name) {
      message("col:", col)
      # Skip date column
      if (col == "date") next

      # Determine column type
      type_query <- sprintf(
        "SELECT typeof(%s) as col_type FROM %s WHERE %s IS NOT NULL LIMIT 1",
        col, CACHE_CONFIG$data_table, col
      )
      col_type <- dbGetQuery(pool, type_query)$col_type

      # Map SQLite types to our types
      col_type <- ifelse(col_type %in% c("integer", "real"), "numeric", "categorical")

      # Sync metadata
      sync_column_metadata(pool, col, col_type)
    }

    message("Cache sync completed successfully")
  }, error = function(e) {
    log_error("Error during cache sync: {str(e)}")
    stop(e)
  }, finally = {
    poolClose(pool)
  })
}

# Run main function if script is called directly
if (!interactive()) {
  main()
}
