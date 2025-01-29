
library(DBI)
library(pool)
library(RSQLite)
library(logger)
library(jsonlite)

# Cache configuration
CACHE_CONFIG <- list(
  cache_table = "column_metadata",
  data_table = "timeseries",
  max_cache_age_hours = 24
)

#' Initialize the cache database schema
#' @param pool Database connection pool
initialize_cache_schema <- function(pool) {
  message("Initializing cache schema")

  tryCatch({
    # Create timeseries table
    # dbExecute(pool, "
    #   CREATE TABLE IF NOT EXISTS timeseries (
    #     id INTEGER PRIMARY KEY,
    #     date TIMESTAMP,
    #     value NUMERIC,
    #     category VARCHAR(255),
    #     region VARCHAR(255)
    #   )
    # ")

    # Create column metadata cache table
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS column_metadata (
        column_name VARCHAR(255) PRIMARY KEY,
        column_type VARCHAR(50),
        distinct_values TEXT,
        min_value NUMERIC,
        max_value NUMERIC,
        last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")

    message("Cache schema initialized successfully")
  }, error = function(e) {
    log_error("Error initializing cache schema: {str(e)}")
    stop(e)
  })
}

#' Get column metadata from cache
#' @param column_name Name of the column
#' @return List containing column metadata
get_cached_column_metadata <- function(column_name) {
  pool <- get_pool()

  result <- dbGetQuery(pool, sprintf("
    SELECT * FROM %s WHERE column_name = ?",
                                     CACHE_CONFIG$cache_table
  ), params = list(column_name))

  if (nrow(result) == 0) return(NULL)

  # Parse distinct values from JSON
  if (!is.na(result$distinct_values)) {
    result$distinct_values <- list(fromJSON(result$distinct_values))
  }

  return(as.list(result))
}

#' Update column metadata in cache
#' @param column_name Name of the column
#' @param metadata List containing column metadata
update_column_metadata <- function(column_name, metadata) {
  pool <- get_pool()

  # Convert distinct values to JSON if present
  if (is.null(metadata$distinct_values)) {
    metadata$distinct_values <- toJSON(NA)

  } else {
    metadata$distinct_values <- toJSON(metadata$distinct_values)
  }

  if (is.null(metadata$min_value)) {
    metadata$min_value <- NA
  }

  if (is.null(metadata$max_value)) {
    metadata$max_value <- NA
  }


  tryCatch({
    dbExecute(pool, sprintf("
      INSERT OR REPLACE INTO %s (
        column_name, column_type, distinct_values,
        min_value, max_value, last_updated
      ) VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)",
                            CACHE_CONFIG$cache_table
    ), params = c(
      column_name,
      metadata$column_type,
      metadata$distinct_values,
      metadata$min_value,
      metadata$max_value
    ))

    message("Updated cache for column: {column_name}")
  }, error = function(e) {
    log_error("Error updating cache for column {column_name}: {str(e)}")
    stop(e)
  })
}

#' Check if cache is stale for a given column
#' @param column_name Name of the column
#' @return Boolean indicating if cache is stale
is_cache_stale <- function(column_name = NULL) {
  pool <- get_pool()

  query <- sprintf("
    SELECT MAX(ROUND((julianday('now') - julianday(last_updated)) * 24)) as hours_old
    FROM %s", CACHE_CONFIG$cache_table)

  if (!is.null(column_name)) {
    query <- paste(query, "WHERE column_name = ?")
    params <- list(column_name)
  } else {
    params <- list()
  }

  result <- dbGetQuery(pool, query, params = params)

  # Consider cache stale if no data or older than configured hours
  return(is.na(result$hours_old) ||
           result$hours_old > CACHE_CONFIG$max_cache_age_hours)
}

#' Get all column names from the data table
#' @return Character vector of column names
get_column_names <- function() {
  pool <- get_pool()

  # Use pragma table_info instead of dbColumnInfo
  result <- dbGetQuery(pool, sprintf("PRAGMA table_info(%s)",
                                     CACHE_CONFIG$data_table))
  return(result$name)
}

#' Get column type (numeric or categorical)
#' @param column_name Name of the column
#' @return Character string indicating column type
get_column_type <- function(column_name) {
  metadata <- get_cached_column_metadata(column_name)
  if (!is.null(metadata)) {
    return(metadata$column_type)
  }

  # Fallback to querying the database
  pool <- get_pool()
  result <- dbGetQuery(pool, sprintf(
    "SELECT typeof(%s) as col_type FROM %s WHERE %s IS NOT NULL LIMIT 1",
    column_name, CACHE_CONFIG$data_table, column_name
  ))

  return(ifelse(result$col_type %in% c("integer", "real"), "numeric", "categorical"))
}

#' Get distinct values for a categorical column
#' @param column_name Name of the column
#' @return Vector of distinct values
get_distinct_values <- function(column_name) {
  metadata <- get_cached_column_metadata(column_name)
  if (!is.null(metadata) && !is.null(metadata$distinct_values)) {
    return(metadata$distinct_values[[1]])
  }

  # Fallback to querying the database
  pool <- get_pool()
  result <- dbGetQuery(pool, sprintf(
    "SELECT DISTINCT %s FROM %s WHERE %s IS NOT NULL",
    column_name, CACHE_CONFIG$data_table, column_name
  ))

  return(result[[1]])
}

#' Get range for a numeric column
#' @param column_name Name of the column
#' @return Vector containing min and max values
get_range <- function(column_name) {
  metadata <- get_cached_column_metadata(column_name)
  if (!is.null(metadata) && !is.null(metadata$min_value)) {
    return(c(metadata$min_value, metadata$max_value))
  }

  # Fallback to querying the database
  pool <- get_pool()
  result <- dbGetQuery(pool, sprintf(
    "SELECT MIN(%s) as min_val, MAX(%s) as max_val FROM %s",
    column_name, column_name, CACHE_CONFIG$data_table
  ))

  return(c(result$min_val, result$max_val))
}


library(DBI)
library(pool)
library(RSQLite)
library(logger)
library(jsonlite)
library(lubridate)

# Cache configuration
CACHE_CONFIG <- list(
  cache_table = "column_metadata",
  data_table = "timeseries",
  max_cache_age_hours = 24
)

#' Create sample timeseries data
#' @param n Number of rows to generate
#' @return Data frame with sample timeseries data
create_sample_data <- function(n = 1000) {
  # Create date sequence for past 2 years
  dates <- seq(
    from = Sys.Date() - years(2),
    to = Sys.Date(),
    length.out = n
  )

  # Create multiple metrics with different patterns
  set.seed(123)  # For reproducibility

  data.frame(
    date = dates,
    # Main value with trend and seasonality
    value = sin(1:n / 50) * 100 + # Seasonal component
      0.1 * 1:n +            # Upward trend
      rnorm(n, sd = 10),     # Random noise

    # Additional metrics
    daily_users = rpois(n, lambda = 100) + 50 * sin(1:n / 30),
    revenue = rlnorm(n, meanlog = 4, sdlog = 0.3) * (1 + sin(1:n / 60)),

    # Categorical variables
    category = sample(
      c("Product A", "Product B", "Product C", "Product D"),
      n, replace = TRUE,
      prob = c(0.4, 0.3, 0.2, 0.1)
    ),
    region = sample(
      c("North America", "Europe", "Asia", "South America"),
      n, replace = TRUE
    ),
    status = sample(
      c("Active", "Inactive", "Pending"),
      n, replace = TRUE,
      prob = c(0.7, 0.2, 0.1)
    )
  )
}

#' Initialize the database with sample data
#' @param pool Database connection pool
initialize_sample_database <- function(pool) {
  message("Initializing sample database")

  tryCatch({
    # Drop existing tables if they exist
    dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s", CACHE_CONFIG$data_table))
    dbExecute(pool, sprintf("DROP TABLE IF EXISTS %s", CACHE_CONFIG$cache_table))

    # Create timeseries table
    dbExecute(pool, "
      CREATE TABLE timeseries (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        date DATE NOT NULL,
        value NUMERIC,
        daily_users INTEGER,
        revenue NUMERIC,
        category VARCHAR(50),
        region VARCHAR(50),
        status VARCHAR(20)
      )
    ")

    # Create indexes for better query performance
    dbExecute(pool, "CREATE INDEX idx_timeseries_date ON timeseries(date)")
    dbExecute(pool, "CREATE INDEX idx_timeseries_category ON timeseries(category)")
    dbExecute(pool, "CREATE INDEX idx_timeseries_region ON timeseries(region)")

    # Create column metadata cache table
    dbExecute(pool, "
      CREATE TABLE column_metadata (
        column_name VARCHAR(255) PRIMARY KEY,
        column_type VARCHAR(50),
        distinct_values TEXT,
        min_value NUMERIC,
        max_value NUMERIC,
        last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")

    # Generate and insert sample data
    message("Generating sample data")
    sample_data <- create_sample_data(1000)

    # Insert data in chunks to avoid memory issues
    chunk_size <- 100
    n_chunks <- ceiling(nrow(sample_data) / chunk_size)

    for(i in 1:n_chunks) {
      start_idx <- ((i-1) * chunk_size) + 1
      end_idx <- min(i * chunk_size, nrow(sample_data))
      chunk <- sample_data[start_idx:end_idx, ]

      dbWriteTable(pool, CACHE_CONFIG$data_table, chunk,
                   append = TRUE, row.names = FALSE)
    }

    message("Sample database initialized successfully")
  }, error = function(e) {
    log_error("Error initializing sample database: {str(e)}")
    stop(e)
  })
}
