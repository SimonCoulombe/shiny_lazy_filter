# ===== myapp/R/db_utils.R =====
library(DBI)
library(pool)
library(RSQLite)
library(logger)

get_pool <- function() {
  log_info("Initializing connection pool")
  pool <- dbPool(
    RSQLite::SQLite(),
    dbname = "cache.db",
    maxSize = 10,
    idleTimeout = 3600
  )
  return(pool)
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

  log_info("Generated query: {query}")
  query
}

