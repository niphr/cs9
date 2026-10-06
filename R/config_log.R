#' Update Configuration Log
#'
#' Logs configuration updates with relevant metadata such as timestamp,
#' session state, task name, and a custom message. The function inserts
#' the log entry into the `config_log` table in the current configuration.
#'
#' @param ss Character. Surveillance system identifier. Defaults to `"unspecified"`.
#' @param task Character. Name of the task being logged. Defaults to `"unspecified"`.
#' @param ... Character. Custom message describing the log entry. Must not be `NULL`.
#'
#' @details
#' The function records the type of interaction (automatic or interactive),
#' session state, task description, and a user-provided message in the configuration log.
#' It throws an error if the `message` argument is `NULL`.
#'
#' The `datetime` column holds milliseconds. Each call in one R process
#' writes a `datetime` at least 4 milliseconds after the previous call.
#' This keeps two calls for the same task apart in the key of `config_log`.
#'
#' @return No return value; this function is called for its side effect of inserting
#' a log entry into the `config_log` table.
#'
#' @examples
#' \dontrun{
#' update_config_log(ss = "weather", task = "data_import", message = "Imported dataset successfully.")
#' }
#'
#' @export
update_config_log <- function(
  ss = "unspecified",
  task = "unspecified",
  ...
) {
  # Capture ... as a single string for logging
  msg <- paste0(..., collapse = "")

  # Check that message is not null
  stopifnot(!is.null(msg))

  datetime <- config_log_datetime(Sys.time())
  date <- stringr::str_sub(datetime, 1, 10)

  # Output the message to the console using message()
  message(msg)

  to_upload <- data.table(
    auto_interactive = ifelse(config$is_auto, "auto", "interactive"),
    ss = ss,
    task = task,
    date,
    datetime,
    message = msg
  )
  return(config$tables$config_log$insert_data(to_upload))
}

# The key of config_log is auto_interactive, ss, task and datetime. A datetime
# in whole seconds made a second run of a task in the same second fail on
# SQLite with "UNIQUE constraint failed". The bulk loads on PostgreSQL and SQL
# Server ignore the exit status of psql and bcp, so there the row was lost
# without an R error.
#
# The fix changes the value and leaves the table alone. csdb drops and
# recreates a table whose field names change, and it never changes the keys of
# a table that exists. So a new key column would delete the production log.
#
# Three decimals fit all three backends. SQLite stores the text. PostgreSQL
# TIMESTAMP keeps microseconds. SQL Server DATETIME accepts three decimals and
# rounds them to steps of 1/300 second, and one rounding step covers at most 4
# consecutive milliseconds. The clock on Windows can return the same time for
# about 10 ms. So each value is at least 4 ms after the previous one.
config_log_state <- new.env(parent = emptyenv())
config_log_state$last_ms <- -Inf

config_log_datetime <- function(now) {
  ms <- floor(as.numeric(now) * 1000)
  ms <- max(ms, config_log_state$last_ms + 4)
  config_log_state$last_ms <- ms
  return(paste0(
    format(.POSIXct(ms %/% 1000), "%Y-%m-%d %H:%M:%S"),
    sprintf(".%03d", as.integer(ms %% 1000))
  ))
}

# Formats an instant with milliseconds, in the time zone of the instant. The
# key of config_tasks_stats and config_data_hash_for_each_plan holds a
# datetime. In whole seconds, a second write in the same second replaced the
# first row. This helper keeps no state, unlike config_log_datetime(). It
# rounds, because format(x, "%OS3") truncates and can print .123 as .122.
datetime_ms <- function(x) {
  ms <- round(as.numeric(x) * 1000)
  return(paste0(
    format(.POSIXct(ms %/% 1000, tz = attr(x, "tzone")), "%Y-%m-%d %H:%M:%S"),
    sprintf(".%03d", as.integer(ms %% 1000))
  ))
}

#' Get Configuration Log
#'
#' Retrieves configuration log entries from the `config_log` table with optional filtering
#' by surveillance system identifier, task name, and date range.
#'
#' @param ss Character. Surveillance system identifier to filter by. Defaults to `NULL` (no filtering).
#' @param task Character. Task name to filter by. Defaults to `NULL` (no filtering).
#' @param start_date Character. Start date (`YYYY-MM-DD`) for filtering log entries. Defaults to `NULL`.
#' @param end_date Character. End date (`YYYY-MM-DD`) for filtering log entries. Defaults to `NULL`.
#'
#' @details
#' The function retrieves entries from the `config_log` table in the current configuration.
#' The function applies any date filters to the `timestamp` field of the log entries.
#'
#' @return A `data.table` containing the filtered log entries.
#'
#' @examples
#' \dontrun{
#' # Get all log entries
#' get_config_log()
#'
#' # Get logs for a specific surveillance system
#' get_config_log(ss = "weather")
#'
#' # Get logs for a specific task and date range
#' get_config_log(task = "data_import", start_date = "2024-01-01", end_date = "2024-12-31")
#' }
#'
#' @export
get_config_log <- function(
  ss = NULL,
  task = NULL,
  start_date = NULL,
  end_date = NULL
) {
  # Retrieve the entire config_log table
  log_data <- config$tables$config_log$tbl() |>
    dplyr::collect() |>
    setDT()

  # Inside `[.data.table`, `ss` and `task` name the columns. So the
  # arguments are copied to names that no column has.
  x_ss <- ss
  x_task <- task
  if (!is.null(x_ss)) {
    log_data <- log_data[ss == x_ss]
  }

  if (!is.null(x_task)) {
    log_data <- log_data[task == x_task]
  }

  if (!is.null(start_date)) {
    log_data <- log_data[as.Date(date) >= as.Date(start_date)]
  }

  if (!is.null(end_date)) {
    log_data <- log_data[as.Date(date) <= as.Date(end_date)]
  }

  # Return the filtered data.table
  return(log_data)
}
