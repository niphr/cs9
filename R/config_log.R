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

  datetime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
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
  config$tables$config_log$insert_data(to_upload)
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
  log_data <- config$tables$config_log$tbl() %>%
    dplyr::collect() %>%
    setDT()

  # Apply filtering if parameters are provided
  if (!is.null(ss)) {
    log_data <- log_data[ss == get(ss)]
  }

  if (!is.null(task)) {
    log_data <- log_data[task == get(task)]
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
