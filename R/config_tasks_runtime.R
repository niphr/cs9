update_config_tasks_stats <- function(
    ss = "unspecified",
    task,
    implementation_version = "unspecified",
    cores_n,
    plans_n,
    analyses_n,
    start_datetime,
    stop_datetime,
    ram_all_cores_mb,
    ram_per_core_mb,
    status
) {
  stopifnot(status %in% c("succeeded", "failed"))

  runtime_minutes <- round(as.numeric(difftime(stop_datetime, start_datetime, units = "min")), 2)

  start_datetime <- format(start_datetime, "%Y-%m-%d %H:%M:%S")
  start_date <- stringr::str_sub(start_datetime, 1, 10)

  stop_datetime <- format(stop_datetime, "%Y-%m-%d %H:%M:%S")
  stop_date <- stringr::str_sub(stop_datetime, 1, 10)

  to_upload <- data.table(
    auto_interactive = ifelse(config$is_auto, "auto", "interactive"),
    ss = ss,
    task = task,
    cs_version = utils::packageDescription("cs9", fields = "Version"),
    implementation_version = implementation_version,
    cores_n = cores_n,
    plans_n = plans_n,
    analyses_n = analyses_n,
    start_date = start_date,
    start_datetime = start_datetime,
    stop_date = stop_date,
    stop_datetime = stop_datetime,
    runtime_minutes = runtime_minutes,
    ram_all_cores_mb = ram_all_cores_mb,
    ram_per_core_mb = ram_per_core_mb,
    status = status
  )
  config$tables$config_tasks_stats$upsert_data(to_upload)
}

#' Get Configuration Tasks Statistics
#'
#' Retrieves runtime statistics and performance metrics for surveillance tasks
#' from the configuration database.
#'
#' @param task Character string specifying the task name to filter by. If NULL,
#'   returns statistics for all tasks.
#' @param last_run Logical value indicating whether to return only the most
#'   recent run statistics. If FALSE, returns all historical data.
#'
#' @return A data.table containing task statistics with columns including:
#'   task, datetime, runtime_seconds, memory_usage, status, and other metrics
#'
#' @examples
#' \dontrun{
#' # Get all task statistics
#' get_config_tasks_stats()
#' 
#' # Get statistics for a specific task
#' get_config_tasks_stats(task = "covid_analysis")
#' 
#' # Get only the last run for a task
#' get_config_tasks_stats(task = "covid_analysis", last_run = TRUE)
#' }
#'
#' @export
get_config_tasks_stats <- function(task = NULL, last_run = FALSE) {
  if (!is.null(task)) {
    temp <- config$tables$config_tasks_stats$tbl() %>%
      dplyr::filter(task == !!task) %>%
      dplyr::collect() %>%
      as.data.table()
  } else {
    temp <- config$tables$config_tasks_stats$tbl() %>%
      dplyr::collect() %>%
      as.data.table()
  }
  return(temp)
}
