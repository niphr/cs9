update_config_tables_last_updated <- function(table_name, date = NULL, datetime = NULL) {
  if (!is.null(datetime)) datetime <- as.character(datetime)

  if (is.null(date) & is.null(datetime)) {
    date <- lubridate::today()
    datetime <- cstime::now_c()
  }
  if (is.null(date) & !is.null(datetime)) {
    date <- stringr::str_sub(datetime, 1, 10)
  }
  if (!is.null(date) & is.null(datetime)) {
    datetime <- paste0(date, " 00:01:00")
  }

  table_cleaned <- stringr::str_split(table_name, "].\\[")[[1]]
  table_cleaned <- table_cleaned[length(table_cleaned)]

  to_upload <- data.table(
    table_name = table_cleaned,
    date = date,
    datetime = datetime
  )
  config$tables$config_tables_last_updated$upsert_data(to_upload)
}


#' Get Configuration Tables Last Updated
#'
#' Retrieves the last updated timestamps for database tables from the 
#' configuration tracking system.
#'
#' @param table_name Character string specifying the table name to filter by.
#'   If NULL, returns data for all tables.
#'
#' @return A data.table containing last updated information with columns:
#'   table_name, last_updated_datetime, and other tracking metadata
#'
#' @examples
#' \dontrun{
#' # Get last updated info for all tables
#' get_config_tables_last_updated()
#' 
#' # Get info for a specific table
#' get_config_tables_last_updated(table_name = "anon_covid_cases")
#' }
#'
#' @export
get_config_tables_last_updated <- function(table_name = NULL) {
  if (!is.null(table_name)) {
    temp <- config$tables$config_tables_last_updated$tbl() %>%
      dplyr::filter(table_name == !!table_name) %>%
      dplyr::collect() %>%
      as.data.table()
  } else {
    temp <- config$tables$config_tables_last_updated$tbl() %>%
      dplyr::collect() %>%
      as.data.table()
  }
  return(temp)
}
