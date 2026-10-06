# `table_name` is a character vector. The function writes one row per name in
# one upsert. A partitioned table records every child in one call this way.
update_config_tables_last_updated <- function(
  table_name,
  date = NULL,
  datetime = NULL
) {
  if (!is.null(datetime)) {
    datetime <- as.character(datetime)
  }

  if (is.null(date) && is.null(datetime)) {
    date <- lubridate::today()
    datetime <- cstime::now_c()
  }
  if (is.null(date) && !is.null(datetime)) {
    date <- stringr::str_sub(datetime, 1, 10)
  }
  if (!is.null(date) && is.null(datetime)) {
    datetime <- paste0(date, " 00:01:00")
  }

  # Each name keeps only the part after its last "].[".
  table_cleaned <- vapply(
    stringr::str_split(table_name, "].\\["),
    function(x) x[length(x)],
    character(1)
  )

  to_upload <- data.table(
    table_name = table_cleaned,
    date = date,
    datetime = datetime
  )
  return(config$tables$config_tables_last_updated$upsert_data(to_upload))
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
#'   table_name, last_updated_datetime, and other tracking metadata.
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
    temp <- config$tables$config_tables_last_updated$tbl() |>
      dplyr::filter(table_name == !!table_name) |>
      dplyr::collect() |>
      as.data.table()
  } else {
    temp <- config$tables$config_tables_last_updated$tbl() |>
      dplyr::collect() |>
      as.data.table()
  }
  return(temp)
}
