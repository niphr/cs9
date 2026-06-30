#' Filter surveillance data by standard epidemiological dimensions
#'
#' Applies a set of standard include/exclude filters to a database table or
#' data frame using the conventional surveillance field names. Each \code{NULL}
#' argument is a no-op, so callers only need to supply the dimensions they
#' actually want to restrict.
#'
#' @param .data A data frame, \code{data.table}, or remote database table (e.g.
#'   the result of \code{dplyr::tbl()}) that contains the standard surveillance
#'   columns.
#' @param granularity_time Character vector. Values of \code{granularity_time}
#'   to keep. \code{NULL} (default) applies no filter.
#' @param granularity_time_not Character vector. Values of
#'   \code{granularity_time} to drop. \code{NULL} (default) applies no filter.
#' @param granularity_geo Character vector. Values of \code{granularity_geo} to
#'   keep. \code{NULL} (default) applies no filter.
#' @param granularity_geo_not Character vector. Values of
#'   \code{granularity_geo} to drop. \code{NULL} (default) applies no filter.
#' @param country_iso3 Character vector. Values of \code{country_iso3} to keep.
#'   \code{NULL} (default) applies no filter.
#' @param location_code Character vector. Values of \code{location_code} to
#'   keep. \code{NULL} (default) applies no filter.
#' @param age Character vector. Values of \code{age} to keep. \code{NULL}
#'   (default) applies no filter.
#' @param age_not Character vector. Values of \code{age} to drop. \code{NULL}
#'   (default) applies no filter.
#' @param sex Character vector. Values of \code{sex} to keep. \code{NULL}
#'   (default) applies no filter.
#' @param sex_not Character vector. Values of \code{sex} to drop. \code{NULL}
#'   (default) applies no filter.
#'
#' @return The filtered object in the same class as \code{.data}.
#'
#' @examples
#' \dontrun{
#' # Inside a data_selector function, filter a remote table before collecting:
#' d <- schema$anon_covid_cases$tbl() |>
#'   mandatory_db_filter(
#'     granularity_time = "isoweek",
#'     granularity_geo  = "county",
#'     age_not          = "total"
#'   ) |>
#'   dplyr::collect()
#' }
#'
#' @export
mandatory_db_filter <- function(.data,
                                granularity_time = NULL,
                                granularity_time_not = NULL,
                                granularity_geo = NULL,
                                granularity_geo_not = NULL,
                                country_iso3 = NULL,
                                location_code = NULL,
                                age = NULL,
                                age_not = NULL,
                                sex = NULL,
                                sex_not = NULL) {
  retval <- .data

  if (!is.null(granularity_time)) retval <- retval %>% dplyr::filter(granularity_time %in% !!granularity_time)
  if (!is.null(granularity_time_not)) retval <- retval %>% dplyr::filter(!granularity_time %in% !!granularity_time_not)

  if (!is.null(granularity_geo)) retval <- retval %>% dplyr::filter(granularity_geo %in% !!granularity_geo)
  if (!is.null(granularity_geo_not)) retval <- retval %>% dplyr::filter(!granularity_geo %in% !!granularity_geo_not)

  if (!is.null(country_iso3)) retval <- retval %>% dplyr::filter(!country_iso3 %in% !!country_iso3)

  if (!is.null(location_code)) retval <- retval %>% dplyr::filter(location_code %in% !!location_code)

  if (!is.null(age)) retval <- retval %>% dplyr::filter(age %in% !!age)
  if (!is.null(age_not)) retval <- retval %>% dplyr::filter(!age %in% !!age_not)

  if (!is.null(sex)) retval <- retval %>% dplyr::filter(sex %in% !!sex)
  if (!is.null(sex_not)) retval <- retval %>% dplyr::filter(!sex %in% !!sex_not)

  return(retval)
}
