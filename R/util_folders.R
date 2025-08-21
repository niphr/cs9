#' Get Results Folder Path
#'
#' Constructs the appropriate folder path for surveillance system results,
#' with automatic switching between production and interactive modes.
#'
#' @param ... Character strings specifying the second level directory and beyond
#' @param create_dir Logical value indicating whether to create the directory
#'   if it doesn't exist. Defaults to FALSE.
#' @param trailing_slash Logical value indicating whether to add a trailing 
#'   slash to the returned path. Defaults to FALSE.
#' @param auto Logical value indicating whether this is running in automatic
#'   mode (uses base directory) or interactive mode (adds "_interactive" subdirectory).
#'   Defaults to the current cs9::config$is_auto setting.
#'
#' @return Character string containing the constructed file path
#'
#' @examples
#' \dontrun{
#' # Get basic output path
#' path("reports", "daily")
#' 
#' # Create directory if it doesn't exist
#' path("reports", "daily", create_dir = TRUE)
#' 
#' # Get path with trailing slash
#' path("reports", "daily", trailing_slash = TRUE)
#' }
#'
#' @export
path <- function(..., create_dir = FALSE, trailing_slash = FALSE, auto = cs9::config$is_auto) {
  end_location <- glue::glue(fs::path(...), .envir = parent.frame(n = 1))
  end_location <- stringr::str_split(end_location, "/")[[1]]
  end_location <- end_location[end_location != ""]
  if (!auto) {
    if (length(end_location) == 1) {
      end_location <- c(end_location[1], "_interactive")
    } else if (length(end_location) >= 2) {
      end_location <- c(end_location[1], "_interactive", end_location[2:length(end_location)])
    }
  }

  retval <- paste0(c(config$path, end_location), collapse = "/")
  if (create_dir) {
    if (!fs::dir_exists(retval)) dir.create(retval, showWarnings = FALSE, recursive = TRUE)
  }
  if (trailing_slash) retval <- paste0(retval, "/")
  return(retval)
}

#' Create Folder If It Doesn't Exist
#'
#' Creates a directory and all necessary parent directories if they don't
#' already exist.
#'
#' @param path Character string specifying the directory path to create
#'
#' @return Character string containing the created directory path
#'
#' @examples
#' \dontrun{
#' # Create a new directory
#' create_folder_if_doesnt_exist("/tmp/my_analysis/results")
#' }
#'
#' @export
create_folder_if_doesnt_exist <- function(path) {
  retval <- glue::glue(path, .envir = parent.frame(n = 1))
  if (!fs::dir_exists(retval)) dir.create(retval, showWarnings = FALSE, recursive = TRUE)
  return(retval)
}

#' Create Latest Folder
#'
#' Copies results from a dated folder to a "latest" folder, providing
#' easy access to the most recent analysis results.
#'
#' @param results_folder_name Character string specifying the name of the 
#'   results folder (subdirectory under "output")
#' @param date Character string specifying the date of extraction (used to
#'   identify the source folder)
#'
#' @return No return value. This function is called for its side effect of
#'   copying files from the dated folder to the latest folder.
#'
#' @details
#' This function copies all contents from \code{output/results_folder_name/date}
#' to \code{output/results_folder_name/latest}, overwriting existing files.
#'
#' @examples
#' \dontrun{
#' # Copy today's results to latest folder
#' create_latest_folder("covid_reports", "2024-01-15")
#' }
#'
#' @export
create_latest_folder <- function(results_folder_name, date) {
  from_folder <- path("output", results_folder_name, date)
  to_folder <- path("output", results_folder_name, "latest")
  processx::run("cp", c("-rT", from_folder, to_folder))
}
