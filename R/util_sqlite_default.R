#' Use SQLite databases in one directory as the database configuration
#'
#' @description
#' Points every access in `access` at a SQLite file in `dir` and rebuilds the
#' four configuration tables. It reads and sets no environment variable.
#'
#' @details
#' Access `a` uses the file `fs::path(dir, paste0(a, ".sqlite"))`. The function
#' creates `dir` if it does not exist. Each entry of `config$dbconfigs` has the
#' fields and the `id` rule that `CS9_DBCONFIG_DRIVER=SQLite` gives through
#' [reload_db_config()]. The fields that SQLite does not read are empty
#' strings, and `port` is `NA_integer_`.
#'
#' The function sets `config$path` to `dir` and `config$is_auto` to `FALSE`.
#' It does not change `config$share`.
#'
#' A child R process does not inherit this configuration, because no
#' environment variable carries it. Call it in a package's `.onLoad()`. Each
#' child R process that loads that package then calls it again, with the
#' child's own arguments.
#'
#' @section State safety:
#' The function disconnects every table in `config$tables` and empties the
#' list before it changes `config$dbconfigs`. A failure after that point leaves
#' `config$tables` empty. `access` MUST contain `"config"`, because the four
#' configuration tables use the `config` access.
#'
#' @param dir The directory that holds the SQLite files.
#' @param access The access names, one SQLite file for each.
#' @return `invisible(NULL)`, called for its effect on `config`.
#' @export
#' @seealso [reload_db_config()], which reads the configuration from the
#'   `CS9_DBCONFIG_*` environment variables.
#' @examples
#' \dontrun{
#' # In a dependent package's .onLoad()
#' cs9::use_sqlite_default(file.path(tempdir(), "cs9"))
#' }
use_sqlite_default <- function(dir, access = c("config", "anon")) {
  stopifnot(is.character(dir), length(dir) == 1L, !is.na(dir))
  stopifnot(is.character(access), length(access) >= 1L, !anyNA(access))

  fs::dir_create(dir)

  discard_tables()

  dbconfigs <- list()
  for (a in access) {
    db <- as.character(fs::path(dir, paste0(a, ".sqlite")))
    dbconfigs[[a]] <- new_dbconfig(
      access = a,
      driver = "SQLite",
      port = NA_integer_,
      user = "",
      password = "",
      trusted_connection = "",
      sslmode = "",
      role_create_table = "",
      server = "",
      schema = "",
      db = db
    )
  }
  config$dbconfigs <- dbconfigs
  config$path <- dir
  config$is_auto <- FALSE

  setup_database_tables()

  return(invisible(NULL))
}
