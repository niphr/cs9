.onLoad <- function(libname, pkgname) {
  # Silent setup only - no user messages

  # Set up progress bars and plnr options (non-critical)
  set_progressr()
  set_plnr()

  # Attempt environment variable setup silently
  tryCatch(
    {
      set_env_vars()
    },
    error = function(e) {
      # Silent failure - diagnostics available via check_environment_setup()
    }
  )

  # Attempt database table setup silently
  tryCatch(
    {
      if (length(config$dbconfigs) > 0) {
        setup_database_tables()
      }
    },
    error = function(e) {
      # Silent failure - diagnostics available via check_environment_setup()
    }
  )

  invisible()
}


# Database table setup function (extracted from .onLoad)
#
# The four tables are built into a local list and assigned to config$tables in
# one statement at the end. Assigning each one directly into config$tables would
# leave a partially-populated table list behind if the third constructor failed,
# and a caller cannot tell a partial list from a complete one.
setup_database_tables <- function() {
  tables <- list()

  # config_log ----
  tables$config_log <- csdb::DBTable_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "config_log",
    field_types = c(
      "auto_interactive" = "TEXT",
      "ss" = "TEXT",
      "task" = "TEXT",
      "date" = "DATE",
      "datetime" = "DATETIME",
      "message" = "TEXT"
    ),
    keys = c(
      "auto_interactive",
      "ss",
      "task",
      "datetime"
    ),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )

  # config_tables_last_updated ----
  tables$config_tables_last_updated <- csdb::DBTable_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "config_tables_last_updated",
    field_types = c(
      "table_name" = "TEXT",
      "date" = "DATE",
      "datetime" = "DATETIME"
    ),
    keys = c(
      "table_name"
    ),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )

  # config_tasks_stats ----
  tables$config_tasks_stats <- csdb::DBTable_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "config_tasks_stats",
    field_types = c(
      "auto_interactive" = "TEXT",
      "ss" = "TEXT",
      "task" = "TEXT",
      "cs_version" = "TEXT",
      "implementation_version" = "TEXT",
      "cores_n" = "INTEGER",
      "plans_n" = "INTEGER",
      "analyses_n" = "INTEGER",
      "start_date" = "DATE",
      "start_datetime" = "DATETIME",
      "stop_date" = "DATE",
      "stop_datetime" = "DATETIME",
      "runtime_minutes" = "DOUBLE",
      "ram_all_cores_mb" = "DOUBLE",
      "ram_per_core_mb" = "DOUBLE",
      "status" = "TEXT"
    ),
    keys = c(
      "auto_interactive",
      "ss",
      "task",
      "start_datetime"
    ),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )

  # config_data_hash_for_each_plan ----
  tables$config_data_hash_for_each_plan <- csdb::DBTable_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "config_data_hash_for_each_plan",
    field_types = c(
      "task" = "TEXT",
      "index_plan" = "INTEGER",
      "element_tag" = "TEXT",
      "date" = "DATE",
      "datetime" = "DATETIME",
      "element_hash" = "TEXT",
      "all_hash" = "TEXT"
    ),
    keys = c(
      "task",
      "index_plan",
      "element_tag",
      "date",
      "datetime"
    ),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )

  # all four constructors succeeded, so the swap is safe
  config$tables <- tables

  invisible(NULL)
}

# Environmental variables ----
get_db_acess_from_env <- function() {
  retval <- Sys.getenv("CS9_DBCONFIG_ACCESS") |>
    stringr::str_split("/") |>
    unlist()
  retval <- retval[retval != ""]
  return(retval)
}

get_db_from_env <- function(access) {
  retval <- list(
    access = access,
    driver = Sys.getenv("CS9_DBCONFIG_DRIVER"),
    port = as.integer(Sys.getenv("CS9_DBCONFIG_PORT")),
    user = Sys.getenv("CS9_DBCONFIG_USER"),
    password = Sys.getenv("CS9_DBCONFIG_PASSWORD"),
    trusted_connection = Sys.getenv("CS9_DBCONFIG_TRUSTED_CONNECTION"),
    sslmode = Sys.getenv("CS9_DBCONFIG_SSLMODE"),
    role_create_table = Sys.getenv("CS9_DBCONFIG_ROLE_CREATE_TABLE"),
    server = Sys.getenv("CS9_DBCONFIG_SERVER"),
    schema = Sys.getenv(paste0("CS9_DBCONFIG_SCHEMA_", toupper(access))),
    db = Sys.getenv(paste0("CS9_DBCONFIG_DB_", toupper(access)))
  )

  retval$schema <- gsub("\\\\", "\\\\", retval$schema)

  if (toupper(retval$driver) == "SQLITE") {
    # SQLite is a file and has no schemas, so the identifier that names the
    # database is the file path itself.
    retval$id <- retval$db
  } else {
    retval$id <- paste0("[", retval$db, "].[", retval$schema, "]")
  }

  return(retval)
}

set_env_vars <- function() {
  config$dbconfigs <- list()

  # Get database access configuration with fallback
  db_access <- tryCatch(
    {
      access_list <- get_db_acess_from_env()
      if (length(access_list) == 0) {
        stop("No CS9_DBCONFIG_ACCESS environment variable found")
      }
      access_list
    },
    error = function(e) {
      # Provide fallback for missing access configuration
      character(0)
    }
  )

  for (i in db_access) {
    config$dbconfigs[[i]] <- get_db_from_env(i)
  }

  # Set auto mode with fallback
  config$is_auto <- isTRUE(Sys.getenv("CS9_AUTO") == "1")

  # Set path with fallback to empty string
  config$path <- Sys.getenv("CS9_PATH", unset = "")
}

#' Reload the database configuration from the environment
#'
#' @description
#' Re-reads every `CS9_DBCONFIG_*` environment variable and rebuilds the four
#' configuration tables.
#'
#' @details
#' `cs9` reads the environment once, in its own `.onLoad()`. A package that sets
#' its own `CS9_DBCONFIG_*` values inside `.onLoad()` therefore sets them too
#' late. `cs9` is a dependency, so `cs9` loads first. `cs9` reads the
#' environment before the dependent package runs. Call this function
#' immediately after the `Sys.setenv()` block. `cs9` then picks the new values
#' up.
#'
#' It re-runs `set_env_vars()`, which rebuilds `config$dbconfigs`, and then
#' `setup_database_tables()`, which rebuilds `config$tables`. Neither opens a
#' database connection: `csdb::DBTable_v9` creates its table lazily, on first
#' use.
#'
#' @section State safety:
#' The reload is safe against both a failed reload and a repeated one.
#'
#' It disconnects every table it is about to discard, so a second reload does
#' not leak the connections held by the R6 objects it replaces.
#'
#' It then empties `config$tables` before it touches `config$dbconfigs`. A
#' failure anywhere after that point therefore leaves `config$tables` empty
#' rather than holding tables built from the previous configuration. An empty
#' table list is an honest "not configured"; a stale one describes a database
#' that `config$dbconfigs` no longer points at.
#'
#' @return `invisible(NULL)`, called for its effect on `config`.
#' @export
#' @seealso [check_environment_setup()], which reports whether the environment
#'   is complete.
#'   The installation vignette, `vignette("installation", package = "cs9")`.
#' @examples
#' \dontrun{
#' # In a dependent package's .onLoad(), before its own setup runs
#' Sys.setenv(CS9_DBCONFIG_ACCESS = "config/anon")
#' Sys.setenv(CS9_DBCONFIG_DRIVER = "SQLite")
#' Sys.setenv(CS9_DBCONFIG_DB_CONFIG = "/tmp/config.sqlite")
#' Sys.setenv(CS9_DBCONFIG_DB_ANON = "/tmp/anon.sqlite")
#' cs9::reload_db_config()
#' }
reload_db_config <- function() {
  # release the connections held by the objects about to be discarded
  for (tab in config$tables) {
    tryCatch(tab$disconnect(), error = function(e) invisible(NULL))
  }

  # empty first, so a failure below leaves an honest "not configured" rather
  # than tables describing the previous configuration
  config$tables <- list()

  set_env_vars()
  setup_database_tables()

  invisible(NULL)
}

set_progressr <- function() {
  options("progressr.enable" = TRUE)
  progressr::handlers(
    progressr::handler_progress(
      format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta\n",
      clear = FALSE
    )
  )
}

set_plnr <- function() {
  plnr::set_opts(force_verbose = TRUE)
}

# Internal helper function for environment validation
validate_environment <- function() {
  result <- list(
    status = "ok",
    issues = character(0),
    recommendations = character(0)
  )

  # Tier 1: Always required variables
  # CS9_DBCONFIG_PORT and CS9_DBCONFIG_SERVER are not here: SQLite is a file
  # and reads neither. They are required by tier 2 instead.
  always_required <- c(
    "CS9_AUTO",
    "CS9_PATH",
    "CS9_DBCONFIG_ACCESS",
    "CS9_DBCONFIG_DRIVER"
  )

  missing_vars <- character(0)
  for (var in always_required) {
    if (Sys.getenv(var) == "") {
      missing_vars <- c(missing_vars, var)
    }
  }

  if (length(missing_vars) > 0) {
    result$status <- "error"
    result$issues <- c(
      result$issues,
      paste(
        "Missing required environment variables:",
        paste(missing_vars, collapse = ", ")
      )
    )
    result$recommendations <- c(
      result$recommendations,
      "Set required environment variables before loading CS9",
      "Refer to CS9 documentation for environment setup instructions"
    )
  }

  driver <- Sys.getenv("CS9_DBCONFIG_DRIVER")
  is_sqlite <- toupper(driver) == "SQLITE"

  # Tier 2: server-based variables (every driver except SQLite)
  if (!is_sqlite) {
    server_required <- c(
      "CS9_DBCONFIG_PORT",
      "CS9_DBCONFIG_SERVER"
    )

    missing_server_vars <- character(0)
    for (var in server_required) {
      if (Sys.getenv(var) == "") {
        missing_server_vars <- c(missing_server_vars, var)
      }
    }

    if (length(missing_server_vars) > 0) {
      result$status <- "error"
      result$issues <- c(
        result$issues,
        paste(
          "Missing server-specific variables:",
          paste(missing_server_vars, collapse = ", ")
        )
      )
      result$recommendations <- c(
        result$recommendations,
        "Every driver except SQLite connects to a database server",
        "Set CS9_DBCONFIG_SERVER and CS9_DBCONFIG_PORT, or use CS9_DBCONFIG_DRIVER=SQLite"
      )
    }
  }

  # Tier 2b: SQLite-specific variables (matched case-insensitively, as csdb does)
  # SQLite ignores CS9_DBCONFIG_USER, CS9_DBCONFIG_PASSWORD and every
  # CS9_DBCONFIG_SCHEMA_*, so none of them is required here.
  if (is_sqlite) {
    accesses <- get_db_acess_from_env()

    # setup_database_tables() reads config$dbconfigs$config unconditionally, so
    # an access list without "config" fails later and obscurely.
    if (!"config" %in% accesses) {
      result$status <- "error"
      result$issues <- c(
        result$issues,
        "CS9_DBCONFIG_ACCESS must include 'config'"
      )
      result$recommendations <- c(
        result$recommendations,
        "The four configuration tables are built from the 'config' access",
        "Set CS9_DBCONFIG_ACCESS to a '/'-separated list that includes config"
      )
    }

    sqlite_required <- paste0("CS9_DBCONFIG_DB_", toupper(accesses))

    missing_sqlite_vars <- character(0)
    for (var in sqlite_required) {
      if (Sys.getenv(var) == "") {
        missing_sqlite_vars <- c(missing_sqlite_vars, var)
      }
    }

    if (length(missing_sqlite_vars) > 0) {
      result$status <- "error"
      result$issues <- c(
        result$issues,
        paste(
          "Missing SQLite-specific variables:",
          paste(missing_sqlite_vars, collapse = ", ")
        )
      )
      result$recommendations <- c(
        result$recommendations,
        "SQLite stores each access in its own file",
        "Set CS9_DBCONFIG_DB_<ACCESS> to a file path for every access in CS9_DBCONFIG_ACCESS"
      )
    }
  }

  # Tier 3: PostgreSQL-specific variables (only if using PostgreSQL Unicode driver)
  if (driver == "PostgreSQL Unicode") {
    postgresql_required <- c(
      "CS9_DBCONFIG_USER",
      "CS9_DBCONFIG_PASSWORD",
      "CS9_DBCONFIG_SCHEMA_CONFIG",
      "CS9_DBCONFIG_DB_CONFIG",
      "CS9_DBCONFIG_SCHEMA_ANON",
      "CS9_DBCONFIG_DB_ANON"
    )

    missing_postgresql_vars <- character(0)
    for (var in postgresql_required) {
      if (Sys.getenv(var) == "") {
        missing_postgresql_vars <- c(missing_postgresql_vars, var)
      }
    }

    if (length(missing_postgresql_vars) > 0) {
      result$status <- "error"
      result$issues <- c(
        result$issues,
        paste(
          "Missing PostgreSQL-specific variables:",
          paste(missing_postgresql_vars, collapse = ", ")
        )
      )
      result$recommendations <- c(
        result$recommendations,
        "PostgreSQL Unicode driver requires additional authentication and schema variables",
        "Set PostgreSQL-specific environment variables for database access"
      )
    }
  }

  return(result)
}

#' Check Environment Setup
#'
#' @description
#' Diagnostic function that checks the CS9 environment configuration. This
#' function checks the required environment variables and the database
#' connectivity. It reports detailed feedback that helps you troubleshoot
#' configuration problems.
#'
#' @details
#' CS9 needs specific environment variables for database connectivity and
#' configuration. This function checks for:
#' \itemize{
#'   \item Required environment variables (CS9_DBCONFIG_ACCESS, CS9_DBCONFIG_DRIVER, etc.).
#'   \item Database configuration availability.
#'   \item Database table initialization status.
#' }
#'
#' When you install CS9 from CRAN without a database configuration, the package
#' loads with limited functionality. Use this function to identify what you
#' must configure for full database-driven surveillance functionality.
#'
#' @param verbose Logical. If TRUE (default), prints detailed diagnostic output.
#'   If FALSE, runs the check silently and only returns the result object.
#' @param use_startup_message Logical. If TRUE, uses packageStartupMessage()
#'   for output (suppressible). If FALSE (default), uses cat() for console output.
#' @return A list containing environment setup status and recommendations:
#' \itemize{
#'   \item status: "ok", "warning", or "error".
#'   \item issues: Character vector of identified problems.
#'   \item recommendations: Character vector of suggested fixes.
#' }
#'
#' @examples
#' # Check environment setup with verbose output
#' check_environment_setup()
#'
#' # Check silently and examine results
#' result <- check_environment_setup(verbose = FALSE)
#' if(result$status != "ok") {
#'   cat("Issues found:", result$issues, "\n")
#'   cat("Recommendations:", result$recommendations, "\n")
#' }
#'
#' @seealso
#' The installation vignette: \code{vignette("installation", package = "cs9")}.
#'
#' @export
check_environment_setup <- function(
  verbose = TRUE,
  use_startup_message = FALSE
) {
  # Run core validation
  result <- validate_environment()

  # Add runtime checks (database configs and tables) - only relevant after set_env_vars() has run
  if (length(config$dbconfigs) == 0) {
    result$status <- if (result$status == "ok") "warning" else result$status
    result$issues <- c(result$issues, "No database configurations available")
    result$recommendations <- c(
      result$recommendations,
      "Verify CS9_DBCONFIG_ACCESS is properly set"
    )
  }

  if (length(config$tables) == 0) {
    result$status <- if (result$status == "ok") "warning" else result$status
    result$issues <- c(result$issues, "No configuration tables initialized")
    result$recommendations <- c(
      result$recommendations,
      "Check database connectivity and permissions"
    )
  }

  # Print results only if verbose mode
  if (verbose) {
    # Choose output method based on context
    output_fn <- if (use_startup_message) packageStartupMessage else cat

    output_fn("CS9 Environment Check Results:\n")
    output_fn("Status: ", result$status, "\n\n")

    if (length(result$issues) > 0) {
      output_fn("Issues found:\n")
      for (issue in result$issues) {
        output_fn(" - ", issue, "\n")
      }
      output_fn("\n")
    }

    if (length(result$recommendations) > 0) {
      output_fn("Recommendations:\n")
      for (rec in result$recommendations) {
        output_fn(" - ", rec, "\n")
      }
      output_fn("\n")
    }

    if (result$status == "ok") {
      output_fn("CS9 environment is properly configured\n")
    }
  }

  invisible(result)
}
