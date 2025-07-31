.onLoad <- function(libname, pkgname) {
  # Initialize environment variables with error handling
  env_result <- tryCatch({
    set_env_vars()
    TRUE
  }, error = function(e) {
    packageStartupMessage("Warning: Environment variable setup failed: ", e$message)
    FALSE
  })
  
  # Set up progress bars and plnr options (non-critical)
  set_progressr()
  set_plnr()

  # Only attempt database setup if environment variables were set successfully
  if(!env_result || length(config$dbconfigs) == 0){
    packageStartupMessage("CS9 database configuration not available. Package loaded with limited functionality.")
    packageStartupMessage("Use cs9::check_environment_setup() to diagnose configuration issues.")
  } else {
    # Attempt database table setup with error handling
    db_result <- tryCatch({
      setup_database_tables()
      TRUE
    }, error = function(e) {
      packageStartupMessage("Warning: Database table setup failed: ", e$message)
      packageStartupMessage("CS9 loaded with limited functionality. Check database connectivity.")
      FALSE
    })
  }

  invisible()
}

# Database table setup function (extracted from .onLoad)
setup_database_tables <- function() {
  # config_log ----
  config$tables$config_log <- csdb::DBTable_v9$new(
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
  config$tables$config_tables_last_updated <- csdb::DBTable_v9$new(
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
  config$tables$config_tasks_stats <- csdb::DBTable_v9$new(
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
  config$tables$config_data_hash_for_each_plan <- csdb::DBTable_v9$new(
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

  retval$id <- paste0("[", retval$db, "].[", retval$schema, "]")

  return(retval)
}

set_env_vars <- function(){
  config$dbconfigs <- list()
  
  # Get database access configuration with fallback
  db_access <- tryCatch({
    access_list <- get_db_acess_from_env()
    if(length(access_list) == 0) {
      stop("No CS9_DBCONFIG_ACCESS environment variable found")
    }
    access_list
  }, error = function(e) {
    # Provide fallback for missing access configuration
    character(0)
  })
  
  for (i in db_access) {
    config$dbconfigs[[i]] <- get_db_from_env(i)
  }

  # Set auto mode with fallback
  config$is_auto <- isTRUE(Sys.getenv("CS9_AUTO") == "1")

  # Set path with fallback to empty string
  config$path <- Sys.getenv("CS9_PATH", unset = "")
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

#' Check Environment Setup
#'
#' Diagnostic function to check CS9 environment configuration
#'
#' @return A list containing environment setup status and recommendations
#' @export
check_environment_setup <- function() {
  result <- list(
    status = "ok",
    issues = character(0),
    recommendations = character(0)
  )
  
  # Check required environment variables
  required_vars <- c(
    "CS9_DBCONFIG_ACCESS",
    "CS9_DBCONFIG_DRIVER", 
    "CS9_DBCONFIG_SERVER",
    "CS9_DBCONFIG_USER"
  )
  
  missing_vars <- character(0)
  for(var in required_vars) {
    if(Sys.getenv(var) == "") {
      missing_vars <- c(missing_vars, var)
    }
  }
  
  if(length(missing_vars) > 0) {
    result$status <- "error"
    result$issues <- c(result$issues, paste("Missing environment variables:", paste(missing_vars, collapse = ", ")))
    result$recommendations <- c(result$recommendations, 
      "Set required environment variables before loading CS9",
      "Refer to CS9 documentation for environment setup instructions"
    )
  }
  
  # Check database configuration
  if(length(config$dbconfigs) == 0) {
    result$status <- if(result$status == "ok") "warning" else result$status
    result$issues <- c(result$issues, "No database configurations available")
    result$recommendations <- c(result$recommendations, "Verify CS9_DBCONFIG_ACCESS is properly set")
  }
  
  # Check database table availability
  if(length(config$tables) == 0) {
    result$status <- if(result$status == "ok") "warning" else result$status  
    result$issues <- c(result$issues, "No configuration tables initialized")
    result$recommendations <- c(result$recommendations, "Check database connectivity and permissions")
  }
  
  # Print results
  cat("CS9 Environment Check Results:\n")
  cat("Status:", result$status, "\n\n")
  
  if(length(result$issues) > 0) {
    cat("Issues found:\n")
    for(issue in result$issues) {
      cat(" -", issue, "\n")
    }
    cat("\n")
  }
  
  if(length(result$recommendations) > 0) {
    cat("Recommendations:\n")
    for(rec in result$recommendations) {
      cat(" -", rec, "\n") 
    }
    cat("\n")
  }
  
  if(result$status == "ok") {
    cat("✓ CS9 environment is properly configured\n")
  }
  
  invisible(result)
}