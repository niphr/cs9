# use_sqlite_default() sets the SQLite default from arguments. It MUST touch no
# environment variable, so a child R process inherits nothing from it.

# Save the cs9 configuration and restore it when `envir` exits, so later test
# files see the configuration they would have seen without this file. Call it
# after withr::local_tempdir(): the restore closes the connections before the
# directory is deleted.
local_cs9_config <- function(envir = parent.frame()) {
  saved <- list(
    dbconfigs = config$dbconfigs,
    path = config$path,
    is_auto = config$is_auto
  )
  withr::defer(
    {
      discard_tables()
      config$dbconfigs <- saved$dbconfigs
      config$path <- saved$path
      config$is_auto <- saved$is_auto
      if ("config" %in% names(saved$dbconfigs)) {
        setup_database_tables()
      }
    },
    envir = envir
  )
  return(invisible(saved))
}

# The CS9_* variables of the current process, as a named character vector.
cs9_envvars <- function() {
  e <- Sys.getenv()
  e <- e[startsWith(names(e), "CS9_")]
  return(stats::setNames(as.character(e), names(e))[order(names(e))])
}

# The CS9_* variables that a fresh child R process sees.
child_cs9_envvars <- function() {
  return(callr::r(function() {
    e <- Sys.getenv()
    e <- e[startsWith(names(e), "CS9_")]
    return(stats::setNames(as.character(e), names(e))[order(names(e))])
  }))
}

sqlite_file <- function(dir, access) {
  return(as.character(fs::path(dir, paste0(access, ".sqlite"))))
}

test_that("use_sqlite_default is exported and returns invisible(NULL)", {
  d <- withr::local_tempdir()
  local_cs9_config()

  expect_identical(cs9::use_sqlite_default, use_sqlite_default)
  out <- withVisible(use_sqlite_default(d))
  expect_null(out$value)
  expect_false(out$visible)
})

test_that("use_sqlite_default sets dbconfigs, path and is_auto", {
  d <- file.path(withr::local_tempdir(), "not_yet_created")
  local_cs9_config()
  expect_false(dir.exists(d))

  use_sqlite_default(d)

  expect_true(dir.exists(d))
  expect_identical(names(config$dbconfigs), c("config", "anon"))
  for (a in c("config", "anon")) {
    expect_identical(config$dbconfigs[[a]]$db, sqlite_file(d, a))
    expect_identical(config$dbconfigs[[a]]$driver, "SQLite")
    expect_identical(config$dbconfigs[[a]]$id, sqlite_file(d, a))
  }
  expect_identical(config$path, d)
  expect_false(config$is_auto)
})

test_that("use_sqlite_default gives the dbconfig shape of get_db_from_env", {
  d <- withr::local_tempdir()
  local_cs9_config()

  use_sqlite_default(d)

  withr::with_envvar(
    c(
      CS9_DBCONFIG_DRIVER = "SQLite",
      CS9_DBCONFIG_DB_CONFIG = sqlite_file(d, "config"),
      CS9_DBCONFIG_DB_ANON = sqlite_file(d, "anon"),
      CS9_DBCONFIG_PORT = NA,
      CS9_DBCONFIG_SERVER = NA,
      CS9_DBCONFIG_USER = NA,
      CS9_DBCONFIG_PASSWORD = NA,
      CS9_DBCONFIG_TRUSTED_CONNECTION = NA,
      CS9_DBCONFIG_SSLMODE = NA,
      CS9_DBCONFIG_ROLE_CREATE_TABLE = NA,
      CS9_DBCONFIG_SCHEMA_CONFIG = NA,
      CS9_DBCONFIG_SCHEMA_ANON = NA
    ),
    {
      expect_identical(config$dbconfigs$config, get_db_from_env("config"))
      expect_identical(config$dbconfigs$anon, get_db_from_env("anon"))
    }
  )
})

test_that("(a) use_sqlite_default changes no environment variable", {
  d <- withr::local_tempdir()
  local_cs9_config()

  before <- Sys.getenv()
  use_sqlite_default(d)
  expect_identical(Sys.getenv(), before)
})

test_that("(b) a child process sees the parent's CS9_* values from before the call", {
  d <- withr::local_tempdir()
  local_cs9_config()

  # the parent has some CS9_* values of its own
  withr::local_envvar(c(
    CS9_DBCONFIG_DRIVER = "PostgreSQL Unicode",
    CS9_PATH = file.path(d, "parent_path")
  ))
  before <- cs9_envvars()
  expect_true(length(before) >= 2L)

  use_sqlite_default(d)

  expect_identical(child_cs9_envvars(), before)
})

test_that("(b) a child process sees no CS9_* value when the parent had none", {
  d <- withr::local_tempdir()
  local_cs9_config()

  present <- names(cs9_envvars())
  if (length(present) > 0L) {
    withr::local_envvar(stats::setNames(rep(NA, length(present)), present))
  }
  expect_length(cs9_envvars(), 0L)

  use_sqlite_default(d)

  expect_length(child_cs9_envvars(), 0L)
})

test_that("(c) switching A, B, A repoints every table and disconnects the old ones", {
  dir_a <- withr::local_tempdir()
  dir_b <- withr::local_tempdir()
  local_cs9_config()
  table_names <- c(
    "config_log",
    "config_tables_last_updated",
    "config_tasks_stats",
    "config_data_hash_for_each_plan"
  )

  old <- list()
  for (d in list(dir_a, dir_b, dir_a)) {
    use_sqlite_default(d)

    expect_identical(sort(names(config$tables)), sort(table_names))
    for (tab in config$tables) {
      expect_identical(tab$dbconfig$db, sqlite_file(d, "config"))
      expect_identical(tab$dbconnection$config$db, sqlite_file(d, "config"))
    }

    # the tables of the previous call were replaced and disconnected
    for (tab in old) {
      expect_false(tab$dbconnection$is_connected())
      expect_false(any(vapply(
        config$tables,
        function(x) identical(x, tab),
        logical(1)
      )))
    }

    # open every connection, so the next switch has something to close
    for (tab in config$tables) {
      tab$connect()
      expect_true(tab$dbconnection$is_connected())
    }
    old <- config$tables
  }
})
