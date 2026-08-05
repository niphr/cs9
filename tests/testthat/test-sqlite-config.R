test_that("a SQLite environment validates as ok", {
  d <- withr::local_tempdir()
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "config/anon",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = file.path(d, "config.sqlite"),
    CS9_DBCONFIG_DB_ANON = file.path(d, "anon.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA,
    CS9_DBCONFIG_USER = NA,
    CS9_DBCONFIG_PASSWORD = NA,
    CS9_DBCONFIG_SCHEMA_CONFIG = NA,
    CS9_DBCONFIG_SCHEMA_ANON = NA
  ))

  reload_db_config()

  result <- check_environment_setup(verbose = FALSE)
  expect_equal(result$status, "ok")
  expect_equal(result$issues, character(0))
})

test_that("the dbconfig id is the file path under SQLite", {
  d <- withr::local_tempdir()
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "config/anon",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = file.path(d, "config.sqlite"),
    CS9_DBCONFIG_DB_ANON = file.path(d, "anon.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA,
    CS9_DBCONFIG_SCHEMA_CONFIG = "some_schema",
    CS9_DBCONFIG_SCHEMA_ANON = "some_schema"
  ))

  reload_db_config()

  # the file path itself, not [db].[schema]. CS9_DBCONFIG_SCHEMA_* is set above
  # precisely so that the [db].[schema] form would be visibly different.
  expect_equal(config$dbconfigs$config$id, file.path(d, "config.sqlite"))
  expect_equal(config$dbconfigs$anon$id, file.path(d, "anon.sqlite"))
  expect_false(grepl("[", config$dbconfigs$config$id, fixed = TRUE))
})

test_that("reload_db_config is exported and returns invisible(NULL)", {
  # `::` respects the namespace pkgload::load_all() simulates, so this really
  # does pin the export: cs9::validate_environment errors with
  # "not an exported object from 'namespace:cs9'".
  expect_true(is.function(cs9::reload_db_config))
  expect_identical(cs9::reload_db_config, reload_db_config)

  d <- withr::local_tempdir()
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "config",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = file.path(d, "config.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA
  ))

  expect_null(withVisible(reload_db_config())$value)
  expect_false(withVisible(reload_db_config())$visible)
})

test_that("reload_db_config leaves no stale tables when the environment is invalid", {
  d <- withr::local_tempdir()

  # a valid environment first, so there is something stale to leave behind
  withr::with_envvar(
    c(
      CS9_AUTO = "0",
      CS9_PATH = d,
      CS9_DBCONFIG_ACCESS = "config",
      CS9_DBCONFIG_DRIVER = "SQLite",
      CS9_DBCONFIG_DB_CONFIG = file.path(d, "good.sqlite"),
      CS9_DBCONFIG_PORT = NA,
      CS9_DBCONFIG_SERVER = NA
    ),
    reload_db_config()
  )
  expect_length(config$tables, 4)
  expect_equal(config$dbconfigs$config$db, file.path(d, "good.sqlite"))

  # now an access list without "config": setup_database_tables() reads
  # config$dbconfigs$config unconditionally, so construction fails
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "anon",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = NA,
    CS9_DBCONFIG_DB_ANON = file.path(d, "anon.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA
  ))
  expect_error(reload_db_config())

  # the tables from the previous configuration must be gone, not stale
  expect_length(config$tables, 0)
  expect_null(config$tables$config_log)
})

test_that("reload_db_config disconnects the tables it replaces", {
  # csdb gained its SQLite backend in 2026.8.5. Against 2026.5.13 the driver
  # string "SQLite" matches no branch, falls into csdb's generic ODBC arm, and
  # $connect() fails with "Can't open lib 'SQLite' : file not found", which
  # says nothing about the cause.
  #
  # The guard is not redundant with the csdb (>= 2026.8.5) in DESCRIPTION.
  # That floor is honoured by the resolver that installs the dependency, and
  # by R CMD check, but nothing enforces it afterwards: cs9 reaches csdb
  # through `csdb::` alone, so NAMESPACE holds no import directive, R runs no
  # version check at load time, and `library(cs9)` on top of csdb 2026.5.13
  # loads without complaint. Measured on 2026-08-05: R CMD INSTALL exits 0 and
  # library(cs9) succeeds. So this is the only thing standing between an
  # out-of-date csdb and two opaque ODBC errors.
  skip_if_not_installed("csdb", "2026.8.5")

  d <- withr::local_tempdir()
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "config",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = file.path(d, "config.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA
  ))

  reload_db_config()
  old <- config$tables$config_log
  old$connect()
  expect_true(old$dbconnection$is_connected())

  reload_db_config()

  # the discarded object was disconnected, and really was replaced
  expect_false(old$dbconnection$is_connected())
  expect_false(identical(old, config$tables$config_log))

  withr::defer(config$tables$config_log$disconnect())
})

test_that("a SQLite environment does not require SERVER, PORT, USER or PASSWORD", {
  d <- withr::local_tempdir()
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "config/anon",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = file.path(d, "config.sqlite"),
    CS9_DBCONFIG_DB_ANON = file.path(d, "anon.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA,
    CS9_DBCONFIG_USER = NA,
    CS9_DBCONFIG_PASSWORD = NA,
    CS9_DBCONFIG_SCHEMA_CONFIG = NA,
    CS9_DBCONFIG_SCHEMA_ANON = NA
  ))

  # the four variables really are absent
  expect_equal(Sys.getenv("CS9_DBCONFIG_SERVER"), "")
  expect_equal(Sys.getenv("CS9_DBCONFIG_PORT"), "")
  expect_equal(Sys.getenv("CS9_DBCONFIG_USER"), "")
  expect_equal(Sys.getenv("CS9_DBCONFIG_PASSWORD"), "")

  result <- validate_environment()
  expect_equal(result$status, "ok")
  expect_equal(result$issues, character(0))
})

test_that("an access list without config is rejected", {
  d <- withr::local_tempdir()
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "anon",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = NA,
    CS9_DBCONFIG_DB_ANON = file.path(d, "anon.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA
  ))

  result <- validate_environment()
  expect_equal(result$status, "error")
  expect_true(any(grepl("must include 'config'", result$issues, fixed = TRUE)))
})

test_that("a missing CS9_DBCONFIG_DB_<ACCESS> is rejected", {
  d <- withr::local_tempdir()
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "config/anon",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = file.path(d, "config.sqlite"),
    CS9_DBCONFIG_DB_ANON = NA,
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA
  ))

  result <- validate_environment()
  expect_equal(result$status, "error")
  expect_true(any(grepl("CS9_DBCONFIG_DB_ANON", result$issues, fixed = TRUE)))
})

test_that("a SQLite partitioned table uses the xxpxx separator", {
  dbconfig <- function(driver) {
    list(
      driver = driver,
      server = "",
      port = NA_integer_,
      user = "",
      password = "",
      trusted_connection = "",
      sslmode = "",
      role_create_table = "",
      schema = "",
      db = tempfile(fileext = ".sqlite")
    )
  }
  partition_names <- function(driver) {
    pt <- DBPartitionedTableExtended_v9$new(
      dbconfig = dbconfig(driver),
      table_name_base = "anon_demo",
      table_name_partitions = c("2023", "2024"),
      column_name_partition = "part",
      field_types = c("x" = "TEXT"),
      keys = c("x", "part"),
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
    )
    unname(vapply(pt$tables, function(z) z$table_name, character(1)))
  }

  # PARTITION is a keyword in SQLite's window-function grammar
  expected_xxpxx <- c("anon_demo_xxpxx_2023", "anon_demo_xxpxx_2024")
  expect_equal(partition_names("SQLite"), expected_xxpxx)
  expect_equal(partition_names("sqlite"), expected_xxpxx)
  expect_equal(partition_names("SQLITE"), expected_xxpxx)

  # the other two backends are unchanged
  expect_equal(partition_names("PostgreSQL Unicode"), expected_xxpxx)
  expect_equal(
    partition_names("ODBC Driver 17 for SQL Server"),
    c("anon_demo_PARTITION_2023", "anon_demo_PARTITION_2024")
  )
})

test_that("connect() creates the config table in the SQLite file", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # See the note on the same guard above: csdb's SQLite backend is 2026.8.5.
  skip_if_not_installed("csdb", "2026.8.5")

  d <- withr::local_tempdir()
  db_file <- file.path(d, "config.sqlite")
  withr::local_envvar(c(
    CS9_AUTO = "0",
    CS9_PATH = d,
    CS9_DBCONFIG_ACCESS = "config",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = db_file,
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA,
    CS9_DBCONFIG_USER = NA,
    CS9_DBCONFIG_PASSWORD = NA,
    CS9_DBCONFIG_SCHEMA_CONFIG = NA
  ))

  reload_db_config()

  # setup_database_tables() only constructs the R6 objects. Creation is lazy,
  # deferred to private$lazy_creation_of_table() by connect().
  expect_false(file.exists(db_file))

  config$tables$config_log$connect()
  withr::defer(config$tables$config_log$disconnect())

  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  withr::defer(DBI::dbDisconnect(con))

  expect_true("config_log" %in% DBI::dbListTables(con))
})
