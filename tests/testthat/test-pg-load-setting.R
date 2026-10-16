# CS9_DBCONFIG_PG_LOAD reaches pg_load in every dbconfig cs9 builds, and from
# there the csdb connection of every table cs9 builds. Nothing here connects:
# csdb opens a connection at its first use.
pg_load_sqlite_env <- function(dir, pg_load) {
  return(c(
    CS9_AUTO = "0",
    CS9_PATH = dir,
    CS9_DBCONFIG_ACCESS = "config/anon",
    CS9_DBCONFIG_DRIVER = "SQLite",
    CS9_DBCONFIG_DB_CONFIG = file.path(dir, "config.sqlite"),
    CS9_DBCONFIG_DB_ANON = file.path(dir, "anon.sqlite"),
    CS9_DBCONFIG_PORT = NA,
    CS9_DBCONFIG_SERVER = NA,
    CS9_DBCONFIG_USER = NA,
    CS9_DBCONFIG_PASSWORD = NA,
    CS9_DBCONFIG_SCHEMA_CONFIG = NA,
    CS9_DBCONFIG_SCHEMA_ANON = NA,
    CS9_DBCONFIG_PG_LOAD = pg_load
  ))
}

pg_load_postgres_env <- function(dir, pg_load) {
  return(c(
    CS9_AUTO = "0",
    CS9_PATH = dir,
    CS9_DBCONFIG_ACCESS = "config/anon",
    CS9_DBCONFIG_DRIVER = "PostgreSQL Unicode",
    CS9_DBCONFIG_SERVER = "localhost",
    CS9_DBCONFIG_PORT = "5432",
    CS9_DBCONFIG_USER = "cs9_user",
    CS9_DBCONFIG_PASSWORD = NA,
    CS9_DBCONFIG_SCHEMA_CONFIG = "config",
    CS9_DBCONFIG_DB_CONFIG = "cs9_surveillance",
    CS9_DBCONFIG_SCHEMA_ANON = "anon",
    CS9_DBCONFIG_DB_ANON = "cs9_surveillance",
    CS9_DBCONFIG_PG_LOAD = pg_load
  ))
}

test_that("an unset or empty CS9_DBCONFIG_PG_LOAD builds pg_load psql", {
  d <- withr::local_tempdir()
  for (value in list(NA_character_, "")) {
    withr::with_envvar(pg_load_postgres_env(d, value), {
      for (access in c("config", "anon")) {
        expect_identical(get_db_from_env(access)$pg_load, "psql")
      }
    })
  }
  withr::local_envvar(pg_load_sqlite_env(d, NA_character_))
  reload_db_config()
  expect_identical(config$dbconfigs$config$pg_load, "psql")
  expect_identical(config$dbconfigs$anon$pg_load, "psql")
})

test_that("CS9_DBCONFIG_PG_LOAD=json builds pg_load json for every access", {
  d <- withr::local_tempdir()
  withr::with_envvar(pg_load_postgres_env(d, "json"), {
    set_env_vars()
    expect_identical(names(config$dbconfigs), c("config", "anon"))
    for (access in names(config$dbconfigs)) {
      expect_identical(config$dbconfigs[[access]]$pg_load, "json")
    }
    result <- check_environment_setup(verbose = FALSE)
    expect_identical(result$pg_load, "json")
    out <- capture.output(check_environment_setup(verbose = TRUE))
    expect_true(any(grepl("CS9_DBCONFIG_PG_LOAD: json", out, fixed = TRUE)))
  })
  # SQLite ignores the setting, and cs9 still carries it
  withr::local_envvar(pg_load_sqlite_env(d, "json"))
  reload_db_config()
  for (access in c("config", "anon")) {
    expect_identical(config$dbconfigs[[access]]$pg_load, "json")
  }
  expect_equal(check_environment_setup(verbose = FALSE)$status, "ok")
})

test_that("an invalid CS9_DBCONFIG_PG_LOAD stops the load with the variable and the allowed values", {
  d <- withr::local_tempdir()
  msg <- 'CS9_DBCONFIG_PG_LOAD MUST be "psql" or "json", or unset. It is "copy".'
  for (env in list(
    pg_load_postgres_env(d, "copy"),
    pg_load_sqlite_env(d, "copy")
  )) {
    withr::with_envvar(env, {
      expect_error(reload_db_config(), msg, fixed = TRUE)
      expect_length(config$dbconfigs, 0)
      expect_length(config$tables, 0)
      # .onLoad() swallows a missing variable, and passes this error on
      expect_error(.onLoad(libname = "", pkgname = "cs9"), msg, fixed = TRUE)
      result <- validate_environment()
      expect_equal(result$status, "error")
      expect_true(any(grepl(msg, result$issues, fixed = TRUE)))
    })
  }
  withr::local_envvar(pg_load_sqlite_env(d, NA_character_))
  reload_db_config()
})

test_that("every table cs9 builds carries pg_load to its csdb connection", {
  d <- withr::local_tempdir()
  withr::local_envvar(pg_load_sqlite_env(d, "json"))
  reload_db_config()

  for (tab in config$tables) {
    expect_identical(tab$dbconnection$config$pg_load, "json")
  }
  ss <- SurveillanceSystem_v9$new(name = "pgload")
  ss$add_table(
    name_access = "anon",
    name_grouping = "pgload",
    name_variant = "plain",
    field_types = c(a = "TEXT", n = "INTEGER"),
    keys = "a"
  )
  ss$add_partitionedtable(
    name_access = "anon",
    name_grouping = "pgload",
    name_variant = "part",
    name_partitions = c("p1", "p2"),
    column_name_partition = "part",
    field_types = c(part = "TEXT", a = "TEXT"),
    keys = c("part", "a")
  )
  plain <- ss$tables$anon_pgload_plain
  part <- ss$partitionedtables$anon_pgload_part
  expect_identical(plain$dbconnection$config$pg_load, "json")
  expect_identical(part$dbconnection$config$pg_load, "json")
  for (child in part$tables) {
    expect_identical(child$dbconnection$config$pg_load, "json")
  }
  staging <- run_new_staging_table(
    dbconfig = plain$dbconfig,
    live_name = plain$table_name,
    field_types = plain$field_types,
    keys = plain$keys,
    dbconnection = plain$dbconnection
  )
  expect_identical(staging$dbconfig$pg_load, "json")
  expect_identical(staging$dbconnection$config$pg_load, "json")
  conn <- run_new_connection(config$dbconfigs$anon)
  expect_identical(conn$config$pg_load, "json")
})

test_that("an invalid CS9_DBCONFIG_PG_LOAD stops the load when CS9_DBCONFIG_ACCESS is unset", {
  d <- withr::local_tempdir()
  msg <- 'CS9_DBCONFIG_PG_LOAD MUST be "psql" or "json", or unset. It is "copy".'
  for (access in list(NA_character_, "")) {
    env <- pg_load_sqlite_env(d, "copy")
    env[["CS9_DBCONFIG_ACCESS"]] <- access
    withr::with_envvar(env, {
      expect_error(.onLoad(libname = "", pkgname = "cs9"), msg, fixed = TRUE)
      expect_error(set_env_vars(), msg, fixed = TRUE)
    })
  }
  withr::local_envvar(pg_load_sqlite_env(d, NA_character_))
  reload_db_config()
})

test_that("cs9 loads with CS9_DBCONFIG_ACCESS and CS9_DBCONFIG_PG_LOAD unset", {
  d <- withr::local_tempdir()
  env <- pg_load_sqlite_env(d, NA_character_)
  env[["CS9_DBCONFIG_ACCESS"]] <- NA_character_
  withr::with_envvar(env, {
    expect_no_error(.onLoad(libname = "", pkgname = "cs9"))
    expect_length(config$dbconfigs, 0)
  })
  # the SQLite default a dependent package then sets is unchanged
  withr::local_envvar(pg_load_sqlite_env(d, NA_character_))
  reload_db_config()
  expect_identical(config$dbconfigs$config$driver, "SQLite")
  expect_identical(config$dbconfigs$config$pg_load, "psql")
  expect_length(config$tables, 4)
})
