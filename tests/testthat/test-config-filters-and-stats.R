# Two defects in the config tables, found on 2026-10-05.
#
# 1. get_config_log() filtered with `ss == get(ss)`. Inside `[.data.table`
#    both names are the column, so a filter on ss or task failed.
# 2. config_tasks_stats and config_data_hash_for_each_plan hold a datetime in
#    the key, and the code wrote it in whole seconds. A second write in the same
#    second replaced the first row.
#
# The fixture repeats test-run-task-twice.R. testthat sources each test file
# into its own environment, so a helper in one file is not visible in another.

local_sqlite_dbconfig <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  withr::local_envvar(
    c(
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
    ),
    .local_envir = envir
  )

  reload_db_config()
  withr::defer(
    for (tab in config$tables) {
      tab$disconnect()
    },
    envir = envir
  )

  return(config$dbconfigs$anon)
}

# A stored datetime in whole milliseconds since the epoch, in UTC.
as_ms <- function(x) {
  if (!inherits(x, "POSIXct")) {
    x <- as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
  }
  return(round(as.numeric(x) * 1000))
}

test_that("get_config_log() returns exactly the rows that match each filter", {
  local_sqlite_dbconfig()
  for (s in c("a", "b")) {
    for (t in c("x", "y")) {
      suppressMessages(update_config_log(ss = s, task = t, s, t))
    }
  }

  expect_equal(sort(get_config_log(ss = "a")$message), c("ax", "ay"))
  expect_equal(sort(get_config_log(task = "x")$message), c("ax", "bx"))
  expect_equal(get_config_log(ss = "a", task = "x")$message, "ax")
  expect_equal(sort(get_config_log()$message), c("ax", "ay", "bx", "by"))
})

test_that("two stats writes in the same second keep two rows with the true start", {
  withr::local_timezone("UTC")
  local_sqlite_dbconfig()
  starts <- .POSIXct(1790000000 + c(0.123, 0.323), tz = "UTC")
  for (i in 1:2) {
    update_config_tasks_stats(
      ss = "cs9test",
      task = "stats_twice",
      cores_n = 1L,
      plans_n = 1L,
      analyses_n = 1L,
      start_datetime = starts[i],
      stop_datetime = starts[i] + 0.5,
      ram_all_cores_mb = 0,
      ram_per_core_mb = 0,
      status = "succeeded"
    )
  }

  stats <- get_config_tasks_stats(task = "stats_twice")
  expect_equal(nrow(stats), 2L)
  expect_equal(
    sort(as_ms(stats$start_datetime)),
    c(1790000000123, 1790000000323)
  )
})

test_that("two hash writes in the same second keep two rows", {
  withr::local_timezone("UTC")
  local_sqlite_dbconfig()
  clock <- new.env()
  clock$times <- .POSIXct(1790000000 + c(0.1, 0.3), tz = "UTC")
  local_mocked_bindings(config_now = function() {
    now <- clock$times[1]
    clock$times <- clock$times[-1]
    return(now)
  })
  for (h in c("h1", "h2")) {
    update_config_data_hash_for_each_plan(
      task = "hash_twice",
      index_plan = 1L,
      element_tag = "e",
      element_hash = h,
      all_hash = h
    )
  }

  hash <- get_config_data_hash_for_each_plan(task = "hash_twice")
  expect_equal(nrow(hash), 2L)
  expect_equal(sort(hash$element_hash), c("h1", "h2"))
  expect_equal(
    sort(as_ms(hash$datetime)),
    c(1790000000100, 1790000000300)
  )
})
