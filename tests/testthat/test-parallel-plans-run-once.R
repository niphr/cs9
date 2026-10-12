# `Task$run()` MUST run each parallel plan once.
#
# Before 26.10.12 `run_parallel_plans()` ran a failing plan up to 5 times, with
# 5 seconds of sleep between tries. It stopped with the error of the last try.
# In norsyss.cs9 the retries failed on a duplicate key, so the first error never
# reached the log. The scheduler retries the whole task, so cs9 does not retry.
#
# These tests drive the real `Task$run()` with 2 cores. `pbmcapply::pbmclapply`
# forks, so the tests skip on Windows. A forked child does not share memory with
# the parent, so each analysis appends one line per call to a file for its plan.
# The line holds `cs9::config$plan_attempt_index`.
#
# The fixtures repeat `tests/testthat/test-fork-ordering.R`. testthat sources
# each test file into its own environment, so a helper in one file is not
# visible in another.

# A SQLite dbconfig for the "anon" access, plus the four configuration tables.
#
# `.local_envir = envir` ties the deferred cleanup to the caller. The default is
# this function's own frame, which deletes the directory when the function
# returns.
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

  config$dbconfigs$anon
}

# Six plans. `Task$run()` runs plan 1 and plan 6 sequentially, and plans 2 to 5
# in parallel. Each plan in `failing` fails on every call.
new_failing_task <- function(dbconfig, record_dir, failing) {
  tab <- DBTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name = "anon_parallel_once",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  plans <- lapply(1:6, function(i) {
    p <- plnr::Plan$new()
    p$add_data(
      name = "d",
      fn = function() data.table::data.table(x = "a", n = 1L)
    )
    p$add_analysis(
      fn = function(data, argset, tables) {
        cat(
          config$plan_attempt_index,
          "\n",
          sep = "",
          file = file.path(record_dir, paste0("plan-", argset$index_plan)),
          append = TRUE
        )
        if (argset$index_plan %in% failing) {
          stop("plan ", argset$index_plan, " failed on purpose")
        }
        invisible(NULL)
      },
      index = i
    )
    p
  })
  Task$new(
    name_grouping = "parallel",
    name_action = "once",
    plans = plans,
    tables = list("anon_parallel_once" = tab),
    cores = 2,
    upsert_at_end_of_each_plan = FALSE,
    insert_at_end_of_each_plan = FALSE
  )
}

# Runs the failing task and returns the error, the number of calls per plan and
# every recorded `plan_attempt_index`.
run_failing_task <- function(failing = 3L) {
  dbconfig <- local_sqlite_dbconfig()
  record_dir <- withr::local_tempdir()
  task <- new_failing_task(dbconfig, record_dir, failing)
  withr::defer(
    for (s in task$tables) {
      s$disconnect()
    }
  )

  err <- tryCatch(
    suppressWarnings(suppressMessages(task$run(cores = 2))),
    error = function(e) e
  )
  lines <- lapply(1:6, function(i) {
    f <- file.path(record_dir, paste0("plan-", i))
    if (file.exists(f)) readLines(f) else character(0)
  })
  list(
    error = err,
    calls = lengths(lines),
    attempts = unlist(lines)
  )
}

test_that("a parallel plan that fails runs once", {
  skip_on_os("windows")
  out <- run_failing_task()

  expect_identical(out$calls[3], 1L)
})

test_that("the other plans run once when one parallel plan fails", {
  skip_on_os("windows")
  out <- run_failing_task()

  # Plan 1 runs before the fork, and plans 2 to 5 run in parallel. Plan 6 never
  # runs, because `Task$run()` stops after the parallel plans. With prescheduled
  # cores, the error in plan 3 also skipped plan 5, which shared its core.
  expect_identical(out$calls, c(1L, 1L, 1L, 1L, 1L, 0L))
})

test_that("Task$run() stops with the error text of the failing parallel plan", {
  skip_on_os("windows")
  out <- run_failing_task()

  expect_s3_class(out$error, "error")
  expect_match(conditionMessage(out$error), "Error in index ", fixed = TRUE)
  expect_match(
    conditionMessage(out$error),
    "plan 3 failed on purpose",
    fixed = TRUE
  )
})

test_that("a parallel plan runs with plan_attempt_index 1", {
  skip_on_os("windows")
  out <- run_failing_task()

  # Five calls with a recorded index, one each for plans 1 to 5.
  expect_length(out$attempts, 5L)
  expect_true(all(out$attempts == "1"))
})

test_that("two failing parallel plans run once, and the error names both", {
  skip_on_os("windows")
  out <- run_failing_task(failing = c(2L, 3L))

  expect_identical(out$calls, c(1L, 1L, 1L, 1L, 1L, 0L))

  # The error starts with a count line and holds every failed plan, in plan order.
  msg <- conditionMessage(out$error)
  expect_match(msg, "2 of 4 parallel plans failed.", fixed = TRUE)
  expect_match(msg, "plan 2 failed on purpose", fixed = TRUE)
  expect_match(msg, "plan 3 failed on purpose", fixed = TRUE)
  expect_lt(
    regexpr("plan 2 failed on purpose", msg, fixed = TRUE),
    regexpr("plan 3 failed on purpose", msg, fixed = TRUE)
  )
})
