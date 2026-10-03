# Two defects, both found on 2026-10-02 when norsyss.cs9 ran on SQLite.
#
# 1. The key of config_log holds `datetime`, and update_config_log() wrote it in
#    whole seconds. A second run of a task in the same second failed with
#    "UNIQUE constraint failed".
# 2. Task$update_plans() built the plans of a task with a plan analysis once,
#    and then set update_plans_fn to NULL. A second run_task() in one session
#    reused the plans of the first, even when the data had changed.
#
# The fixtures repeat test-plan-end-write.R. testthat sources each test file
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

# plnr::get_anything() finds a function name without `::` through get(), and
# get() reaches the global environment. A test function must therefore live
# there while the test runs.
local_global_fns <- function(fns, envir = parent.frame()) {
  for (nm in names(fns)) {
    assign(nm, fns[[nm]], envir = globalenv())
  }
  withr::defer(rm(list = names(fns), envir = globalenv()), envir = envir)
  return(invisible(NULL))
}

# The argument is not `task`. Inside `[.data.table`, `task` names the column.
read_config_log <- function(task_name) {
  log <- data.table::setDT(dplyr::collect(config$tables$config_log$tbl()))
  return(log[log$task == task_name])
}

test_that("two runs of one task in the same second both write a config_log row", {
  local_sqlite_dbconfig()
  local_global_fns(list(
    cs9test_twice_action = function(data, argset, tables) {
      return(invisible(NULL))
    }
  ))
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  ss$add_task(
    name_grouping = "log",
    name_action = "twice",
    for_each_plan = plnr::expand_list(x = 1L),
    action_fn_name = "cs9test_twice_action"
  )
  # The first insert creates the table, so the two runs below insert only.
  suppressMessages(update_config_log(ss = "cs9test", task = "warmup", "x"))

  # Start at the beginning of a second, so that both runs fall inside it.
  Sys.sleep(1 - as.numeric(Sys.time()) %% 1)
  suppressMessages(ss$run_task("log_twice"))
  suppressMessages(ss$run_task("log_twice"))

  log <- read_config_log("log_twice")
  expect_equal(nrow(log), 2L)
  expect_equal(
    log$message,
    rep("Running task=log_twice with plans=1 and analyses=1", 2)
  )
  # The precondition: both rows really are in one second.
  expect_length(unique(floor(as.numeric(log$datetime))), 1L)
  expect_length(unique(as.numeric(log$datetime)), 2L)
})

test_that("update_config_log() in a tight loop writes one row per call", {
  local_sqlite_dbconfig()

  for (i in 1:5) {
    suppressMessages(update_config_log(ss = "cs9test", task = "loop", i))
  }

  log <- read_config_log("loop")
  expect_equal(sort(log$message), as.character(1:5))
  gaps <- diff(sort(as.numeric(log$datetime)))
  # 4 ms, less a margin for the binary fraction of the parsed text
  expect_true(all(gaps > 0.0039))
})

test_that("config_log_datetime() writes milliseconds and moves forward 4 ms", {
  withr::local_timezone("UTC")
  old <- config_log_state$last_ms
  withr::defer(config_log_state$last_ms <- old)
  config_log_state$last_ms <- -Inf

  now <- .POSIXct(1790000000.1234)
  expect_identical(config_log_datetime(now), "2026-09-21 14:13:20.123")
  expect_identical(config_log_datetime(now), "2026-09-21 14:13:20.127")
  expect_identical(
    config_log_datetime(.POSIXct(1790000000.130)),
    "2026-09-21 14:13:20.131"
  )
  expect_identical(
    config_log_datetime(.POSIXct(1790000001.5)),
    "2026-09-21 14:13:21.500"
  )
})

# The plan analysis reads `state`, which stands for the data in a database.
# The action records the argset of every analysis it runs.
local_replan_task <- function(state, envir = parent.frame()) {
  local_global_fns(
    list(
      cs9test_replan_plan_analysis = function(argset, tables) {
        state$plan_analysis_calls <- state$plan_analysis_calls + 1L
        return(list(
          for_each_plan = plnr::expand_list(i = seq_len(state$n_plans)),
          for_each_analysis = NULL
        ))
      },
      cs9test_replan_action = function(data, argset, tables) {
        state$ran <- c(state$ran, argset$i)
        return(invisible(NULL))
      }
    ),
    envir = envir
  )
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  ss$add_task(
    name_grouping = "replan",
    name_action = "task",
    plan_analysis_fn_name = "cs9test_replan_plan_analysis",
    action_fn_name = "cs9test_replan_action"
  )
  return(ss)
}

new_replan_state <- function(n_plans) {
  state <- new.env()
  state$n_plans <- n_plans
  state$plan_analysis_calls <- 0L
  state$ran <- integer(0)
  return(state)
}

test_that("a second run_task() in one session uses the new plans", {
  local_sqlite_dbconfig()
  # config_log is the first defect, and it is tested above. Without the mock,
  # two runs in one second would fail on it before this test reached the plans.
  local_mocked_bindings(update_config_log = function(...) invisible(NULL))
  state <- new_replan_state(n_plans = 1L)
  ss <- local_replan_task(state)

  suppressMessages(ss$run_task("replan_task"))
  expect_equal(state$ran, 1L)

  state$n_plans <- 3L
  state$ran <- integer(0)
  suppressMessages(ss$run_task("replan_task"))
  expect_equal(state$ran, 1:3)
  expect_equal(ss$tasks$replan_task$num_plans(), 3L)
})

test_that("the first run_task() calls the plan analysis once", {
  local_sqlite_dbconfig()
  local_mocked_bindings(update_config_log = function(...) invisible(NULL))
  state <- new_replan_state(n_plans = 2L)
  ss <- local_replan_task(state)

  suppressMessages(ss$run_task("replan_task"))
  expect_equal(state$plan_analysis_calls, 1L)
  expect_equal(state$ran, 1:2)

  suppressMessages(ss$run_task("replan_task"))
  expect_equal(state$plan_analysis_calls, 2L)
})

test_that("the shortcuts keep the plans of the last build", {
  local_sqlite_dbconfig()
  local_mocked_bindings(update_config_log = function(...) invisible(NULL))
  state <- new_replan_state(n_plans = 2L)
  ss <- local_replan_task(state)

  expect_equal(ss$shortcut_get_num_analyses("replan_task"), 2)
  state$n_plans <- 3L
  expect_equal(ss$shortcut_get_num_analyses("replan_task"), 2)
  expect_equal(ss$shortcut_get_argset("replan_task", index_plan = 2)$i, 2L)
  expect_equal(state$plan_analysis_calls, 1L)

  suppressMessages(ss$run_task("replan_task"))
  expect_equal(state$plan_analysis_calls, 2L)
  state$n_plans <- 1L
  expect_equal(ss$shortcut_get_num_analyses("replan_task"), 3)
  expect_equal(state$plan_analysis_calls, 2L)
})
