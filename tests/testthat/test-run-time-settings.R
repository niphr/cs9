# A task reads `cores` and the date when it runs, not when add_task()
# registers it. Found by norsyss.cs9 Phase 19 (norsyss.cs9#56).
#
# 1. add_task() fixed `cores` at registration. norsyss.cs9 caps its cores with
#    NORSYSS_CORES_MAX, so a value set after the package loaded did nothing.
#    `cores` MAY now be a function with no arguments, which Task$run() calls.
# 2. TaskJob and run_task_sequentially_as_rstudio_job_using_load_all() set
#    `cores` to 1 before run_task(). That override MUST still win over a
#    function.
# 3. A task with fixed plans (`for_each_plan`) set `argset$today` when it was
#    registered, which is at package load. run_task() now builds the plans
#    again, so `argset$today` is the date of the run.
#
# The fixtures repeat test-run-task-twice.R. testthat sources each test file
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

# Replaces `pbmcapply::pbmclapply` until the calling test ends, and returns an
# environment that records whether Task$run() called it and with which
# `mc.cores`. Nothing forks.
#
# Task$run() calls pbmclapply only in its parallel branch: 4 or more plans,
# `cores` above 1 and a session that is not interactive. With `cores` equal to
# 1 it takes the sequential branch and never calls pbmclapply.
local_observe_fork <- function(envir = parent.frame()) {
  observed <- new.env(parent = emptyenv())
  observed$called <- FALSE
  testthat::local_mocked_bindings(
    pbmclapply = function(X, FUN, ...) {
      observed$called <- TRUE
      observed$mc_cores <- list(...)$mc.cores
      return(lapply(seq_along(X), function(i) 1))
    },
    .package = "pbmcapply",
    .env = envir
  )
  return(observed)
}

# Four plans, which is the smallest count that reaches the parallel branch.
add_cores_task <- function(ss, name_action, cores) {
  return(ss$add_task(
    name_grouping = "runtime",
    name_action = name_action,
    cores = cores,
    for_each_plan = plnr::expand_list(i = 1:4),
    action_fn_name = "cs9test_runtime_action"
  ))
}

local_runtime_action <- function(envir = parent.frame()) {
  local_global_fns(
    list(
      cs9test_runtime_action = function(data, argset, tables) {
        return(invisible(NULL))
      }
    ),
    envir = envir
  )
}

# The `cores_n` that update_config_tasks_stats() stored for the task.
stored_cores <- function(task_name) {
  return(get_config_tasks_stats(task = task_name)$cores_n)
}

# Runs a script that a runner wrote, except its devtools::load_all() call. The
# script reaches the surveillance system through `ss_prefix`, which is
# `cs9test_runner$ss` here.
run_runner_script <- function(path, ss) {
  exprs <- parse(path, keep.source = FALSE)
  is_load_all <- vapply(
    exprs,
    function(e) {
      return(is.call(e) && identical(e[[1]], quote(devtools::load_all)))
    },
    logical(1)
  )
  # The script holds one load_all() call, and other expressions to run.
  testthat::expect_equal(sum(is_load_all), 1L)
  testthat::expect_gt(sum(!is_load_all), 1L)
  env <- new.env(parent = globalenv())
  env$cs9test_runner <- new.env(parent = emptyenv())
  env$cs9test_runner$ss <- ss
  utils::capture.output(suppressMessages(
    for (e in exprs[!is_load_all]) {
      eval(e, env)
    }
  ))
  return(invisible(NULL))
}

test_that("a function cores is called when the task runs", {
  testthat::skip_if(interactive(), "the parallel branch needs a batch session")
  local_sqlite_dbconfig()
  local_runtime_action()
  withr::local_envvar(CS9TEST_CORES_MAX = "1")
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  add_cores_task(
    ss,
    name_action = "fn",
    cores = function() {
      return(as.integer(Sys.getenv("CS9TEST_CORES_MAX")))
    }
  )
  # The value changes after registration, as when a pod sets the cap after the
  # package loads.
  withr::local_envvar(CS9TEST_CORES_MAX = "3")
  observed <- local_observe_fork()

  suppressMessages(ss$run_task("runtime_fn"))

  expect_true(observed$called)
  expect_identical(observed$mc_cores, 3L)
  expect_equal(stored_cores("runtime_fn"), 3)
})

test_that("a numeric cores reaches the parallel branch unchanged", {
  testthat::skip_if(interactive(), "the parallel branch needs a batch session")
  local_sqlite_dbconfig()
  local_runtime_action()
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  add_cores_task(ss, name_action = "number", cores = 2)
  observed <- local_observe_fork()

  suppressMessages(ss$run_task("runtime_number"))

  expect_true(observed$called)
  expect_identical(observed$mc_cores, 2)
  expect_equal(stored_cores("runtime_number"), 2)
})

test_that("TaskJob runs a task with a function cores on 1 core", {
  testthat::skip_if(interactive(), "the parallel branch needs a batch session")
  local_sqlite_dbconfig()
  local_runtime_action()
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  add_cores_task(ss, name_action = "taskjob", cores = function() {
    return(3L)
  })
  observed <- local_observe_fork()
  job <- TaskJob$new(
    "runtime_taskjob",
    ss_prefix = "cs9test_runner$ss",
    log_dir = withr::local_tempdir()
  )

  run_runner_script(job$script_path, ss)

  expect_false(observed$called)
  expect_equal(stored_cores("runtime_taskjob"), 1)
})

test_that("the RStudio job runner runs a task with a function cores on 1 core", {
  testthat::skip_if(interactive(), "the parallel branch needs a batch session")
  local_sqlite_dbconfig()
  local_runtime_action()
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  add_cores_task(ss, name_action = "rstudio", cores = function() {
    return(3L)
  })
  observed <- local_observe_fork()
  job <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    jobRunScript = function(path, ...) {
      job$path <- path
      return(invisible(NULL))
    },
    .package = "rstudioapi"
  )
  run_task_sequentially_as_rstudio_job_using_load_all(
    "runtime_rstudio",
    ss_prefix = "cs9test_runner$ss"
  )
  withr::defer(unlink(job$path))

  run_runner_script(job$path, ss)

  expect_false(observed$called)
  expect_equal(stored_cores("runtime_rstudio"), 1)
})

test_that("run_task() sets argset$today of a task with fixed plans to the run date", {
  local_sqlite_dbconfig()
  seen <- new.env(parent = emptyenv())
  seen$rows <- list()
  local_global_fns(list(
    cs9test_today_data = function(argset, tables) {
      return(list(data_today = argset$today))
    },
    cs9test_today_action = function(data, argset, tables) {
      seen$rows[[length(seen$rows) + 1L]] <- data.frame(
        data_today = data$data_today,
        today = argset$today,
        yesterday = argset$yesterday
      )
      return(invisible(NULL))
    }
  ))
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  ss$add_task(
    name_grouping = "runtime",
    name_action = "today",
    for_each_plan = plnr::expand_list(i = 1:2),
    for_each_analysis = plnr::expand_list(j = 1:2),
    data_selector_fn_name = "cs9test_today_data",
    action_fn_name = "cs9test_today_action"
  )
  registered <- lubridate::today()
  # The precondition: registration stamped the real date.
  expect_equal(
    ss$tasks$runtime_today$plans[[1]]$get_argset(1)$today,
    registered
  )

  # Midnight passes between registration and the run.
  run_date <- registered + 1
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(run_date)
    },
    .package = "lubridate"
  )
  suppressMessages(ss$run_task("runtime_today"))

  rows <- do.call(rbind, seen$rows)
  expect_equal(nrow(rows), 4L)
  expect_equal(unique(rows$today), run_date)
  expect_equal(unique(rows$yesterday), run_date - 1)
  expect_equal(unique(rows$data_today), run_date)
})

test_that("run_task() sets argset$today of a task with a plan analysis to the run date", {
  local_sqlite_dbconfig()
  seen <- new.env(parent = emptyenv())
  seen$today <- list()
  local_global_fns(list(
    cs9test_today_plan_analysis = function(argset, tables) {
      return(list(
        for_each_plan = plnr::expand_list(i = 1:2),
        for_each_analysis = NULL
      ))
    },
    cs9test_today_planned_action = function(data, argset, tables) {
      seen$today[[length(seen$today) + 1L]] <- argset$today
      return(invisible(NULL))
    }
  ))
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  ss$add_task(
    name_grouping = "runtime",
    name_action = "planned",
    plan_analysis_fn_name = "cs9test_today_plan_analysis",
    action_fn_name = "cs9test_today_planned_action"
  )
  # get_task() builds the plans before the run, as an interactive user would.
  registered <- lubridate::today()
  expect_equal(ss$shortcut_get_argset("runtime_planned")$today, registered)

  run_date <- registered + 1
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(run_date)
    },
    .package = "lubridate"
  )
  suppressMessages(ss$run_task("runtime_planned"))

  expect_length(seen$today, 2L)
  expect_equal(unique(do.call(c, seen$today)), run_date)
})
