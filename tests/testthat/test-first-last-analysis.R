# `Task$run()` MUST run the argset with `last_analysis = TRUE` after every other
# argset, and MUST NOT run it when a parallel plan fails.
#
# A task with 4 or more plans, `cores > 1` and `interactive()` FALSE takes the
# parallel branch of `Task$run()`. That branch runs plan 1 alone, plans 2 to
# n-1 through `pbmcapply::pbmclapply`, and plan n alone. The last argset is in
# plan n. An action uses it to combine the results of all the other argsets.
#
# The mock below replaces `pbmcapply::pbmclapply` with a serial `lapply()`. It
# calls the worker in this process, so nothing forks and the order of the
# records is the order of the runs. Each test checks that the mock received
# plans 2, 3 and 4. A run that takes the sequential branch therefore fails.
#
# testthat sources each test file into its own environment. The two fixtures
# below therefore repeat `tests/testthat/test-fork-ordering.R`.

# A SQLite dbconfig for the "anon" access, plus the four configuration tables.
#
# `.local_envir = envir` ties the deferred cleanup to the caller. The default is
# this function's own frame, which would delete the temporary directory as soon
# as the function returns.
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

new_task_table <- function(dbconfig) {
  DBTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name = "anon_fork",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
}

# Five plans with two analyses each. Each analysis appends one row to
# `record$runs`.
#
# Each plan reads different data, so each plan has a different data hash. With
# identical data every plan has the same hash, and a hash stored under the wrong
# index_plan cannot be detected.
#
# The analyses of plan `fail_plan` stop with an error.
new_plans <- function(record, fail_plan = NULL) {
  lapply(1:5, function(i) {
    p <- plnr::Plan$new()
    p$add_data(
      name = "d",
      fn = function() data.table::data.table(x = letters[i], n = i)
    )
    for (j in 1:2) {
      p$add_analysis(
        fn = function(data, argset, tables) {
          if (identical(argset$index_plan, fail_plan)) {
            stop("plan ", fail_plan, " fails")
          }
          record$runs[[length(record$runs) + 1]] <- data.table::data.table(
            index_plan = argset$index_plan,
            index_analysis = argset$index_analysis,
            first_analysis = argset$first_analysis,
            last_analysis = argset$last_analysis
          )
          invisible(NULL)
        },
        index = i
      )
    }
    p
  })
}

new_record <- function() {
  record <- new.env(parent = emptyenv())
  record$runs <- list()
  record$parallel_plans <- integer(0)
  record
}

new_task <- function(tables, record, name_action, fail_plan = NULL) {
  Task$new(
    name_grouping = "firstlast",
    name_action = name_action,
    plans = new_plans(record, fail_plan),
    tables = tables,
    cores = 2,
    upsert_at_end_of_each_plan = FALSE,
    insert_at_end_of_each_plan = FALSE
  )
}

# Replaces `pbmcapply::pbmclapply` with a serial `lapply()` that calls `FUN`.
# It records the index_plan of each plan it receives.
local_serial_pbmclapply <- function(record, envir = parent.frame()) {
  testthat::local_mocked_bindings(
    pbmclapply = function(
      X,
      FUN,
      ...,
      ignore.interactive,
      mc.cores,
      mc.style,
      mc.substyle
    ) {
      record$parallel_plans <- vapply(
        X,
        function(x) x$get_argset(1)$index_plan,
        integer(1)
      )
      lapply(X, FUN, ...)
    },
    .package = "pbmcapply",
    .env = envir
  )
}

test_that("the argset with last_analysis = TRUE runs after every other argset", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_task_table(dbconfig)
  withr::defer(tab$disconnect())

  record <- new_record()
  task <- new_task(list("anon_fork" = tab), record, name_action = "lastruns")
  local_serial_pbmclapply(record)

  suppressMessages(task$run(cores = 2))
  runs <- data.table::rbindlist(record$runs)

  expect_identical(record$parallel_plans, 2:4)

  # Each of the 10 argsets ran once.
  expect_identical(nrow(runs), 10L)
  expect_setequal(
    paste(runs$index_plan, runs$index_analysis),
    paste(rep(1:5, each = 2), rep(1:2, times = 5))
  )

  expect_identical(which(runs$first_analysis), 1L)
  expect_identical(which(runs$last_analysis), 10L)
  expect_identical(runs$index_plan[10], 5L)
  expect_identical(runs$index_analysis[10], 2L)
})

test_that("a failed parallel plan stops run() before the last argset runs", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_task_table(dbconfig)
  withr::defer(tab$disconnect())

  record <- new_record()
  task <- new_task(
    list("anon_fork" = tab),
    record,
    name_action = "failstops",
    fail_plan = 3L
  )
  local_serial_pbmclapply(record)

  # The worker tries a failed plan 5 times, with `Sys.sleep(5)` between tries.
  testthat::local_mocked_bindings(
    Sys.sleep = function(...) NULL,
    .package = "base"
  )

  expect_error(suppressMessages(task$run(cores = 2)), "plan 3 fails")
  runs <- data.table::rbindlist(record$runs)

  expect_identical(record$parallel_plans, 2:4)

  # Plans 1 and 2 ran, so the assertions below do not pass on an empty record.
  expect_identical(runs$index_plan, c(1L, 1L, 2L, 2L))
  expect_false(any(runs$last_analysis))
})

test_that("run() stores the data hash of plan n under index_plan n", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_task_table(dbconfig)
  withr::defer(tab$disconnect())

  record <- new_record()
  task <- new_task(list("anon_fork" = tab), record, name_action = "hashindex")
  expected <- vapply(
    task$plans,
    function(p) p$get_data()$hash$current,
    character(1)
  )
  expect_length(unique(expected), 5L)

  local_serial_pbmclapply(record)
  suppressMessages(task$run(cores = 2))

  expect_identical(record$parallel_plans, 2:4)

  hashes <- get_config_data_hash_for_each_plan(task = task$name)
  index_plan <- as.integer(hashes$index_plan)

  expect_identical(sort(index_plan), 1:5)
  expect_length(unique(hashes$all_hash), 5L)
  expect_identical(hashes$all_hash[index_plan == 1L], expected[[1]])
  expect_identical(hashes$all_hash[order(index_plan)], expected)
})

# `DBTableExtended_v9` overrides the four load methods of `csdb::DBTable_v9`.
# Each override MUST pass `load_timeout` to csdb. csdb hands it to its internal
# load functions `load_data_infile()` and `upsert_load_data_infile()`.
#
# The block below replaces both functions in the csdb namespace with wrappers.
# Each wrapper records the table and the `load_timeout` that it receives, and
# then calls the original function, so the rows still reach SQLite. The
# configuration tables load through the same functions with the default of
# 3600 s. The block therefore reads only the calls for its own table.
#
# `drop_all_rows_and_then_upsert_data()` reaches the load through the internal
# call `self$upsert_data(..., load_timeout = load_timeout)` in csdb. That call
# dispatches to the cs9 override of `upsert_data()`.
test_that("the cs9 load methods pass load_timeout to the csdb load functions", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_task_table(dbconfig)
  withr::defer(tab$disconnect())

  calls <- new.env(parent = emptyenv())
  reset_calls <- function() {
    calls$table <- list()
    calls$load_timeout <- numeric(0)
  }
  reset_calls()

  record_load <- function(original) {
    force(original)
    function(...) {
      args <- list(...)
      calls$table <- c(calls$table, list(args$table))
      calls$load_timeout <- c(calls$load_timeout, args$load_timeout)
      original(...)
    }
  }
  original_load <- utils::getFromNamespace("load_data_infile", "csdb")
  original_upsert_load <- utils::getFromNamespace(
    "upsert_load_data_infile",
    "csdb"
  )
  testthat::local_mocked_bindings(
    load_data_infile = record_load(original_load),
    upsert_load_data_infile = record_load(original_upsert_load),
    .package = "csdb"
  )

  own_table <- tab$table_name_short_for_mssql_fully_specified_for_postgres
  own_timeouts <- function() {
    calls$load_timeout[vapply(calls$table, identical, logical(1), own_table)]
  }
  rows <- function() {
    d <- dplyr::collect(tab$tbl())
    d[order(d$x), c("x", "n")]
  }

  reset_calls()
  suppressMessages(tab$insert_data(
    data.table::data.table(x = "a", n = 1L),
    load_timeout = 7
  ))
  expect_identical(own_timeouts(), 7)
  expect_identical(rows()$x, "a")
  expect_equal(rows()$n, 1)

  reset_calls()
  suppressMessages(tab$upsert_data(
    data.table::data.table(x = c("a", "b"), n = c(10L, 2L)),
    load_timeout = 7
  ))
  expect_identical(own_timeouts(), 7)
  expect_identical(rows()$x, c("a", "b"))
  expect_equal(rows()$n, c(10, 2))

  reset_calls()
  suppressMessages(tab$drop_all_rows_and_then_upsert_data(
    data.table::data.table(x = "c", n = 3L),
    load_timeout = 7
  ))
  expect_identical(own_timeouts(), 7)
  expect_identical(rows()$x, "c")
  expect_equal(rows()$n, 3)

  reset_calls()
  suppressMessages(tab$drop_all_rows_and_then_insert_data(
    data.table::data.table(x = "d", n = 4L),
    load_timeout = 7
  ))
  expect_identical(own_timeouts(), 7)
  expect_identical(rows()$x, "d")
  expect_equal(rows()$n, 4)
})
