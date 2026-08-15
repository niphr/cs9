# `Task$run()` MUST close every database connection before it forks.
#
# `Task$run()` takes its parallel branch, runs plan 1 through `run_sequential()`,
# and then calls `private$run_parallel_plans()`. `run_parallel_plans()` forks with
# `pbmcapply::pbmclapply` and passes the table objects into the workers.
# `run_sequential()` ends with a disconnect sweep, so every worker opens its own
# backend rather than inheriting one.
#
# A forked child that uses an inherited PostgreSQL connection reads wrong
# answers. It does not error. Measured against the NorSySS server on 2026-08-14.
# A child asked for `select 4` and read 3. The parent asked for `select 999` and
# read 2. `DBI::dbIsValid()` returns TRUE on such a handle, so the reconnect path
# in `autoconnection` never fires.
#
# The ordering is the invariant, so this file drives the public `Task$run()` and
# mocks `pbmcapply::pbmclapply`. The mock reads every connection at the fork
# boundary, records what it read, and returns without forking. A test that called
# the private `run_sequential()` would prove that the helper disconnects. It would
# not prove that the public path calls the helper before it forks.
#
# The fixtures below repeat `tests/testthat/test-shared-connection.R`. testthat
# sources each test file into its own environment, so a helper in one file is not
# visible in another.
#
# There is deliberately no skip on a missing package. csdb imports RSQLite, and
# cs9 imports csdb, so RSQLite is present whenever this suite runs. `R/r6_Task.R`
# calls `pbmcapply::pbmclapply` directly. cs9 cannot fork without pbmcapply, so a
# missing pbmcapply is a real failure and not a reason to skip.

# A SQLite dbconfig for the "anon" access, plus the four configuration tables.
#
# `.local_envir = envir` is mandatory, not decoration. withr ties the deferred
# cleanup to that environment. The default is this function's own frame. The
# temporary directory would then be deleted as soon as the function returns.
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

  # DBTableExtended_v9$insert_data() writes to
  # config$tables$config_tables_last_updated. The configuration tables must
  # therefore exist before the first insert.
  reload_db_config()
  withr::defer(for (tab in config$tables) tab$disconnect(), envir = envir)

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

# Four plans, which is the smallest count that reaches the parallel branch.
#
# `Task$run()` forces the sequential branch at one plan, at two plans and at
# three plans. It runs plan 1 and plan 4 sequentially, and plans 2 and 3 in
# parallel. See the branch table in `R/r6_Task.R`.
#
# Each analysis appends its plan index to `record`. The mock then reads which
# plans finished before the fork.
new_plans <- function(record) {
  lapply(1:4, function(i) {
    p <- plnr::Plan$new()
    p$add_data(
      name = "d",
      fn = function() data.table::data.table(x = "a", n = 1L)
    )
    p$add_analysis(
      fn = function(data, argset, tables) {
        record$plans_run <- c(record$plans_run, argset$index_plan)
        invisible(NULL)
      },
      index = i
    )
    p
  })
}

new_task <- function(tables, record) {
  Task$new(
    name_grouping = "fork",
    name_action = "ordering",
    plans = new_plans(record),
    tables = tables,
    cores = 2,
    upsert_at_end_of_each_plan = FALSE,
    insert_at_end_of_each_plan = FALSE
  )
}

# TRUE for each table whose connection is open.
#
# `is_connected()` reads `private$pconnection` and probes it with
# `DBI::dbListTables()`. It does not connect. `$autoconnection` would connect and
# would hide a close.
connection_states <- function(tables) {
  vapply(tables, function(s) s$dbconnection$is_connected(), logical(1))
}

# Runs the task with `pbmcapply::pbmclapply` replaced, and returns what the
# replacement saw at the fork boundary.
#
# The replacement returns one element per plan, so the caller's own error check
# finds no `try-error`. Nothing forks.
run_and_observe_fork <- function(task, record) {
  observed <- new.env(parent = emptyenv())
  observed$called <- FALSE

  testthat::local_mocked_bindings(
    pbmclapply = function(X, FUN, ...) {
      dots <- list(...)
      observed$called <- TRUE
      observed$plans_n <- length(X)
      observed$mc_cores <- dots$mc.cores
      observed$task_tables <- connection_states(task$tables)
      observed$config_tables <- connection_states(config$tables)
      observed$plans_run <- record$plans_run
      lapply(seq_along(X), function(i) 1)
    },
    .package = "pbmcapply"
  )

  suppressMessages(task$run(cores = 2))
  observed
}

test_that("Task$run() holds no open connection when it calls pbmclapply", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_task_table(dbconfig)
  withr::defer(tab$disconnect())

  record <- new.env(parent = emptyenv())
  record$plans_run <- integer(0)
  task <- new_task(list("anon_fork" = tab), record)

  # Open every connection first. Without this the assertions below could pass on
  # connections that nothing ever opened.
  tab$connect()
  for (s in config$tables) s$connect()

  # `all(logical(0))` is TRUE, so the two `all()` calls below would pass on an
  # empty list. These two lengths make that guard local to this block, rather
  # than leave it to the two `expect_length()` calls at the fork boundary.
  expect_length(connection_states(task$tables), 1L)
  expect_length(connection_states(config$tables), 4L)

  expect_true(all(connection_states(task$tables)))
  expect_true(all(connection_states(config$tables)))

  observed <- run_and_observe_fork(task, record)

  # The run really took the parallel branch and really reached the fork site.
  # `plans_index` is `2:(length(self$plans) - 1)`, so the mock receives 2 plans.
  expect_true(observed$called)
  expect_identical(observed$plans_n, 2L)
  expect_identical(observed$mc_cores, 2)

  # A vapply over an empty list returns logical(0), and `any(logical(0))` is
  # FALSE. These two lengths stop the assertions below passing on nothing.
  expect_length(observed$task_tables, 1L)
  expect_length(observed$config_tables, 4L)

  expect_false(any(observed$task_tables))
  expect_false(any(observed$config_tables))
})

test_that("Task$run() finishes plan 1 sequentially before it calls pbmclapply", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_task_table(dbconfig)
  withr::defer(tab$disconnect())

  record <- new.env(parent = emptyenv())
  record$plans_run <- integer(0)
  task <- new_task(list("anon_fork" = tab), record)

  observed <- run_and_observe_fork(task, record)

  # Plan 1 runs through `run_sequential()`, and the disconnect sweep is the last
  # thing that method does. So "plan 1 already ran" and "the sweep already ran"
  # are the same fact. This assertion names the ordering directly.
  expect_true(observed$called)
  expect_identical(observed$plans_run, 1L)

  # Plan 4 is the other sequential plan, and it runs after the fork.
  expect_identical(record$plans_run, c(1L, 4L))
})
