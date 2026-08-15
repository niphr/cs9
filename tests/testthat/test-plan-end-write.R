# `upsert_at_end_of_each_plan` and `insert_at_end_of_each_plan` MUST write what
# the analysis returned.
#
# `run_sequential()` collects the return value of every analysis in a plan, and
# then flattens it with
# `csutil::unnest_dfs_within_list_of_fully_named_lists()`. The flattened names
# index `tables`, so an analysis that returns a bare data.frame writes to the
# table registered under `"output"`.
#
# No test reached either feature before 26.8.19. That mattered, because those two
# lines called `splutil::`, and `splutil` was in neither `Imports` nor `Suggests`.
# `splutil` and `csutil` publish the same utilities under two names, and the
# NorSySS pods carry `csutil` alone. So both features raised
# `there is no package called 'splutil'` on every pod, and nothing installed the
# package that would have fixed it. `R CMD check` cannot see the call, because it
# sits inside an R6 method. See `CLAUDE.md`.
#
# The fixtures below repeat `tests/testthat/test-fork-ordering.R`. testthat
# sources each test file into its own environment, so a helper in one file is not
# visible in another.

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

  reload_db_config()
  withr::defer(for (tab in config$tables) tab$disconnect(), envir = envir)

  config$dbconfigs$anon
}

new_output_table <- function(dbconfig) {
  DBTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name = "anon_plan_end",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
}

# One plan, so `Task$run()` takes the sequential branch. The analysis returns a
# bare data.table, which is the shape that exercises
# `returned_name_when_dfs_are_not_nested`.
#
# The argset entries are `val_x` and `val_n`, not `x` and `n`. `add_analysis()`
# has the formals `name`, `fn`, `fn_name`, `...`, so R partial-matches a `n =`
# argument to `name` and the plan then fails with `subscript out of bounds`.
new_plans_returning <- function(rows) {
  p <- plnr::Plan$new()
  p$add_data(name = "d", fn = function() data.table::data.table(seed = 1L))
  p$add_analysis(
    fn = function(data, argset, tables) {
      data.table::data.table(x = argset$val_x, n = argset$val_n)
    },
    val_x = rows$x,
    val_n = rows$n
  )
  list(p)
}

# `tables` is keyed by the name the flattening produces, not by the table name.
new_task_writing_at_end <- function(table, rows, upsert, insert) {
  Task$new(
    name_grouping = "planend",
    name_action = if (upsert) "upsert" else "insert",
    plans = new_plans_returning(rows),
    tables = list("output" = table),
    cores = 1,
    upsert_at_end_of_each_plan = upsert,
    insert_at_end_of_each_plan = insert
  )
}

test_that("upsert_at_end_of_each_plan writes what the analysis returned", {
  dbconfig <- local_sqlite_dbconfig()
  table <- new_output_table(dbconfig)
  withr::defer(table$disconnect())

  task <- new_task_writing_at_end(
    table,
    rows = list(x = "a", n = 7L),
    upsert = TRUE,
    insert = FALSE
  )

  suppressMessages(task$run(cores = 1))

  got <- data.table::setDT(table$tbl() |> dplyr::collect())
  expect_equal(nrow(got), 1L)
  expect_equal(got$x, "a")
  expect_equal(got$n, 7L)
})

test_that("insert_at_end_of_each_plan writes what the analysis returned", {
  dbconfig <- local_sqlite_dbconfig()
  table <- new_output_table(dbconfig)
  withr::defer(table$disconnect())

  task <- new_task_writing_at_end(
    table,
    rows = list(x = "b", n = 9L),
    upsert = FALSE,
    insert = TRUE
  )

  suppressMessages(task$run(cores = 1))

  got <- data.table::setDT(table$tbl() |> dplyr::collect())
  expect_equal(nrow(got), 1L)
  expect_equal(got$x, "b")
  expect_equal(got$n, 9L)
})

# The flattening is what makes the two features work, so name it directly. A
# reader who sees the two tests above pass does not otherwise learn which
# function did the work.
#
# Two frames, with different columns in a different order. One frame would leave
# `use.names` and `fill` inert, and the call passes both.
test_that("csutil provides the flattening that both features depend on", {
  expect_true(requireNamespace("csutil", quietly = TRUE))

  flat <- csutil::unnest_dfs_within_list_of_fully_named_lists(
    list(
      data.table::data.table(x = "a", n = 1L),
      data.table::data.table(n = 2L, x = "b", extra = "z")
    ),
    returned_name_when_dfs_are_not_nested = "output",
    use.names = TRUE,
    fill = TRUE
  )

  expect_named(flat, "output")
  expect_true(is.data.frame(flat$output))

  # use.names matched the reordered columns rather than binding by position.
  expect_equal(flat$output$x, c("a", "b"))
  expect_equal(flat$output$n, c(1L, 2L))

  # fill supplied the column the first frame does not carry.
  expect_true("extra" %in% names(flat$output))
  expect_equal(flat$output$extra, c(NA, "z"))
})

# The parallel branch has its own copy of the two write blocks, at
# `R/r6_Task.R:351` and `:357`. The tests above drive `run_sequential()` and
# reach neither.
#
# That copy fails worse than the sequential one. The worker wraps its body in a
# `tryCatch` with five attempts and a five second sleep between them, so a
# missing package costs 25 seconds before it surfaces as
# `Error in index N. ... there is no package called '<pkg>'`.
#
# `pbmcapply::pbmclapply` is mocked to run the worker in this process. Nothing
# forks, so the write is observable afterwards.
new_plans_returning_many <- function(n_plans) {
  lapply(1:n_plans, function(i) {
    p <- plnr::Plan$new()
    p$add_data(name = "d", fn = function() data.table::data.table(seed = 1L))
    p$add_analysis(
      fn = function(data, argset, tables) {
        data.table::data.table(x = argset$val_x, n = argset$val_n)
      },
      index = i,
      index_plan = i,
      val_x = letters[i],
      val_n = i
    )
    p
  })
}

local_serial_pbmclapply <- function(envir = parent.frame()) {
  testthat::local_mocked_bindings(
    pbmclapply = function(X, FUN, ...) {
      dots <- list(...)
      lapply(
        X,
        FUN,
        tables = dots$tables,
        upsert_at_end_of_each_plan = dots$upsert_at_end_of_each_plan,
        insert_at_end_of_each_plan = dots$insert_at_end_of_each_plan
      )
    },
    .package = "pbmcapply",
    .env = envir
  )
}

# Four plans is the smallest count that reaches the parallel branch. Plans 1 and
# 4 go through `run_sequential()`, plans 2 and 3 through the mock.
new_parallel_task <- function(table, upsert, insert) {
  Task$new(
    name_grouping = "planend",
    name_action = if (upsert) "parupsert" else "parinsert",
    plans = new_plans_returning_many(4),
    tables = list("output" = table),
    cores = 2,
    upsert_at_end_of_each_plan = upsert,
    insert_at_end_of_each_plan = insert
  )
}

# The two sites are separate blocks, so one test cannot cover both. Setting only
# `upsert` leaves `R/r6_Task.R:357` unreached.
test_that("the parallel branch upserts at the end of each plan", {
  dbconfig <- local_sqlite_dbconfig()
  table <- new_output_table(dbconfig)
  withr::defer(table$disconnect())

  task <- new_parallel_task(table, upsert = TRUE, insert = FALSE)
  local_serial_pbmclapply()

  suppressMessages(task$run(cores = 2))

  got <- data.table::setDT(table$tbl() |> dplyr::collect())
  data.table::setorderv(got, "x")
  expect_equal(nrow(got), 4L)
  expect_equal(got$x, c("a", "b", "c", "d"))
  expect_equal(got$n, 1:4)
})

test_that("the parallel branch inserts at the end of each plan", {
  dbconfig <- local_sqlite_dbconfig()
  table <- new_output_table(dbconfig)
  withr::defer(table$disconnect())

  task <- new_parallel_task(table, upsert = FALSE, insert = TRUE)
  local_serial_pbmclapply()

  suppressMessages(task$run(cores = 2))

  got <- data.table::setDT(table$tbl() |> dplyr::collect())
  data.table::setorderv(got, "x")
  expect_equal(nrow(got), 4L)
  expect_equal(got$x, c("a", "b", "c", "d"))
  expect_equal(got$n, 1:4)
})
