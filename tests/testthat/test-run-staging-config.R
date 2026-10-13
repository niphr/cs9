# A config table is one of cs9's own config tables, the objects in
# config$tables. Sharing the database and the schema of the config access does
# not make a table a config table.
#
# In production the config and the anon accesses share the database AND the
# schema. Before 26.10.17 cs9 then counted every task table as a config table,
# staged nothing, and a run with a run_key ran unstaged (2026-10-13). These
# tests use that layout: in SQLite both accesses are one file.
#
# The fixtures are in helper-run-staging.R.

test_that("in the production layout a keyed run stages and publishes its table", {
  db <- local_staging_db(shared = TRUE)
  expect_identical(config$dbconfigs$config$db, config$dbconfigs$anon$db)
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$hooks[["2"]] <- record_live_nrow(ctl)

  err <- run_attempt(new_stage_task(out), run_key = "production layout")

  expect_null(err)
  # Plan 2 read the old rows, and the run is published.
  expect_equal(seen_x(ctl, 2), old_rows()$x)
  run_row <- store_rows("cs9_run", "production layout")
  expect_equal(nrow(run_row), 1L)
  expect_true(all(!is.na(run_row$published_datetime)))
  expect_equal(live_rows(out), expected_rows(1:4))
})

test_that("in the production layout a cs9 config table is written directly", {
  db <- local_staging_db(shared = TRUE)
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$cfg$insert_data(config_log_row("cfg-1"))
    return(invisible(NULL))
  }
  ctl$hooks[["2"]] <- function(tables, argset) {
    record_live_nrow(ctl)(tables, argset)
    writeLines(config_log_messages(), file.path(ctl$dir, "cfg-seen-2"))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(
      out,
      tables = list(out = out, cfg = config$tables$config_log)
    ),
    run_key = "production config"
  )

  expect_null(err)
  seen_file <- file.path(ctl$dir, "cfg-seen-2")
  cfg_seen <- if (file.exists(seen_file)) readLines(seen_file) else NA
  expect_true("cfg-1" %in% cfg_seen)
  expect_equal(seen_x(ctl, 2), old_rows()$x)
  expect_equal(nrow(store_rows("cs9_run", "production config")), 1L)
  expect_equal(live_rows(out), expected_rows(1:4))
})

test_that("in the production layout a plain csdb wrapper of a task table stops a keyed run", {
  db <- local_staging_db(shared = TRUE)
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A wrapper of the same live table through the config access. It shares the
  # database and the schema of config, but it is not a cs9 config table.
  wrapper <- csdb::DBTable_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "anon_stage_plain",
    field_types = c(
      x = "TEXT",
      part = "TEXT",
      n = "INTEGER",
      auto_last_updated_datetime = "DATETIME"
    ),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(wrapper$disconnect())
  ctl <- local_stage_ctl()

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, wrapper = wrapper)),
    run_key = "production wrapper"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
})

test_that("in the production layout a table name ending in _cs9stg is refused", {
  local_staging_db(shared = TRUE)
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  msg <- tryCatch(
    {
      ss$add_table(
        name_access = "anon",
        name_grouping = "other",
        name_variant = "cs9stg",
        field_types = c(x = "TEXT"),
        keys = "x"
      )
      ""
    },
    error = function(e) conditionMessage(e)
  )
  expect_match(
    msg,
    "names ending in _cs9stg are reserved for staging",
    fixed = TRUE
  )
})

test_that("a keyed task whose every table is a config table runs unstaged", {
  local_staging_db()
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$rows <- function(plan) NULL
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$cfg$insert_data(config_log_row("config only"))
    return(invisible(NULL))
  }
  task <- task_from_config_v9(
    name_grouping = "stage",
    name_action = "configonly",
    for_each_plan = plnr::expand_list(plan = 1:2),
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = list(cfg = config$tables$config_log)
  )

  err <- run_attempt(task, run_key = "config only")

  expect_null(err)
  expect_equal(stage_calls(ctl, 2), c(1L, 1L))
  expect_true("config only" %in% config_log_messages())
  expect_equal(made_tables(), character())
})

test_that("a keyed run with tables but no staged table stops", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  # Another way to no staged table than a config-only task. The first pass of
  # run_targets() leaves none today, so a mock makes one.
  testthat::local_mocked_bindings(
    run_targets = function(tables) {
      return(list(
        targets = list(),
        dbconfig = NULL,
        problem = NULL,
        config_only = FALSE
      ))
    }
  )

  err <- run_attempt(new_stage_task(out), run_key = "no staged table")

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
})
