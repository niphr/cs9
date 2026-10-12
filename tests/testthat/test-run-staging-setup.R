# What a run with a run_key stages, and what it refuses.
#
# - A table of the config access is never staged. It is written directly,
#   also when a task lists it, while the task's other tables are staged.
# - A keyed run that cannot stage stops before any write. Before round 2 of
#   phase 21 it gave a warning and wrote its tables directly.
# - The automatic writes at the end of each plan are staged like any other.
# - A staging name stays within the 63-byte PostgreSQL limit.
#
# The fixtures are in helper-run-staging.R.

test_that("a table of the config access is written directly while the others are staged", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  cfg <- DBTableExtended_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "config_stage_direct",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(cfg$disconnect())
  ctl <- local_stage_ctl()
  for (p in as.character(1:4)) {
    ctl$hooks[[p]] <- function(tables, argset) {
      tables$cfg$insert_data(data.table::data.table(
        x = paste0("cfg-", argset$plan),
        part = "p1",
        n = argset$plan
      ))
      return(invisible(NULL))
    }
  }
  ctl$hooks[["2"]] <- function(tables, argset) {
    tables$cfg$insert_data(data.table::data.table(
      x = "cfg-2",
      part = "p1",
      n = 2L
    ))
    record_live_nrow(ctl)(tables, argset)
    cat(
      nrow(dplyr::collect(tables$cfg$tbl())),
      "\n",
      file = file.path(ctl$dir, "cfg-seen-2")
    )
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, cfg = cfg)),
    run_key = "with-config"
  )

  expect_null(err)
  # In plan 2 the config table already held the rows of plans 1 and 2, while
  # the staged table still held the old rows.
  seen_file <- file.path(ctl$dir, "cfg-seen-2")
  cfg_seen <- if (file.exists(seen_file)) {
    as.numeric(readLines(seen_file))
  } else {
    NA
  }
  expect_equal(cfg_seen, 2)
  expect_equal(seen_x(ctl, 2), old_rows()$x)
  expect_equal(live_rows(out), expected_rows(1:4))
  expect_equal(
    sort(dplyr::collect(cfg$tbl())$x),
    paste0("cfg-", 1:4)
  )
  expect_false(any(grepl("config_stage_direct_cs9stg", made_tables())))
})

test_that("a keyed run that cannot stage stops and writes nothing", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A second anon table in another SQLite file: the task's tables are then in
  # two databases, and one transaction cannot publish both.
  other_config <- db
  other_config$db <- file.path(dirname(db$db), "other.sqlite")
  other <- DBTableExtended_v9$new(
    dbconfig = other_config,
    table_name = "anon_stage_other",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(other$disconnect())
  seed_old_rows(other)
  ctl <- local_stage_ctl()
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$other$insert_data(stage_rows_of(argset$plan))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, other = other)),
    run_key = "two-databases"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(error_text(err), "more than one database", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
  expect_equal(live_rows(other), old_rows())
})

test_that("two tables with one name stop a keyed run before any write", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A table with the same name in another SQLite file.
  twin_config <- db
  twin_config$db <- file.path(dirname(db$db), "twin.sqlite")
  twin <- DBTableExtended_v9$new(
    dbconfig = twin_config,
    table_name = "anon_stage_plain",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(twin$disconnect())
  seed_old_rows(twin)
  ctl <- local_stage_ctl()
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$twin$insert_data(stage_rows_of(argset$plan))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, twin = twin)),
    run_key = "twins"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(
    error_text(err),
    "two tables share the name anon_stage_plain",
    fixed = TRUE
  )
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
  expect_equal(live_rows(twin), old_rows())
})

test_that("run_task() passes its run_key to the task run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$hooks[["2"]] <- record_live_nrow(ctl)
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  ss$add_task(
    name_grouping = "stage",
    name_action = "viass",
    for_each_plan = plnr::expand_list(plan = 1:4),
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = list(out = out)
  )

  err <- tryCatch(
    {
      suppressWarnings(suppressMessages(ss$run_task("stage_viass", run_key = "k")))
      NULL
    },
    error = function(e) e
  )

  expect_null(err)
  # The run staged: plan 2 read the old rows, and the run with key "k" is
  # marked as published.
  expect_equal(seen_x(ctl, 2), old_rows()$x)
  run_row <- store_rows("cs9_run", "k")
  expect_equal(nrow(run_row), 1L)
  expect_true(all(!is.na(run_row$published_datetime)))
  expect_equal(live_rows(out), expected_rows(1:4))
})

for (mode in c("insert", "upsert")) {
  test_that(
    paste0("a keyed run stages the ", mode, " at the end of each plan"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, "plain")
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      ctl$return_rows <- TRUE
      ctl$hooks[["2"]] <- record_live_nrow(ctl)

      err <- run_attempt(
        new_stage_task(
          out,
          tables = list(out = out, output = out),
          insert_at_end = mode == "insert",
          upsert_at_end = mode == "upsert"
        ),
        run_key = paste0("plan-end-", mode)
      )

      expect_null(err)
      # cs9 wrote the rows of plan 1 at the end of plan 1. Plan 2 still read
      # the old rows, so that write was staged.
      expect_equal(seen_x(ctl, 2), old_rows()$x)
      expect_equal(live_rows(out), expected_rows(1:4))
    }
  )
}

test_that("a staging name stays within 63 bytes and stays unique", {
  short <- "anon_norsyss_data"
  expect_equal(run_staging_name(short), "anon_norsyss_data_cs9stg")

  prefix <- strrep("a", 52)
  long_1 <- paste0(prefix, "_one_table")
  long_2 <- paste0(prefix, "_two_table")
  name_1 <- run_staging_name(long_1)
  name_2 <- run_staging_name(long_2)

  expect_lte(nchar(name_1, type = "bytes"), 63L)
  expect_lte(nchar(name_2, type = "bytes"), 63L)
  expect_true(endsWith(name_1, "_cs9stg"))
  expect_false(identical(name_1, name_2))
  expect_identical(run_staging_name(long_1), name_1)
})
