# A task run MUST NOT change its tables until every plan has succeeded. It
# stages every write and publishes the staged writes in one transaction after
# the last plan.
#
# Before 26.10.14 a task that rebuilt its table emptied it in plan 1 and
# refilled it plan by plan. During the run, and after a failed run, a reader
# saw an empty or partial table. These tests drive the real Task$run() on
# SQLite, with a plain table and with a partitioned table of 3 partitions. The
# fixtures are in helper-run-staging.R.

for (kind in c("plain", "partitioned")) {
  test_that(
    paste0("a successful run publishes exactly the new rows (", kind, ")"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      rec <- local_last_updated_recorder()

      err <- run_attempt(new_stage_task(out), run_key = "success")

      expect_null(err)
      expect_equal(live_rows(out), expected_rows(1:4))
      expect_equal(staged_count(out), 0)
      # Before 26.10.14 every write recorded last-updated: one drop and four
      # inserts made five calls.
      expect_length(rec$calls, 1L)
      expected_names <- if (kind == "plain") {
        "anon_stage_plain"
      } else {
        paste0("anon_stage_part_xxpxx_", c("p1", "p2", "p3"))
      }
      expect_setequal(rec$calls[[1]], expected_names)
    }
  )

  test_that(
    paste0("a plan that fails leaves the live rows unchanged (", kind, ")"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      ctl$fail <- 3L

      err <- run_attempt(new_stage_task(out), run_key = "first")

      expect_s3_class(err, "error")
      expect_match(error_text(err), "plan 3 failed on purpose", fixed = TRUE)
      expect_equal(live_rows(out), old_rows())

      # A second run with a new key publishes exactly the new rows. Nothing that
      # the failed run staged reaches the live table.
      ctl$fail <- integer()
      err2 <- run_attempt(new_stage_task(out), run_key = "second")

      expect_null(err2)
      expect_equal(live_rows(out), expected_rows(1:4))
      expect_equal(staged_count(out), 0)
    }
  )

  test_that(
    paste0("a plan reads the old live rows during the run (", kind, ")"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      ctl$hooks[["2"]] <- record_live_nrow(ctl)
      ctl$hooks[["4"]] <- record_live_nrow(ctl)

      err <- run_attempt(new_stage_task(out), run_key = "reads-old")

      expect_null(err)
      # Plan 1 emptied the table and wrote 3 rows. Before 26.10.14 plan 2 then
      # read 3 new rows and plan 4 read 9. The old table held 3 rows.
      expect_equal(seen_nrow(ctl, 2), 3)
      expect_equal(seen_nrow(ctl, 4), 3)
      expect_equal(seen_x(ctl, 2), old_rows()$x)
      expect_equal(seen_x(ctl, 4), old_rows()$x)
      expect_equal(live_rows(out), expected_rows(1:4))
    }
  )

  test_that(
    paste0("staged_tbl() reads the rows that the run staged (", kind, ")"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      ctl$hooks[["4"]] <- function(tables, argset) {
        fn <- tables$out$staged_tbl
        got <- if (is.function(fn)) nrow(dplyr::collect(fn())) else -1
        cat(got, "\n", sep = "", file = file.path(ctl$dir, "staged-4"))
        return(invisible(NULL))
      }

      err <- run_attempt(new_stage_task(out), run_key = "staged-tbl")

      expect_null(err)
      # Plans 1 to 3 staged 3 rows each before plan 4 wrote its own.
      expect_equal(as.numeric(readLines(file.path(ctl$dir, "staged-4"))), 9)
    }
  )

  test_that(
    paste0("a second attempt of a published run does nothing (", kind, ")"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      task <- new_stage_task(out)

      err1 <- run_attempt(task, run_key = "done-once")
      # The rows that a repeated run would insert, with a different value.
      ctl$rows <- function(plan) stage_rows_of(plan, n = 99L)
      err2 <- run_attempt(task, run_key = "done-once")

      expect_null(err1)
      expect_null(err2)
      expect_equal(stage_calls(ctl, 4), c(1L, 1L, 1L, 1L))
      expect_equal(live_rows(out), expected_rows(1:4))
    }
  )
}

test_that("a table written outside a task run changes at once", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")

  # The error of a write, or NULL, so an expectation on it fails rather than
  # errors.
  write <- function(fn) {
    return(tryCatch(
      {
        suppressMessages(fn())
        NULL
      },
      error = function(e) e
    ))
  }

  expect_null(write(function() out$insert_data(old_rows())))
  expect_equal(live_rows(out), old_rows())
  expect_null(write(function() out$drop_rows_where("x = 'old-1'")))
  expect_equal(live_rows(out), old_rows()[2:3])
  expect_equal(staged_count(out), 0)
})

# A run without a run_key is how an interactive run, a vignette and every task
# that the scheduler does not yet start with a key run. It MUST write as cs9
# did before 26.10.14, and it MUST NOT create a bookkeeping or staging table.
for (kind in c("plain", "partitioned")) {
  test_that(paste0("a run without a run_key writes the table at once (", kind, ")"), {
    db <- local_staging_db()
    out <- new_stage_out(db, kind)
    seed_old_rows(out)
    ctl <- local_stage_ctl()
    ctl$hooks[["2"]] <- record_live_nrow(ctl)

    err <- run_attempt(new_stage_task(out), run_key = NULL)

    expect_null(err)
    # Plan 1 emptied the table and wrote its 3 rows, and plan 2 read them.
    expect_equal(seen_x(ctl, 2), stage_rows_of(1)$x)
    expect_equal(live_rows(out), expected_rows(1:4))
    made <- character()
    for (f in c(config$dbconfigs$anon$db, config$dbconfigs$config$db)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), f)
      made <- c(made, DBI::dbListTables(con))
      DBI::dbDisconnect(con)
    }
    expect_equal(grep("^cs9_run|_cs9stg$", made, value = TRUE), character())
  })
}

test_that("a task without a table of its own runs without staging", {
  db <- local_staging_db()
  ctl <- local_stage_ctl()
  ctl$rows <- function(plan) NULL
  ctl$drop <- FALSE
  task <- task_from_config_v9(
    name_grouping = "stage",
    name_action = "notables",
    for_each_plan = plnr::expand_list(plan = 1:2),
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = list()
  )

  err <- run_attempt(task)

  expect_null(err)
  expect_equal(stage_calls(ctl, 2), c(1L, 1L))
})
