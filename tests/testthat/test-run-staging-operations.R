# The destructive writes of a task run: drop_all_rows(), drop_rows_where(),
# keep_rows_where(), the drop-and-write methods and replace_all_rows(). A run
# records each one as an operation and runs it on the live table at publish,
# before it inserts the staged rows. The end state MUST be the one that the
# same writes gave before 26.10.14, when they changed the live table at once.
#
# Each test also reads the live table during the run, which MUST still hold the
# old rows. The fixtures are in helper-run-staging.R.

# Runs a task whose plans write only through their hooks, and returns the
# error, the live rows and the live row count that plan 4 read.
run_hooks <- function(out, hooks, ctl, run_key = "ops") {
  ctl$drop <- FALSE
  ctl$rows <- function(plan) NULL
  ctl$hooks <- hooks
  ctl$hooks[["4"]] <- function(tables, argset) {
    if (!is.null(hooks[["4"]])) {
      hooks[["4"]](tables, argset)
    }
    return(record_live_nrow(ctl)(tables, argset))
  }
  err <- run_attempt(new_stage_task(out), run_key = run_key)
  return(list(
    error = err,
    live = live_rows(out),
    seen = seen_nrow(ctl, 4),
    seen_x = seen_x(ctl, 4)
  ))
}

rows <- function(x, part, n) {
  return(data.table::data.table(x = x, part = part, n = as.integer(n)))
}

for (kind in c("plain", "partitioned")) {
  test_that(
    paste0(
      "delete, upsert and keep give the end state of a live run (",
      kind,
      ")"
    ),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      hooks <- list(
        "1" = function(tables, argset) {
          tables$out$drop_rows_where("x = 'old-1'")
          tables$out$insert_data(rows("a", "p1", 1))
          return(invisible(NULL))
        },
        "2" = function(tables, argset) {
          # Replaces old-2 and adds b.
          tables$out$upsert_data(rows(c("old-2", "b"), c("p2", "p2"), c(22, 2)))
          return(invisible(NULL))
        },
        "3" = function(tables, argset) {
          # Within one plan, a delete also removes the rows that the plan wrote.
          tables$out$insert_data(rows(
            c("c", "c-gone"),
            c("p3", "p3"),
            c(3, 33)
          ))
          tables$out$drop_rows_where("x = 'c-gone'")
          return(invisible(NULL))
        },
        "4" = function(tables, argset) {
          tables$out$keep_rows_where("x <> 'old-3'")
          return(invisible(NULL))
        }
      )

      got <- run_hooks(out, hooks, ctl)

      expect_null(got$error)
      expect_equal(got$seen, 3)
      expect_equal(got$seen_x, old_rows()$x)
      expected <- rows(
        c("a", "b", "c", "old-2"),
        c("p1", "p2", "p3", "p2"),
        c(1, 2, 3, 22)
      )
      expect_equal(got$live, expected)
    }
  )

  test_that(
    paste0(
      "a delete of rows that another plan wrote stops the run (",
      kind,
      ")"
    ),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      hooks <- list(
        "2" = function(tables, argset) {
          tables$out$insert_data(rows("b", "p2", 2))
          return(invisible(NULL))
        },
        "3" = function(tables, argset) {
          tables$out$drop_rows_where("x = 'b'")
          return(invisible(NULL))
        }
      )

      got <- run_hooks(out, hooks, ctl)

      expect_s3_class(got$error, "error")
      expect_match(
        error_text(got$error),
        "delete after write is not supported during a task run",
        fixed = TRUE
      )
      expect_equal(got$live, old_rows())
    }
  )

  test_that(paste0("two plans that write one row stop the run (", kind, ")"), {
    db <- local_staging_db()
    out <- new_stage_out(db, kind)
    seed_old_rows(out)
    ctl <- local_stage_ctl()
    hooks <- list(
      "2" = function(tables, argset) {
        tables$out$upsert_data(rows("same", "p1", 2))
        return(invisible(NULL))
      },
      "3" = function(tables, argset) {
        tables$out$upsert_data(rows("same", "p1", 3))
        return(invisible(NULL))
      }
    )

    got <- run_hooks(out, hooks, ctl)

    expect_s3_class(got$error, "error")
    expect_match(
      error_text(got$error),
      "two plans wrote the same row",
      fixed = TRUE
    )
    expect_equal(got$live, old_rows())
  })

  test_that(
    paste0(
      "the drop-and-write methods give the end state of a live run (",
      kind,
      ")"
    ),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      hooks <- list(
        "1" = function(tables, argset) {
          tables$out$drop_all_rows_and_then_insert_data(rows("a", "p1", 1))
          return(invisible(NULL))
        },
        "2" = function(tables, argset) {
          tables$out$upsert_data(rows("b", "p2", 2))
          return(invisible(NULL))
        }
      )

      got <- run_hooks(out, hooks, ctl)

      expect_null(got$error)
      expect_equal(got$seen, 3)
      expect_equal(got$seen_x, old_rows()$x)
      expect_equal(got$live, rows(c("a", "b"), c("p1", "p2"), c(1, 2)))
    }
  )

  test_that(paste0("drop_all_rows_and_then_upsert_data() gives the end state of a live run (", kind, ")"), {
    db <- local_staging_db()
    out <- new_stage_out(db, kind)
    seed_old_rows(out)
    ctl <- local_stage_ctl()
    hooks <- list(
      "1" = function(tables, argset) {
        tables$out$drop_all_rows_and_then_upsert_data(rows(c("a", "b"), c("p1", "p2"), c(1, 2)))
        return(invisible(NULL))
      },
      "2" = function(tables, argset) {
        tables$out$insert_data(rows("c", "p3", 3))
        return(invisible(NULL))
      }
    )

    got <- run_hooks(out, hooks, ctl, run_key = "drop-then-upsert")

    expect_null(got$error)
    # Plan 4 read the old rows: the drop and the upsert of plan 1 were staged.
    expect_equal(got$seen, 3)
    expect_equal(got$seen_x, old_rows()$x)
    expect_equal(got$live, rows(c("a", "b", "c"), c("p1", "p2", "p3"), c(1, 2, 3)))
  })

  test_that(
    paste0(
      "a publish that fails changes no live table and no mark (",
      kind,
      ")"
    ),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      # Plans 1 and 2 add rows to p1 and p2. Plan 3 inserts old-3 into p3 again,
      # without a delete. The insert into p3 fails at publish, on the key that
      # the live table already holds.
      hooks <- list(
        "1" = function(tables, argset) {
          tables$out$insert_data(rows("a", "p1", 1))
          return(invisible(NULL))
        },
        "2" = function(tables, argset) {
          tables$out$insert_data(rows("b", "p2", 2))
          return(invisible(NULL))
        },
        "3" = function(tables, argset) {
          tables$out$insert_data(rows("old-3", "p3", 3))
          return(invisible(NULL))
        }
      )

      got <- run_hooks(out, hooks, ctl, run_key = "publish-fails")

      expect_s3_class(got$error, "error")
      expect_equal(got$live, old_rows())
      # The run is not marked as published. The staged rows and the done marks
      # stay for a retry.
      run_row <- store_rows("cs9_run", "publish-fails")
      expect_equal(nrow(run_row), 1L)
      expect_true(all(is.na(run_row$published_datetime)))
      expect_equal(staged_count(out), 3)
      expect_equal(nrow(store_rows("cs9_run_plan_done", "publish-fails")), 4L)
    }
  )
}

test_that("replace_all_rows() during a run gives the end state of a live run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  hooks <- list(
    "1" = function(tables, argset) {
      d <- rows(c("r1", "r2"), c("p1", "p2"), c(1, 2))
      d[, auto_last_updated_datetime := "2026-01-01 00:00:00"]
      tables$out$replace_all_rows(as.data.frame(d))
      return(invisible(NULL))
    }
  )

  got <- run_hooks(out, hooks, ctl)

  expect_null(got$error)
  expect_equal(got$seen, 3)
  expect_equal(got$seen_x, old_rows()$x)
  expect_equal(got$live, rows(c("r1", "r2"), c("p1", "p2"), c(1, 2)))
})

test_that("a direct write to one partition of a staged table stops", {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  hooks <- list(
    "2" = function(tables, argset) {
      tables$out$tables[["p2"]]$drop_all_rows()
      return(invisible(NULL))
    }
  )

  got <- run_hooks(out, hooks, ctl)

  expect_s3_class(got$error, "error")
  expect_match(
    error_text(got$error),
    "Write through the partitioned table",
    fixed = TRUE
  )
  expect_equal(got$live, old_rows())
})
