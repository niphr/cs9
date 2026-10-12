# A staged run with forked plans. Task$run() with 2 cores and 6 plans runs
# plan 1 first, plans 2 to 5 in forked workers, and plan 6 last. A worker
# stages its rows and marks its plan as done over its own connection. The
# publish runs in the main process after plan 6.
#
# The workers fork, so these tests skip on Windows. The fixtures are in
# helper-run-staging.R.

for (kind in c("plain", "partitioned")) {
  test_that(
    paste0("a parallel run publishes exactly the new rows (", kind, ")"),
    {
      skip_on_os("windows")
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      ctl$hooks[["3"]] <- record_live_nrow(ctl)

      err <- run_attempt(
      new_stage_task(out, n_plans = 6, cores = 2),
      run_key = "parallel",
      cores = 2
    )

      expect_null(err)
      # Plan 3 runs in a forked worker. Before 26.10.14 it read the 3 rows that
      # plan 1 wrote after it emptied the table.
      expect_equal(seen_nrow(ctl, 3), 3)
      expect_equal(seen_x(ctl, 3), old_rows()$x)
      expect_equal(live_rows(out), expected_rows(1:6))
      expect_equal(staged_count(out), 0)
    }
  )

  test_that(
    paste0(
      "a parallel retry skips the forked plans that are done (",
      kind,
      ")"
    ),
    {
      skip_on_os("windows")
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      task <- new_stage_task(out, n_plans = 6, cores = 2)

      ctl$fail <- 4L
      err1 <- run_attempt(task, run_key = "par", cores = 2)
      expect_s3_class(err1, "error")
      expect_equal(live_rows(out), old_rows())

      ctl$fail <- integer()
      err2 <- run_attempt(task, run_key = "par", cores = 2)

      expect_null(err2)
      # Attempt 1 ran plans 1 to 5, and plan 4 failed. Plan 6 never started.
      expect_equal(stage_calls(ctl, 6), c(2L, 1L, 1L, 2L, 1L, 1L))
      expect_equal(live_rows(out), expected_rows(1:6))
      expect_equal(staged_count(out), 0)
    }
  )
}
