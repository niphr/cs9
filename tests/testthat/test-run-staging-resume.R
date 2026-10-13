# A second attempt of a run with the same run_key MUST resume the first one.
# It skips each plan that the first attempt finished, except plan 1, and
# publishes each row once.
#
# The scheduler retries a failed task with the same run_key. Before 26.10.14
# Task$run() had no run_key, so every attempt ran every plan again. The
# fixtures are in helper-run-staging.R.

for (kind in c("plain", "partitioned")) {
  test_that(
    paste0(
      "a retry with the same run_key skips the plans that are done (",
      kind,
      ")"
    ),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      task <- new_stage_task(out)

      # Plan 3 stages its rows and then fails, so it never gets its done mark.
      ctl$fail <- 3L
      err1 <- run_attempt(task, run_key = "dag-1 abc")
      expect_s3_class(err1, "error")
      expect_equal(live_rows(out), old_rows())

      ctl$fail <- integer()
      err2 <- run_attempt(task, run_key = "dag-1 abc")

      expect_null(err2)
      # Plan 1 runs on every attempt. Plan 2 is done after attempt 1. Plan 3
      # failed, and plan 4 never started.
      expect_equal(stage_calls(ctl, 4), c(2L, 1L, 2L, 1L))
      # Plan 3 staged its rows twice. The second attempt removed the rows of the
      # first before plan 3 ran again, so each row is published once.
      expect_equal(live_rows(out), expected_rows(1:4))
      expect_equal(staged_count(out), 0)
    }
  )
}

test_that("the last plan runs again after a publish that failed", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  # On attempt 1 the last plan stages a row with the key of an old row. Every
  # plan is done, and the publish fails on that key.
  ctl$rows <- function(plan) {
    if (plan == 4 && isTRUE(ctl$bad_last)) {
      return(data.table::data.table(x = "old-1", part = "p1", n = 4L))
    }
    return(stage_rows_of(plan))
  }
  ctl$bad_last <- TRUE
  task <- new_stage_task(out)

  err1 <- run_attempt(task, run_key = "last")
  expect_s3_class(err1, "error")
  expect_equal(live_rows(out), old_rows())

  # The last plan is fixed. A retry MUST run it again rather than skip it.
  ctl$bad_last <- FALSE
  err2 <- run_attempt(task, run_key = "last")

  expect_null(err2)
  expect_equal(stage_calls(ctl, 4), c(2L, 1L, 1L, 2L))
  expected <- rbind(old_rows(), expected_rows(1:4))
  data.table::setorderv(expected, "x")
  expect_equal(live_rows(out), expected)
})

test_that("a one-plan task runs its plan again on a retry", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$rows <- function(plan) {
    if (isTRUE(ctl$bad_last)) {
      return(data.table::data.table(x = "old-1", part = "p1", n = 1L))
    }
    return(stage_rows_of(plan))
  }
  ctl$bad_last <- TRUE
  task <- new_stage_task(out, n_plans = 1)

  err1 <- run_attempt(task, run_key = "one-plan")
  ctl$bad_last <- FALSE
  err2 <- run_attempt(task, run_key = "one-plan")

  expect_s3_class(err1, "error")
  expect_null(err2)
  expect_equal(stage_calls(ctl, 1), 2L)
})

test_that("an operation of a failed plan is not published", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$fail <- 3L
  ctl$hooks[["3"]] <- function(tables, argset) {
    tables$out$drop_rows_where("x = 'old-2'")
    return(invisible(NULL))
  }
  task <- new_stage_task(out)

  err1 <- run_attempt(task, run_key = "ops")
  expect_s3_class(err1, "error")
  expect_equal(live_rows(out), old_rows())

  # On the retry, plan 3 does not delete. The delete that it recorded on the
  # failed attempt MUST NOT reach the live table.
  ctl$fail <- integer()
  ctl$hooks <- list()
  err2 <- run_attempt(task, run_key = "ops")

  expect_null(err2)
  expected <- rbind(old_rows(), expected_rows(1:4))
  data.table::setorderv(expected, "x")
  expect_equal(live_rows(out), expected)
})

test_that("plan 1 runs again on a retry and sets the state that later plans read", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  state <- new.env(parent = emptyenv())
  ctl$hooks[["1"]] <- function(tables, argset) {
    state$enough <- 7L
    return(invisible(NULL))
  }
  # Plans 2 to 4 write the value that plan 1 set. Without it they stop.
  ctl$rows <- function(plan) {
    if (plan == 1) {
      return(stage_rows_of(1))
    }
    if (is.null(state$enough)) {
      stop("plan ", plan, " found no state from plan 1")
    }
    return(stage_rows_of(plan, n = state$enough))
  }
  ctl$fail <- 3L
  task <- new_stage_task(out)
  err1 <- run_attempt(task, run_key = "state")
  expect_s3_class(err1, "error")

  # A new process: the state of plan 1 is gone.
  rm("enough", envir = state)
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "state")

  expect_null(err2)
  expect_equal(stage_calls(ctl, 4), c(2L, 1L, 2L, 1L))
  expected <- rbind(stage_rows_of(1), expected_rows(2:4, n = 7L))
  data.table::setorderv(expected, "x")
  expect_equal(live_rows(out), expected)
})

test_that("a retry after midnight uses the date of the first attempt", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$rows <- function(plan) NULL
  ctl$hooks[["1"]] <- function(tables, argset) NULL
  for (p in as.character(1:4)) {
    ctl$hooks[[p]] <- function(tables, argset) {
      tables$out$insert_data(data.table::data.table(
        x = paste0("today-", argset$plan),
        part = "p1",
        n = as.integer(argset$today - as.Date("2026-01-01"))
      ))
      return(invisible(NULL))
    }
  }
  task <- new_stage_task(out)
  day <- as.Date("2026-03-01")
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(day)
    },
    .package = "lubridate"
  )

  ctl$fail <- 3L
  err1 <- run_attempt(task, run_key = "midnight")
  expect_s3_class(err1, "error")

  day <- as.Date("2026-03-02")
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "midnight")

  expect_null(err2)
  got <- live_rows(out)
  expect_equal(got$x, paste0("today-", 1:4))
  # Every plan saw 2026-03-01, the date of the first attempt.
  expect_equal(
    got$n,
    rep(as.integer(as.Date("2026-03-01") - as.Date("2026-01-01")), 4)
  )
})

test_that("a planner that changes the plan list between attempts stops the retry", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$plans <- function() 1:4
  task <- new_stage_task(out, planned = TRUE)

  ctl$fail <- 3L
  err1 <- run_attempt(task, run_key = "changed")
  expect_s3_class(err1, "error")

  # The planner now leaves out plan 2, which staged rows on attempt 1.
  ctl$plans <- function() c(1L, 3L, 4L)
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "changed")

  expect_s3_class(err2, "error")
  expect_match(
    error_text(err2),
    "the plan list changed between attempts of one run",
    fixed = TRUE
  )
  expect_equal(live_rows(out), old_rows())
})

test_that("a planner that adds a plan between attempts stops the retry", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$plans <- function() 1:4
  task <- new_stage_task(out, planned = TRUE)

  ctl$fail <- 3L
  err1 <- run_attempt(task, run_key = "added")
  expect_s3_class(err1, "error")

  ctl$plans <- function() 1:5
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "added")

  expect_match(
    error_text(err2),
    "the plan list changed between attempts of one run",
    fixed = TRUE
  )
  # The check runs before any plan: attempt 1 ran plans 1 to 3, and no plan
  # ran again.
  expect_equal(stage_calls(ctl, 5), c(1L, 1L, 1L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
})

test_that("a planner that reorders the plans between attempts stops the retry", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$plans <- function() 1:4
  task <- new_stage_task(out, planned = TRUE)

  ctl$fail <- 3L
  err1 <- run_attempt(task, run_key = "reordered")
  expect_s3_class(err1, "error")

  # The same plans, but plan 2 now comes first, so it is the plan that runs
  # again on every attempt.
  ctl$plans <- function() c(2L, 1L, 3L, 4L)
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "reordered")

  expect_match(
    error_text(err2),
    "the plan list changed between attempts of one run",
    fixed = TRUE
  )
  expect_equal(stage_calls(ctl, 4), c(1L, 1L, 1L, 0L))
  expect_equal(live_rows(out), old_rows())
})

test_that("a plan whose settings change before it ever ran stops the retry", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$plans <- function() 1:4
  task <- new_stage_task(out, planned = TRUE)

  # Plan 2 fails, so plans 3 and 4 never start and leave no trace.
  ctl$fail <- 2L
  err1 <- run_attempt(task, run_key = "changed-not-started")
  expect_s3_class(err1, "error")

  # Plan 4 now has other settings.
  ctl$plans <- function() c(1L, 2L, 3L, 40L)
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "changed-not-started")

  expect_match(
    error_text(err2),
    "the plan list changed between attempts of one run",
    fixed = TRUE
  )
  expect_equal(stage_calls(ctl, 4), c(1L, 1L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
})

test_that("a planner that reads staged rows stops the run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$plans <- function() {
    tables <- get("cs9stage_tables", envir = globalenv())
    fn <- tables$out$staged_tbl
    if (is.function(fn)) {
      fn()
    }
    return(1:4)
  }
  assign("cs9stage_tables", list(out = out), envir = globalenv())
  withr::defer(rm("cs9stage_tables", envir = globalenv()))
  task <- new_stage_task(out, planned = TRUE)

  err <- run_attempt(task, run_key = "reads-staged")

  expect_s3_class(err, "error")
  expect_match(
    error_text(err),
    "plans may only read published tables",
    fixed = TRUE
  )
  expect_equal(live_rows(out), old_rows())
})

test_that("two plans with the same settings stop the run before any plan runs", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  # Plans 2 and 3 differ only in index, which cs9 adds.
  ctl$plans <- function() c(1L, 2L, 2L, 4L)
  ctl$rows <- function(plan) NULL
  task <- new_stage_task(out, planned = TRUE)

  err <- run_attempt(task, run_key = "dup")

  expect_s3_class(err, "error")
  expect_match(error_text(err), "plans 2 and 3 of task", fixed = TRUE)
  expect_match(error_text(err), "have the same settings", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
})

test_that("a new run_key removes what a failed run with another key staged", {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  task <- new_stage_task(out)

  ctl$fail <- 3L
  err1 <- run_attempt(task, run_key = "old-key")
  expect_s3_class(err1, "error")
  expect_equal(live_rows(out), old_rows())

  ctl$fail <- integer()
  ctl$rows <- function(plan) stage_rows_of(plan, n = 50L + plan)
  err2 <- run_attempt(task, run_key = "new-key")

  expect_null(err2)
  expect_equal(stage_calls(ctl, 4), c(2L, 2L, 2L, 1L))
  got <- live_rows(out)
  expect_equal(got$x, expected_rows(1:4)$x)
  expect_equal(got$n, expected_rows(1:4)$n + 50L)
  expect_equal(staged_count(out), 0)
})
