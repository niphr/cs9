# Task$run() forks its workers once. Each worker takes the next middle plan
# until none is left, and keeps its database connections from one plan to the
# next. Before 26.10.20 each middle plan had its own fork, which opened its own
# connections.
#
# The workers fork, so these tests skip on Windows. The fixtures are in
# helper-run-staging.R. Each plan saves what it saw in a file, because a
# forked worker cannot change an object of the main process.

# Saves the state of the process at the start of the action of a plan.
record_worker_state <- function(ctl) {
  return(function(tables, argset) {
    hash <- config$tables$config_data_hash_for_each_plan$dbconnection
    state <- list(
      plan = as.integer(argset$plan),
      pid = Sys.getpid(),
      plan_id = if (is.null(config$run$plan_id)) "" else config$run$plan_id,
      context = csdb::csdb_get_context(),
      out_open = tables$out$dbconnection$is_connected(),
      hash_open = hash$is_connected()
    )
    dput(state, file.path(ctl$dir, paste0("state-", argset$plan)))
    return(invisible(NULL))
  })
}

record_state_in_every_plan <- function(ctl, n_plans) {
  for (p in seq_len(n_plans)) {
    ctl$hooks[[as.character(p)]] <- record_worker_state(ctl)
  }
  return(invisible(NULL))
}

# One row per plan, in plan order. `later` is TRUE for each plan that its
# worker ran after an earlier plan. A worker takes the plans in plan order.
worker_states <- function(ctl) {
  files <- list.files(ctl$dir, pattern = "^state-", full.names = TRUE)
  d <- data.table::rbindlist(lapply(files, function(f) {
    return(data.table::as.data.table(dget(f)))
  }))
  data.table::setorderv(d, "plan")
  d[, later := seq_len(.N) > 1L, by = pid]
  return(d)
}

# R CMD check --as-cran sets _R_CHECK_LIMIT_CORES_, and parallel::mclapply()
# then stops above 2 processes with "4 simultaneous processes spawned". The
# tests with 4 workers lift that limit until they end. They run for seconds.
local_allow_four_workers <- function(envir = parent.frame()) {
  withr::local_envvar(c("_R_CHECK_LIMIT_CORES_" = "false"), .local_envir = envir)
  return(invisible(NULL))
}

# The plan ids of a task after its run, as Task$run() computed them.
task_plan_ids <- function(task) {
  return(run_plan_ids(
    task$plans,
    task$.__enclos_env__$private$injected_fields(),
    task$name
  ))
}

test_that("the middle plans run in at most `cores` forked workers", {
  skip_on_os("windows")
  local_allow_four_workers()
  db <- local_staging_db()
  out <- new_stage_out(db)
  ctl <- local_stage_ctl()
  record_state_in_every_plan(ctl, 10)

  err <- run_attempt(new_stage_task(out, n_plans = 10, cores = 4), cores = 4)

  expect_null(err)
  st <- worker_states(ctl)
  expect_identical(stage_calls(ctl, 10), rep(1L, 10))
  expect_identical(st$plan, 1:10)
  mid <- st[plan %in% 2:9]
  expect_true(all(st[plan %in% c(1L, 10L)]$pid == Sys.getpid()))
  expect_false(any(mid$pid == Sys.getpid()))
  # Before 26.10.20 the 8 middle plans ran in 8 processes.
  expect_lte(length(unique(mid$pid)), 4L)
})

test_that("a worker keeps its connections from one plan to the next", {
  skip_on_os("windows")
  db <- local_staging_db()
  out <- new_stage_out(db)
  ctl <- local_stage_ctl()
  record_state_in_every_plan(ctl, 10)

  err <- run_attempt(new_stage_task(out, n_plans = 10, cores = 2), cores = 2)

  expect_null(err)
  mid <- worker_states(ctl)[plan %in% 2:9]
  # 8 plans in at most 2 workers, so a worker ran more than one plan.
  expect_true(any(mid$later))
  # The action of the previous plan wrote to the table.
  expect_true(all(mid[later == TRUE]$out_open))
  # The plan read its last data hash before the action.
  expect_true(all(mid$hash_open))
})

for (kind in c("plain", "partitioned")) {
  test_that(
    paste0(
      "each plan in a worker has its own plan id and context (",
      kind,
      ")"
    ),
    {
      skip_on_os("windows")
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      ctl <- local_stage_ctl()
      record_state_in_every_plan(ctl, 10)
      task <- new_stage_task(out, n_plans = 10, cores = 2)
      before <- csdb::csdb_set_context("cs9test|outer")
      withr::defer(csdb::csdb_set_context(before))

      err <- run_attempt(task, run_key = "ids", cores = 2)

      expect_null(err)
      st <- worker_states(ctl)
      expect_true(any(st[plan %in% 2:9]$later))
      expect_identical(st$plan_id, task_plan_ids(task))
      expect_identical(st$context, paste0(task$name, "|", 1:10))
      expect_identical(csdb::csdb_get_context(), "cs9test|outer")
    }
  )
}

test_that("the staged rows of each plan carry the id of that plan", {
  skip_on_os("windows")
  db <- local_staging_db()
  out <- new_stage_out(db)
  ctl <- local_stage_ctl()
  task <- new_stage_task(out, n_plans = 10, cores = 2)
  # The run stops after the middle plans, so their rows stay staged.
  ctl$fail <- 10L

  err <- run_attempt(task, run_key = "rows", cores = 2)

  expect_s3_class(err, "error")
  con <- DBI::dbConnect(RSQLite::SQLite(), config$dbconfigs$anon$db)
  withr::defer(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(
    con,
    "SELECT x, cs9_plan FROM anon_stage_plain_cs9stg"
  )
  plan_of_row <- as.integer(sub("^new-([0-9]+)-.*$", "\\1", rows$x))
  expect_setequal(plan_of_row, 1:10)
  expect_identical(rows$cs9_plan, task_plan_ids(task)[plan_of_row])
})

test_that("a worker runs its next plan after a plan fails", {
  skip_on_os("windows")
  db <- local_staging_db()
  out <- new_stage_out(db)
  ctl <- local_stage_ctl()
  # Each of the 2 workers meets a failing plan. A worker that stopped at its
  # first failure would leave plan 5 to nobody.
  ctl$fail <- 2:4

  err <- run_attempt(new_stage_task(out, n_plans = 6, cores = 2), cores = 2)

  # Plan 6 never runs, because Task$run() stops after the middle plans.
  expect_identical(stage_calls(ctl, 6), c(1L, 1L, 1L, 1L, 1L, 0L))
  msg <- error_text(err)
  expect_match(msg, "3 of 4 parallel plans failed.", fixed = TRUE)
  at <- vapply(
    paste0("plan ", 2:4, " failed on purpose"),
    function(p) as.integer(regexpr(p, msg, fixed = TRUE)),
    integer(1)
  )
  expect_true(all(at > 0))
  expect_false(is.unsorted(at))
  expect_no_match(msg, "plan 5 failed", fixed = TRUE)
})

test_that("a plan whose worker dies counts as a failed plan", {
  skip_on_os("windows")
  db <- local_staging_db()
  out <- new_stage_out(db)
  ctl <- local_stage_ctl()
  ctl$hooks[["3"]] <- function(tables, argset) {
    tools::pskill(Sys.getpid(), tools::SIGKILL)
  }

  err <- run_attempt(new_stage_task(out, n_plans = 6, cores = 2), cores = 2)

  # The other worker runs plans 4 and 5. Plan 6 never runs.
  expect_identical(stage_calls(ctl, 6), c(1L, 1L, 1L, 1L, 1L, 0L))
  msg <- error_text(err)
  expect_match(msg, "1 of 4 parallel plans failed.", fixed = TRUE)
  expect_match(msg, "Error in index 3.", fixed = TRUE)
})

test_that("a retry with 4 workers runs only the plans that are not done", {
  skip_on_os("windows")
  local_allow_four_workers()
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  task <- new_stage_task(out, n_plans = 10, cores = 4)

  ctl$fail <- 5L
  err1 <- run_attempt(task, run_key = "retry4", cores = 4)
  expect_s3_class(err1, "error")
  expect_equal(live_rows(out), old_rows())

  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "retry4", cores = 4)

  expect_null(err2)
  # Attempt 1 ran plans 1 to 9, and plan 5 failed. Attempt 2 ran plan 1, plan
  # 5 and plan 10.
  expect_identical(
    stage_calls(ctl, 10),
    c(2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L)
  )
  expect_equal(live_rows(out), expected_rows(1:10))
  expect_equal(staged_count(out), 0)
})
