# The publish moves the staged rows in batches of at most
# run_publish_rows_per_batch rows. On PostgreSQL each batch is a transaction
# of its own, and a publish that stops resumes at the next attempt with the
# same run_key. On SQLite one transaction holds the whole publish.
#
# On 2026-10-16 PostgreSQL went down twice during a publish of about 25
# million staged rows in one transaction. The tests set the batch size low, so
# their runs stay small. The SQLite fixtures are in helper-run-staging.R, and
# the PostgreSQL fixtures in helper-run-staging-pg.R.

# The functions before any mock. A probe wraps these, so a second probe in one
# test does not wrap the first.
real_publish_batch <- run_publish_batch
real_publish <- run_publish

# Records each batch of the publish: the rows that it moved. With `stop_after`,
# the publish stops before the batch after that many batches. `before` runs
# before each batch.
local_batch_probe <- function(
  stop_after = Inf,
  before = NULL,
  envir = parent.frame()
) {
  probe <- new.env(parent = emptyenv())
  probe$moved <- numeric()
  real <- real_publish_batch
  testthat::local_mocked_bindings(
    run_publish_batch = function(...) {
      if (length(probe$moved) >= stop_after) {
        stop("cs9 test: the publish stops after batch ", stop_after)
      }
      if (!is.null(before)) {
        before()
      }
      moved <- real(...)
      probe$moved <- c(probe$moved, as.numeric(moved))
      return(moved)
    },
    .env = envir
  )
  return(probe)
}

# SQLite ----

# One run of 4 plans that upsert 3 rows each at the end of the plan. Plan 2
# replaces the live row "aaa" in p1, so a batch before the last holds an
# upsert. With `size`, the publish moves at most `size` rows per batch.
batch_run_sqlite <- function(kind, size = NULL) {
  db <- local_staging_db()
  out <- new_stage_out(db, kind)
  seed_old_rows(out)
  con <- DBI::dbConnect(RSQLite::SQLite(), db$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  live <- if (kind == "plain") out$table_name else out$tables[[1]]$table_name
  DBI::dbAppendTable(
    con,
    live,
    data.frame(
      x = "aaa",
      part = "p1",
      n = 0L,
      auto_last_updated_datetime = "2026-01-01 00:00:00"
    )
  )
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$return_rows <- TRUE
  ctl$rows <- function(plan) {
    rows <- stage_rows_of(plan)
    if (plan == 2) {
      rows$x[1] <- "aaa"
    }
    return(rows)
  }
  if (!is.null(size)) {
    testthat::local_mocked_bindings(run_publish_rows_per_batch = size)
  }
  probe <- local_batch_probe()
  err <- run_attempt(
    new_stage_task(
      out,
      tables = list(out = out, output = out),
      upsert_at_end = TRUE
    ),
    run_key = "batches"
  )
  return(list(
    error = err,
    live = live_rows(out),
    staged = staged_count(out),
    moved = probe$moved
  ))
}

batch_expected_sqlite <- function() {
  d <- rbind(
    old_rows(),
    data.table::data.table(x = "aaa", part = "p1", n = 2L),
    expected_rows(1:4)[x != "new-2-1"]
  )
  data.table::setorderv(d, "x")
  return(d)
}

for (kind in c("plain", "partitioned")) {
  test_that(
    paste0(
      "on SQLite the publish moves the staged rows of each live table at once, whatever the batch size (",
      kind,
      ")"
    ),
    {
      one <- batch_run_sqlite(kind)
      two <- batch_run_sqlite(kind, size = 2L)

      expect_equal(error_text(one$error), "")
      expect_equal(error_text(two$error), "")
      expect_equal(two$live, one$live)
      expect_equal(two$live, batch_expected_sqlite())
      expect_equal(two$staged, 0)
      # 12 staged rows: one batch per live table, also with a batch size of 2.
      expect_equal(one$moved, if (kind == "plain") 12 else c(4, 4, 4))
      expect_equal(two$moved, one$moved)
    }
  )
}

test_that("on SQLite a publish that stops after batch 2 leaves the live rows unchanged", {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  task <- new_stage_task(out)

  probe <- local_batch_probe(stop_after = 2)
  err1 <- run_attempt(task, run_key = "stops")

  expect_match(
    error_text(err1),
    "the publish stops after batch 2",
    fixed = TRUE
  )
  # One batch per partition. One transaction holds the whole publish, so
  # the batches of p1 and p2 rolled back.
  expect_equal(probe$moved, c(4, 4))
  expect_equal(live_rows(out), old_rows())
  expect_equal(staged_count(out), 12)
  expect_true(is.na(store_rows("cs9_run", "stops")$published_datetime))
  expect_equal(nrow(store_rows("cs9_run_publish", "stops")), 0)

  probe2 <- local_batch_probe()
  err2 <- run_attempt(task, run_key = "stops")

  expect_equal(error_text(err2), "")
  expect_equal(live_rows(out), expected_rows(1:4))
  expect_equal(staged_count(out), 0)
  # The retry runs plan 1 and the last plan again, as before 26.10.24.
  expect_equal(stage_calls(ctl, 4), c(2L, 1L, 1L, 2L))
  expect_equal(probe2$moved, c(4, 4, 4))
})

# The staging key holds the live key, so no batch can hold one key twice, and
# no batch splits two rows with one key.
test_that("on SQLite a run that stages one key twice stops before the publish", {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  for (p in c("2", "3")) {
    ctl$hooks[[p]] <- function(tables, argset) {
      tables$out$insert_data(data.table::data.table(
        x = "dup",
        part = "p1",
        n = as.integer(argset$plan)
      ))
      return(invisible(NULL))
    }
  }
  probe <- local_batch_probe()

  err <- run_attempt(new_stage_task(out), run_key = "dup")

  expect_match(error_text(err), "two plans wrote the same row", fixed = TRUE)
  expect_length(probe$moved, 0)
  expect_equal(live_rows(out), old_rows())
})

# The database gives each staged row its cs9_stg_id, so the name is reserved.
test_that("data with a column cs9_stg_id stops before any row is staged", {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$rows <- function(plan) {
    rows <- stage_rows_of(plan)
    if (plan == 2) {
      rows[, cs9_stg_id := 1]
    }
    return(rows)
  }
  ctl$drop <- FALSE

  err <- run_attempt(new_stage_task(out), run_key = "reserved")

  expect_match(
    error_text(err),
    "has the column(s) cs9_stg_id. These names are reserved",
    fixed = TRUE
  )
  # Plan 1 staged its rows. Plan 2 stopped before it staged any.
  expect_equal(staged_count(out), 3)
  expect_equal(live_rows(out), old_rows())

  # The other staging columns are reserved too.
  ctl$rows <- function(plan) {
    rows <- stage_rows_of(plan)
    if (plan == 2) {
      rows[, cs9_task := "x"]
    }
    return(rows)
  }
  err <- run_attempt(new_stage_task(out), run_key = "reserved_task")
  expect_match(
    error_text(err),
    "has the column(s) cs9_task. These names are reserved",
    fixed = TRUE
  )
})

# A live column named rowid hides the SQLite rowid, so the publish on SQLite
# MUST NOT select rows by rowid.
test_that("on SQLite a live table with a column named rowid publishes every staged row", {
  db <- local_staging_db()
  out <- DBTableExtended_v9$new(
    dbconfig = db,
    table_name = "anon_stage_rowid",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER", rowid = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(out$disconnect())
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$rows <- function(plan) {
    rows <- stage_rows_of(plan)
    rows[, rowid := NA_integer_]
    return(rows)
  }
  probe <- local_batch_probe()

  err <- run_attempt(new_stage_task(out), run_key = "rowid")

  expect_equal(error_text(err), "")
  expect_equal(probe$moved, 12)
  expect_equal(live_rows(out), expected_rows(1:4))
  expect_equal(staged_count(out), 0)
})

# PostgreSQL ----

# The publish of the PostgreSQL tests, with the batch size 1000. It installs
# the audit triggers, and it saves the staged rows with their ids, just before
# each attempt publishes.
local_pg_publish <- function(pg, out, envir = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$staged <- list()
  testthat::local_mocked_bindings(
    run_publish_rows_per_batch = 1000L,
    .env = envir
  )
  publish <- real_publish
  testthat::local_mocked_bindings(
    run_publish = function(state, tables) {
      pg_audit_install(pg, out)
      seen$staged[[length(seen$staged) + 1L]] <- pg_staged_rows(pg)
      return(publish(state, tables))
    },
    .env = envir
  )
  return(seen)
}

# The ids of the staged rows of each partition, cut into the batches that the
# publish MUST send: the rows in id order, at most 1000 per batch.
pg_id_batches <- function(staged) {
  out <- list()
  for (p in sort(unique(staged$part))) {
    ids <- sort(staged[part == p]$id)
    for (chunk in split(ids, ceiling(seq_along(ids) / 1000))) {
      out[[length(out) + 1L]] <- list(part = p, id = unname(chunk))
    }
  }
  return(out)
}

test_that("on PostgreSQL each batch of at most the batch size is a transaction of its own", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_stage_ctl()
  seen <- local_pg_publish(pg, out)
  explain <- local_batch_explain(pg)
  before <- new.env(parent = emptyenv())
  before$locked <- logical()
  before$published <- numeric()
  probe <- local_batch_probe(before = function() {
    before$locked <- c(
      before$locked,
      pg_live_locks_held(pg, out)
    )
    before$published <- c(before$published, pg_published_n(pg, "batches"))
    return(invisible(NULL))
  })

  err <- run_attempt(new_stage_task(out, n_plans = 3), run_key = "batches")

  expect_equal(error_text(err), "")
  expect_equal(live_rows(out), pg_expected())
  expect_equal(pg_staged_n(pg), 0)
  tx <- pg_transactions(pg, out)
  # p1 holds 2501 staged rows, p2 301 and p3 10.
  expect_equal(probe$moved, c(1000, 1000, 501, 301, 10))
  # 10 committed transactions: the list of the changed tables, the operation
  # on each of the 3 partitions, 5 batches and the end.
  expect_equal(nrow(tx), 10)
  expect_equal(tx$staged[tx$staged > 0], c(1000, 1000, 501, 301, 10))
  expect_lte(max(tx$staged), 1000)
  expect_equal(tx$inserted, tx$staged)
  # Each batch is the next 1000 staged rows of its partition in id order.
  expect_equal(pg_batch_ids(pg), pg_id_batches(seen$staged[[1]]))
  # The operation runs once per partition, each in a transaction that moves
  # no staged row, before the first batch. It deletes the 4 old rows with
  # n <= 2: 2 in p1, 1 in p2 and 1 in p3. The transaction of p3 removes the
  # operation from cs9_run_operation.
  op <- tx[tx$live_deleted > 0 & tx$staged == 0, ]
  expect_equal(op$live_deleted, c(2, 1, 1))
  expect_equal(op$operations, c(0, 0, 1))
  expect_equal(sum(tx$operations), 1)
  expect_lt(max(op$txid), min(tx$txid[tx$staged > 0]))
  expect_equal(sum(tx$truncated), 0)
  # The run is marked as published once, in the last transaction.
  expect_equal(tx$published, c(rep(0, 9), 1))
  expect_equal(pg_published_n(pg, "batches"), 1)
  # Before each batch: the publish holds the lock of each partition, and the
  # run is not marked as published.
  expect_equal(before$locked, rep(TRUE, 5))
  expect_equal(before$published, rep(0, 5))
  # The SELECT that finds each batch reads the index, never the whole staging
  # table.
  expect_length(explain$plans, 5)
  for (plan in explain$plans) {
    expect_match(plan, "Index (Only )?Scan")
    expect_no_match(plan, "Seq Scan")
  }
})

test_that("on PostgreSQL a publish that stops after batch 2 resumes with the same run_key and runs its operation once", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_stage_ctl()
  task <- new_stage_task(out, n_plans = 3)
  seen <- local_pg_publish(pg, out)

  probe1 <- local_batch_probe(stop_after = 2)
  err1 <- run_attempt(task, run_key = "resume")

  expect_match(
    error_text(err1),
    "the publish stops after batch 2",
    fixed = TRUE
  )
  expect_equal(probe1$moved, c(1000, 1000))
  # The 2 batches committed: the 2000 staged rows of p1 with the lowest ids.
  first <- seen$staged[[1]][part == "p1"][1:2000, c("x", "part", "n")]
  partial <- rbind(
    pg_old_rows()[n > 2 & !x %in% first$x],
    first
  )
  data.table::setorderv(partial, "x")
  expect_equal(live_rows(out), partial)
  expect_equal(pg_staged_n(pg), 2812 - 2000)
  expect_equal(pg_published_n(pg, "resume"), 0)
  calls1 <- stage_calls(ctl, 3)
  expect_equal(calls1, c(1L, 1L, 1L))

  probe2 <- local_batch_probe()
  err2 <- run_attempt(task, run_key = "resume")

  expect_equal(error_text(err2), "")
  expect_equal(live_rows(out), pg_expected())
  expect_equal(pg_staged_n(pg), 0)
  expect_equal(pg_published_n(pg, "resume"), 1)
  # The retry runs no plan. It starts at the lowest id still staged, and
  # publishes the 501 rows left in p1, then p2 and p3.
  expect_equal(stage_calls(ctl, 3), calls1)
  expect_equal(probe2$moved, c(501, 301, 10))
  expect_equal(pg_batch_ids(pg), pg_id_batches(seen$staged[[1]]))
  tx <- pg_transactions(pg, out)
  expect_equal(tx$staged[tx$staged > 0], c(1000, 1000, 501, 301, 10))
  expect_equal(tx$inserted, tx$staged)
  # The operation ran in one transaction of attempt 1 and never again.
  expect_equal(sum(tx$operations > 0), 1)
  expect_equal(sum(tx$live_deleted[tx$staged == 0]), 4)
  expect_equal(tx$published[nrow(tx)], 1)
  expect_equal(sum(tx$published), 1)
})

test_that("on PostgreSQL a run that stages one key twice stops before the publish", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_stage_ctl()
  ctl$hooks[["2"]] <- function(tables, argset) {
    tables$out$insert_data(data.table::data.table(
      x = "dup",
      part = "p1",
      n = 2L
    ))
    return(invisible(NULL))
  }
  ctl$hooks[["3"]] <- function(tables, argset) {
    tables$out$insert_data(data.table::data.table(
      x = "dup",
      part = "p1",
      n = 3L
    ))
    return(invisible(NULL))
  }
  seen <- local_pg_publish(pg, out)
  probe <- local_batch_probe()

  err <- run_attempt(new_stage_task(out, n_plans = 3), run_key = "dup")

  # The staging key holds the live key, so no batch can hold one key twice.
  expect_match(error_text(err), "two plans wrote the same row", fixed = TRUE)
  expect_length(probe$moved, 0)
  live <- live_rows(out)
  data.table::setorderv(live, "x")
  old <- pg_old_rows()
  data.table::setorderv(old, "x")
  expect_equal(live, old)
})

test_that("on PostgreSQL a staging table without cs9_stg_id keeps its staged rows, which get ids and are published", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_stage_ctl()
  seen <- local_pg_publish(pg, out)
  # Plan 3 stages its 2 upserts and then fails. Plans 1 and 2 are done.
  ctl$fail <- 3L
  err1 <- run_attempt(new_stage_task(out, n_plans = 3), run_key = "upgrade")
  expect_match(error_text(err1), "plan 3 failed on purpose", fixed = TRUE)
  expect_equal(pg_staged_n(pg), 2812)
  # The staging table now gets the shape that cs9 3951f7f gave it: no
  # cs9_stg_id, and the index cs9_run on cs9_task, cs9_run_key and part.
  pg_make_old_staging(pg)
  expect_false("cs9_stg_id" %in% pg_staging_columns(pg))

  # A new table object, as in the R process of the next attempt.
  out2 <- new_stage_out(pg$dbconfig, "partitioned")
  ctl$fail <- integer()
  err2 <- run_attempt(new_stage_task(out2, n_plans = 3), run_key = "upgrade")

  expect_equal(error_text(err2), "")
  expect_true("cs9_stg_id" %in% pg_staging_columns(pg))
  expect_true(any(grepl("cs9_stg_id", pg_staging_indexes(pg))))
  # Plan 2 is skipped. Its 2800 rows were staged before the upgrade, kept
  # their place, got an id each, and were published.
  expect_equal(stage_calls(ctl, 3), c(2L, 1L, 2L))
  staged <- seen$staged[[length(seen$staged)]]
  plan2 <- staged[startsWith(x, "b-")]
  expect_equal(nrow(plan2), 2800)
  expect_false(anyNA(plan2$id))
  expect_equal(live_rows(out2), pg_expected())
  expect_equal(pg_staged_n(pg), 0)
})

test_that("on PostgreSQL a new run_key first finishes a partly published run of the same task", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_stage_ctl()
  task <- new_stage_task(out, n_plans = 3)
  seen <- local_pg_publish(pg, out)
  probe1 <- local_batch_probe(stop_after = 2)
  err1 <- run_attempt(task, run_key = "A")
  expect_match(
    error_text(err1),
    "the publish stops after batch 2",
    fixed = TRUE
  )

  # Run B records no operation and upserts one row.
  ctl$rows <- function(plan) NULL
  ctl$hooks <- list("3" = function(tables, argset) {
    tables$out$upsert_data(data.table::data.table(
      x = "keep-p1",
      part = "p1",
      n = 9L
    ))
    return(invisible(NULL))
  })
  probe2 <- local_batch_probe()
  err2 <- run_attempt(task, run_key = "B")

  expect_equal(error_text(err2), "")
  # The rest of run A, then the one row of run B.
  expect_equal(probe2$moved, c(501, 301, 10, 1))
  expected <- pg_expected()
  expected[x == "keep-p1", n := 9L]
  expect_equal(live_rows(out), expected)
  expect_equal(pg_published_n(pg, "A"), 1)
  expect_equal(pg_published_n(pg, "B"), 1)
  expect_equal(pg_staged_n(pg), 0)
})

test_that("on PostgreSQL a retry of a partly published run stops when the plan list changed", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_stage_ctl()
  ctl$plans <- function() 1:3
  task <- new_stage_task(out, planned = TRUE)
  seen <- local_pg_publish(pg, out)
  probe1 <- local_batch_probe(stop_after = 2)
  err1 <- run_attempt(task, run_key = "plans")
  expect_match(
    error_text(err1),
    "the publish stops after batch 2",
    fixed = TRUE
  )

  ctl$plans <- function() c(1L, 2L)
  probe2 <- local_batch_probe()
  err2 <- run_attempt(task, run_key = "plans")

  expect_match(
    error_text(err2),
    "the plan list changed between attempts of one run",
    fixed = TRUE
  )
  expect_length(probe2$moved, 0)
  expect_equal(pg_staged_n(pg), 2812 - 2000)
  expect_equal(pg_published_n(pg, "plans"), 0)
  # No plan ran on attempt 2.
  expect_equal(stage_calls(ctl, 3), c(1L, 1L, 1L))

  ctl$plans <- function() 1:3
  err3 <- run_attempt(task, run_key = "plans")

  expect_equal(error_text(err3), "")
  expect_equal(live_rows(out), pg_expected())
  expect_equal(stage_calls(ctl, 3), c(1L, 1L, 1L))
})

test_that("on PostgreSQL drop_all_rows() truncates each partition in a transaction of its own, before the first batch", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_drop_all_ctl()
  seen <- local_pg_publish(pg, out)
  probe <- local_batch_probe()

  err <- run_attempt(new_stage_task(out, n_plans = 3), run_key = "drop-all")

  expect_equal(error_text(err), "")
  expect_equal(live_rows(out), pg_expected_drop_all())
  tx <- pg_transactions(pg, out)
  tr <- pg_truncates(pg)
  parts <- vapply(out$tables, function(t) t$table_name, character(1))
  # One TRUNCATE per partition, each in its own transaction, in partition
  # order. None of them deletes a row one by one.
  expect_equal(tr$tab, unname(parts))
  expect_equal(length(unique(tr$txid)), 3)
  step2 <- tx[tx$txid %in% tr$txid, ]
  expect_equal(step2$truncated, c(1, 1, 1))
  expect_equal(step2$live_deleted, c(0, 0, 0))
  expect_equal(step2$staged, c(0, 0, 0))
  # The transaction of the last partition removes the operation.
  expect_equal(step2$operations, c(0, 0, 1))
  # Step 2 ends before the first batch of the table inserts a row.
  expect_lt(max(tr$txid), min(tx$txid[tx$inserted > 0]))
  expect_equal(probe$moved, c(1000, 1000, 501, 301, 10))
  expect_equal(nrow(tx), 10)
})

test_that("on PostgreSQL a publish that stops after partition 2 of the operation resumes to the same live rows", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_drop_all_ctl()
  task <- new_stage_task(out, n_plans = 3)
  seen <- local_pg_publish(pg, out)
  done <- new.env(parent = emptyenv())
  done$n <- 0L
  real_part_operations <- run_publish_part_operations
  testthat::local_mocked_bindings(
    run_publish_part_operations = function(...) {
      if (done$n >= 2L) {
        stop("cs9 test: the publish stops after partition 2 of the operation")
      }
      done$n <- done$n + 1L
      return(real_part_operations(...))
    }
  )
  probe1 <- local_batch_probe()

  err1 <- run_attempt(task, run_key = "op-stop")

  expect_match(
    error_text(err1),
    "the publish stops after partition 2 of the operation",
    fixed = TRUE
  )
  # p1 and p2 are empty, p3 keeps its old row, and no batch ran.
  expect_equal(live_rows(out), pg_old_rows()[part == "p3"])
  expect_length(probe1$moved, 0)
  expect_equal(pg_staged_n(pg), 2812)
  expect_equal(pg_published_n(pg, "op-stop"), 0)
  tx1 <- pg_transactions(pg, out)
  expect_equal(sum(tx1$truncated), 2)
  expect_equal(sum(tx1$operations), 0)

  done$n <- -Inf
  probe2 <- local_batch_probe()
  err2 <- run_attempt(task, run_key = "op-stop")

  expect_equal(error_text(err2), "")
  expect_equal(live_rows(out), pg_expected_drop_all())
  expect_equal(pg_staged_n(pg), 0)
  expect_equal(probe2$moved, c(1000, 1000, 501, 301, 10))
  # The retry runs the operation again on all 3 partitions, then the batches.
  tr <- pg_truncates(pg)
  parts <- unname(vapply(out$tables, function(t) t$table_name, character(1)))
  expect_equal(tr$tab, c(parts[1:2], parts))
  tx <- pg_transactions(pg, out)
  expect_lt(max(tr$txid), min(tx$txid[tx$inserted > 0]))
  expect_equal(sum(tx$operations), 1)
  expect_equal(sum(tx$published), 1)
})

# A task that stages the partitioned table and a task that stages its
# partition p1 as a plain table write the same live table. Both publishes lock
# that live table, so one waits for the other.
test_that("on PostgreSQL a publish locks each partition, so a task that stages one partition as a table waits", {
  skip_without_pg()
  pg <- local_pg_db()
  out <- new_stage_out(pg$dbconfig, "partitioned")
  pg_seed(pg, out)
  ctl <- local_pg_stage_ctl()
  seen <- local_pg_publish(pg, out)
  p1 <- out$tables[["p1"]]$table_name
  held_p <- logical()
  probe1 <- local_batch_probe(before = function() {
    held_p <<- c(held_p, pg_live_locks_held(pg, out))
    return(invisible(NULL))
  })
  err1 <- run_attempt(new_stage_task(out, n_plans = 3), run_key = "parent")
  expect_equal(error_text(err1), "")
  # Each batch ran while the run of the partitioned table held the lock of
  # every partition, p1 included.
  expect_equal(held_p, rep(TRUE, 5))

  # The second task stages the partition p1 as a plain table, with the fields
  # of the partition in their order, so csdb keeps the table.
  fields <- out$tables[["p1"]]$field_types
  child <- DBTableExtended_v9$new(
    dbconfig = pg$dbconfig,
    table_name = p1,
    field_types = fields[names(fields) != "auto_last_updated_datetime"],
    keys = c("x", "part"),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(child$disconnect())
  ctl$rows <- function(plan) {
    return(data.table::data.table(x = paste0("child-", plan), part = "p1", n = 3L))
  }
  ctl$hooks <- list()
  held_c <- logical()
  probe2 <- local_batch_probe(before = function() {
    held_c <<- c(held_c, pg_lock_is_held(pg$admin, paste("cs9 table", p1)))
    return(invisible(NULL))
  })
  task2 <- task_from_config_v9(
    name_grouping = "stage",
    name_action = "child",
    cores = 1,
    for_each_plan = plnr::expand_list(plan = 1:2),
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = list(out = child)
  )
  err2 <- run_attempt(task2, run_key = "child")

  expect_equal(error_text(err2), "")
  # The run of the partition holds the same lock key as the run of the
  # partitioned table.
  expect_equal(held_c, TRUE)
  expect_equal(probe2$moved, 2)
  expected <- rbind(
    pg_expected(),
    data.table::data.table(x = c("child-1", "child-2"), part = "p1", n = 3L)
  )
  data.table::setorderv(expected, "x")
  expect_equal(live_rows(out), expected)
})
