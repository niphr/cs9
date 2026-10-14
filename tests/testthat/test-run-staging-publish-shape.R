# The shape of the publish: the statements that it sends to the live tables,
# and the index of the staging table.
#
# On 2026-10-14 the publish of norsyss_data_import_consultations ran for over
# 45 minutes. The run recorded 705 operations on a table of 106 partitions, and
# the publish sent one DELETE per operation per partition. The publish MUST
# send a number of statements per live table that does not grow with the
# number of operations. These tests drive the real Task$run() on SQLite. The
# fixtures are in helper-run-staging.R.
#
# The count comes from a mock of DBI::dbExecute() in the DBI namespace. The
# publish sends every DELETE and INSERT through DBI::dbExecute(). The mock
# counts only while run_publish() runs, so the writes of the plans, of csdb and
# of the bookkeeping before the publish do not count.

shape_n_plans <- 24L
shape_parts <- paste0("anon_stage_part_xxpxx_", c("p1", "p2", "p3"))

# Six old rows, two per partition.
shape_old <- function() {
  return(data.table::data.table(
    x = paste0("old-", 1:6),
    part = rep(c("p1", "p2", "p3"), 2),
    n = 0L
  ))
}

# Writes the old rows with DBI, outside any task run.
shape_seed <- function(out) {
  rows <- shape_old()
  rows[, auto_last_updated_datetime := "2026-01-01 00:00:00"]
  con <- DBI::dbConnect(RSQLite::SQLite(), out$tables[[1]]$dbconfig$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (t in out$tables) {
    # tbl() creates the table at its first use.
    suppressMessages(invisible(t$tbl()))
    tag <- sub(".*_xxpxx_", "", t$table_name)
    DBI::dbAppendTable(con, t$table_name, as.data.frame(rows[part == tag]))
  }
  return(invisible(NULL))
}

# Records each statement that DBI::dbExecute() receives while `rec$on` is
# TRUE. run_publish() sets it for the time of the publish.
local_publish_recorder <- function(envir = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$on <- FALSE
  rec$sql <- character()
  execute <- DBI::dbExecute
  publish <- run_publish
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      if (rec$on) {
        rec$sql <- c(rec$sql, statement)
      }
      return(execute(conn, statement, ...))
    },
    .package = "DBI",
    .env = envir
  )
  testthat::local_mocked_bindings(
    run_publish = function(state, tables) {
      rec$on <- TRUE
      on.exit(rec$on <- FALSE, add = TRUE)
      return(publish(state, tables))
    },
    .env = envir
  )
  return(rec)
}

# The DELETE and INSERT statements whose target is one of `parts`, with the
# verb and the target without quotes.
live_writes <- function(sql, parts) {
  m <- regmatches(sql, regexec("^(DELETE FROM|INSERT INTO) ([^ ]+)", sql))
  verb <- vapply(
    m,
    function(x) if (length(x) == 3) x[2] else NA_character_,
    character(1)
  )
  target <- vapply(
    m,
    function(x) {
      if (length(x) == 3) gsub("[\"`]|\\[|\\]", "", x[3]) else NA_character_
    },
    character(1)
  )
  keep <- !is.na(target) & target %in% parts
  return(data.frame(verb = verb[keep], target = target[keep], sql = sql[keep]))
}

# One run of a task with 24 plans on a partitioned table of 3 partitions. Plan
# k records drop_rows_where(x = 'old-<k + 3>') and stages 3 rows, one per
# partition. The scenario "keep" adds keep_rows_where(x <> 'old-3') in plan 10.
# The scenario "empty" adds drop_all_rows() in plan 1. `run_key = NULL` writes
# the live table directly, as a run before 26.10.14 did.
shape_run <- function(scenario, run_key) {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  shape_seed(out)
  ctl <- local_stage_ctl()
  ctl$drop <- identical(scenario, "empty")
  hook <- function(tables, argset) {
    tables$out$drop_rows_where(paste0("x = 'old-", argset$plan + 3L, "'"))
    if (identical(scenario, "keep") && argset$plan == 10L) {
      tables$out$keep_rows_where("x <> 'old-3'")
    }
    return(invisible(NULL))
  }
  ctl$hooks <- stats::setNames(
    rep(list(hook), shape_n_plans),
    seq_len(shape_n_plans)
  )
  rec <- local_publish_recorder()
  err <- run_attempt(
    new_stage_task(out, n_plans = shape_n_plans),
    run_key = run_key
  )
  return(list(
    error = err,
    live = live_rows(out),
    writes = live_writes(rec$sql, shape_parts)
  ))
}

# The rows of shape_old() named in `x`, and the new rows of every plan.
shape_expected <- function(x) {
  old <- shape_old()[x, on = "x"]
  d <- rbind(old, expected_rows(seq_len(shape_n_plans)))
  data.table::setorderv(d, "x")
  return(d)
}

test_that("a keyed publish sends at most 3 statements per partition, whatever the number of operations", {
  got <- shape_run("drop", run_key = "shape")

  expect_null(got$error)
  per_part <- table(factor(got$writes$target, levels = shape_parts))
  # Before 26.10.18 each partition got 24 deletes and 1 insert: 25 statements.
  expect_lte(max(per_part), 3)
  expect_gte(min(per_part), 1)
  expect_equal(got$live, shape_expected(paste0("old-", 1:3)))
})

test_that("a keyed publish gives the live rows of an unstaged run, with delete, keep and empty operations", {
  for (scenario in c("drop", "keep", "empty")) {
    staged <- shape_run(scenario, run_key = paste0("shape-", scenario))
    direct <- shape_run(scenario, run_key = NULL)

    expect_null(staged$error)
    expect_null(direct$error)
    expect_equal(staged$live, direct$live, label = paste("staged", scenario))
  }
  expect_equal(direct$live, shape_expected(character()))
})

test_that("a keyed publish with a keep operation keeps the rows that the keep keeps", {
  got <- shape_run("keep", run_key = "shape-keep")

  expect_null(got$error)
  expect_equal(got$live, shape_expected(c("old-1", "old-2")))
})

test_that("a keyed publish with an empty operation empties each partition with one plain DELETE", {
  got <- shape_run("empty", run_key = "shape-empty")

  expect_null(got$error)
  # RSQLite quotes a name with backticks, so the quotes go before the compare.
  deletes <- gsub("[`\"]", "", got$writes$sql[got$writes$verb == "DELETE FROM"])
  expect_setequal(deletes, paste0("DELETE FROM ", shape_parts))
  expect_length(deletes, length(shape_parts))
})

test_that("the publish splits a long OR list into deletes of a fixed size", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  n_rows <- 1200L
  n_ops <- 1001L
  con <- DBI::dbConnect(RSQLite::SQLite(), db$db)
  withr::defer(DBI::dbDisconnect(con))
  suppressMessages(invisible(out$tbl()))
  DBI::dbAppendTable(
    con,
    out$table_name,
    data.frame(
      x = paste0("r", seq_len(n_rows)),
      part = "p1",
      n = 0L,
      auto_last_updated_datetime = "2026-01-01 00:00:00"
    )
  )
  ops <- data.frame(
    operation = "delete",
    sql_condition = paste0("x = 'r", seq_len(n_ops), "'")
  )
  rec <- local_publish_recorder()
  rec$on <- TRUE

  run_publish_part(
    con,
    state = NULL,
    live = list(table = out, part = NULL),
    staging = NULL,
    part_col = NULL,
    ops = ops,
    rows = data.frame(part = character(), upsert = integer(), n = numeric())
  )

  rec$on <- FALSE
  writes <- live_writes(rec$sql, out$table_name)
  expect_equal(
    nrow(writes),
    ceiling(n_ops / run_publish_operations_per_delete)
  )
  expect_gt(nrow(writes), 1)
  left <- DBI::dbGetQuery(con, paste0("SELECT x FROM ", out$table_name))$x
  expect_setequal(left, paste0("r", (n_ops + 1L):n_rows))
})

# The columns of each index on a SQLite table that CREATE INDEX made, in index
# order. The primary key is not in the list.
created_indexes <- function(name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), config$dbconfigs$anon$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  quoted <- DBI::dbQuoteIdentifier(con, name)
  il <- DBI::dbGetQuery(con, paste0("PRAGMA index_list(", quoted, ")"))
  out <- list()
  for (i in il$name[il$origin == "c"]) {
    info <- DBI::dbGetQuery(
      con,
      paste0("PRAGMA index_info(", DBI::dbQuoteIdentifier(con, i), ")")
    )
    out[[i]] <- info$name[order(info$seqno)]
  }
  return(out)
}

# The leading columns of each created index, joined by commas.
leading_columns <- function(indexes, n) {
  return(vapply(
    indexes,
    function(cols) paste(utils::head(cols, n), collapse = ","),
    character(1)
  ))
}

for (kind in c("plain", "partitioned")) {
  test_that(
    paste0("the staging table has an index on task and run key (", kind, ")"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, kind)
      seed_old_rows(out)
      ctl <- local_stage_ctl()

      err <- run_attempt(new_stage_task(out), run_key = "index")

      expect_null(err)
      lead <- if (kind == "plain") {
        "cs9_task,cs9_run_key"
      } else {
        "cs9_task,cs9_run_key,part"
      }
      name <- if (kind == "plain") "anon_stage_plain" else "anon_stage_part"
      got <- leading_columns(
        created_indexes(paste0(name, "_cs9stg")),
        lengths(strsplit(lead, ","))
      )
      expect_contains(got, lead)
    }
  )
}

# Creates the staging table of `out` as cs9 26.10.17 created it, without an
# index. Production holds such a table.
create_index_less_staging <- function(out, db) {
  stg <- out$.__enclos_env__$private$staging()
  old <- csdb::DBTable_v9$new(
    dbconfig = db,
    table_name = stg$table_name,
    field_types = stg$field_types,
    keys = stg$keys,
    indexes = NULL,
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  suppressMessages(old$create_table())
  old$disconnect()
  return(invisible(NULL))
}

test_that("an existing staging table without the index gets it at the next keyed run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  create_index_less_staging(out, db)
  expect_length(created_indexes("anon_stage_part_cs9stg"), 0)

  err <- run_attempt(new_stage_task(out), run_key = "existing")

  expect_null(err)
  got <- leading_columns(created_indexes("anon_stage_part_cs9stg"), 3)
  expect_contains(got, "cs9_task,cs9_run_key,part")
  expect_equal(live_rows(out), expected_rows(1:4))
})

# A run that only records operations stages no row. The fence and the clear
# still read the staging table, so the index MUST exist when the run begins.
test_that("a keyed run that only records operations gives an existing staging table its index", {
  db <- local_staging_db()
  out <- new_stage_out(db, "partitioned")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$rows <- function(plan) NULL
  ctl$hooks <- list("2" = function(tables, argset) {
    tables$out$drop_rows_where("x = 'old-2'")
    return(invisible(NULL))
  })
  create_index_less_staging(out, db)
  expect_length(created_indexes("anon_stage_part_cs9stg"), 0)

  err <- run_attempt(new_stage_task(out), run_key = "operations-only")

  expect_null(err)
  got <- leading_columns(created_indexes("anon_stage_part_cs9stg"), 3)
  expect_contains(got, "cs9_task,cs9_run_key,part")
  expect_equal(live_rows(out), old_rows()[x != "old-2"])
})
