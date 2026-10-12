# Fixtures for the tests of staged task runs: test-run-staging-*.R.
#
# A task run stages every write to its own tables and publishes them in one
# transaction after the last plan. These fixtures build a task whose action
# writes three rows per plan, one per partition, to `tables$out`. `out` is a
# plain table or a partitioned table with the partitions p1, p2 and p3. The
# table holds three old rows before each run.
#
# testthat sources a helper-*.R file before every test file, so the test files
# share these fixtures.

# A SQLite dbconfig for the "anon" access, plus the configuration tables.
# `.local_envir = envir` ties the cleanup to the caller's frame.
local_staging_db <- function(envir = parent.frame()) {
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
  withr::defer(
    for (tab in config$tables) {
      tab$disconnect()
    },
    envir = envir
  )
  return(config$dbconfigs$anon)
}

# The output table. Plain: key x. Partitioned: key x and part.
new_stage_out <- function(dbconfig, kind = "plain", envir = parent.frame()) {
  fields <- c(x = "TEXT", part = "TEXT", n = "INTEGER")
  if (kind == "plain") {
    out <- DBTableExtended_v9$new(
      dbconfig = dbconfig,
      table_name = "anon_stage_plain",
      field_types = fields,
      keys = "x",
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
    )
  } else {
    out <- DBPartitionedTableExtended_v9$new(
      dbconfig = dbconfig,
      table_name_base = "anon_stage_part",
      table_name_partitions = c("p1", "p2", "p3"),
      column_name_partition = "part",
      field_types = fields,
      keys = c("x", "part"),
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
    )
  }
  withr::defer(out$disconnect(), envir = envir)
  return(out)
}

stage_rows_of <- function(plan, n = plan) {
  return(data.table::data.table(
    x = paste0("new-", plan, "-", 1:3),
    part = c("p1", "p2", "p3"),
    n = as.integer(n)
  ))
}

old_rows <- function() {
  return(data.table::data.table(
    x = paste0("old-", 1:3),
    part = c("p1", "p2", "p3"),
    n = 0L
  ))
}

# Writes the old rows, outside any task run.
# Writes the old rows with DBI, outside any task run. Not through
# insert_data(), so a fixture never depends on the write path under test.
seed_old_rows <- function(out) {
  if (inherits(out, "DBPartitionedTableExtended_v9")) {
    live <- out$tables
    db <- out$tables[[1]]$dbconfig$db
  } else {
    live <- list(out)
    db <- out$dbconfig$db
  }
  rows <- old_rows()
  rows[, auto_last_updated_datetime := "2026-01-01 00:00:00"]
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (t in live) {
    # tbl() creates the table at its first use.
    suppressMessages(invisible(t$tbl()))
    part <- if (length(live) > 1) rows[part == sub(".*_xxpxx_", "", t$table_name)] else rows
    DBI::dbAppendTable(con, t$table_name, as.data.frame(part))
  }
  return(invisible(NULL))
}

# Every live row, ordered by x, with the columns x, part and n.
live_rows <- function(out) {
  if (inherits(out, "DBPartitionedTableExtended_v9")) {
    d <- data.table::rbindlist(lapply(out$tables, function(t) {
      return(data.table::as.data.table(dplyr::collect(t$tbl())))
    }))
  } else {
    d <- data.table::as.data.table(dplyr::collect(out$tbl()))
  }
  d <- d[, c("x", "part", "n")]
  data.table::setorderv(d, "x")
  return(d)
}

expected_rows <- function(plans, n = NULL) {
  d <- data.table::rbindlist(lapply(plans, function(p) {
    return(stage_rows_of(p, if (is.null(n)) p else n))
  }))
  data.table::setorderv(d, "x")
  return(d)
}

# The rows in the staging table of `out`, read from the SQLite file. 0 when the
# staging table does not exist.
staged_count <- function(out) {
  base <- if (inherits(out, "DBPartitionedTableExtended_v9")) {
    out$table_name_base
  } else {
    out$table_name
  }
  if (is.null(base)) {
    base <- "anon_stage_part"
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), config$dbconfigs$anon$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  name <- paste0(base, "_cs9stg")
  if (!DBI::dbExistsTable(con, name)) {
    return(0)
  }
  return(DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", name))$n)
}

# The rows of one run in a bookkeeping table, read from the SQLite file. No
# row when the table does not exist.
store_rows <- function(name, run_key) {
  con <- DBI::dbConnect(RSQLite::SQLite(), config$dbconfigs$anon$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, name)) {
    return(data.frame())
  }
  return(DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", name, " WHERE run_key = ?"),
    params = list(run_key)
  ))
}

# The controls of the action: which plan fails, which plan runs a hook, and a
# directory where each call leaves one line, also from a forked worker.
local_stage_ctl <- function(envir = parent.frame()) {
  ctl <- new.env(parent = emptyenv())
  ctl$dir <- withr::local_tempdir(.local_envir = envir)
  ctl$fail <- integer()
  ctl$drop <- TRUE
  ctl$hooks <- list()
  ctl$rows <- stage_rows_of
  ctl$plans <- NULL
  ctl$return_rows <- FALSE
  assign("cs9stage_ctl", ctl, envir = globalenv())
  fns <- list(
    cs9stage_data = function(argset, tables) {
      return(list(seed = 1L))
    },
    cs9stage_action = function(data, argset, tables) {
      ctl <- get("cs9stage_ctl", envir = globalenv())
      # One file per call. Forked workers that append to one shared file can
      # interleave their writes, which garbled a line in round 2 of phase 21.
      file.create(file.path(
        ctl$dir,
        paste0("call-", argset$plan, "-", Sys.getpid(), "-", basename(tempfile("")))
      ))
      if (isTRUE(argset$first_analysis) && isTRUE(ctl$drop)) {
        tables$out$drop_all_rows()
      }
      hook <- ctl$hooks[[as.character(argset$plan)]]
      if (!is.null(hook)) {
        hook(tables, argset)
      }
      rows <- ctl$rows(argset$plan)
      # With return_rows, cs9 writes the rows at the end of the plan, through
      # insert_at_end_of_each_plan or upsert_at_end_of_each_plan.
      if (isTRUE(ctl$return_rows)) {
        return(rows)
      }
      if (!is.null(rows)) {
        tables$out$insert_data(rows)
      }
      if (argset$plan %in% ctl$fail) {
        stop("plan ", argset$plan, " failed on purpose")
      }
      return(invisible(NULL))
    },
    cs9stage_plan_analysis = function(argset, tables) {
      ctl <- get("cs9stage_ctl", envir = globalenv())
      return(list(
        for_each_plan = plnr::expand_list(plan = ctl$plans()),
        for_each_analysis = NULL
      ))
    }
  )
  for (n in names(fns)) {
    assign(n, fns[[n]], envir = globalenv())
  }
  withr::defer(
    rm(list = c(names(fns), "cs9stage_ctl"), envir = globalenv()),
    envir = envir
  )
  return(ctl)
}

# How many times each plan ran, as a vector over plans 1 to n.
stage_calls <- function(ctl, n_plans) {
  files <- list.files(ctl$dir, pattern = "^call-")
  got <- as.integer(vapply(strsplit(files, "-"), function(x) x[2], character(1)))
  return(vapply(seq_len(n_plans), function(i) sum(got == i), integer(1)))
}

# A task from add_task(): fixed plans 1 to n_plans, or plans from
# cs9stage_plan_analysis when `planned` is TRUE.
new_stage_task <- function(
  out,
  n_plans = 4,
  cores = 1,
  planned = FALSE,
  tables = list(out = out),
  insert_at_end = FALSE,
  upsert_at_end = FALSE
) {
  return(task_from_config_v9(
    name_grouping = "stage",
    name_action = if (planned) "planned" else "fixed",
    cores = cores,
    plan_analysis_fn_name = if (planned) "cs9stage_plan_analysis" else NULL,
    for_each_plan = if (planned) {
      NULL
    } else {
      plnr::expand_list(plan = seq_len(n_plans))
    },
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = tables,
    insert_at_end_of_each_plan = insert_at_end,
    upsert_at_end_of_each_plan = upsert_at_end
  ))
}

# One attempt of a run, as SurveillanceSystem_v9$run_task() makes it. Returns
# the error, or NULL. Before 26.10.14 Task$run() had no run_key, and every
# attempt was a fresh run; the call then leaves the key out.
run_attempt <- function(task, run_key = NULL, cores = 1) {
  if ("run_key" %in% names(formals(task$run))) {
    attempt <- function() {
      return(task$run(cores = cores, run_key = run_key, replan = TRUE))
    }
  } else {
    attempt <- function() {
      task$update_plans(replan = TRUE)
      return(task$run(cores = cores))
    }
  }
  return(tryCatch(
    {
      suppressWarnings(suppressMessages(attempt()))
      NULL
    },
    error = function(e) e
  ))
}

# The message of an error, or "" for no error. An expectation on the message
# then fails, rather than errors, when the run did not stop.
error_text <- function(err) {
  if (is.null(err)) {
    return("")
  }
  return(conditionMessage(err))
}

# Records every call of update_config_tables_last_updated() in this process.
local_last_updated_recorder <- function(envir = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(
    update_config_tables_last_updated = function(table_name, ...) {
      rec$calls[[length(rec$calls) + 1L]] <- table_name
      return(invisible(NULL))
    },
    .env = envir
  )
  return(rec)
}

# Saves what a plan reads from the live table, from any process: nrow() in
# seen-<plan>, and the sorted x of every row in seenx-<plan>.
record_live_nrow <- function(ctl) {
  return(function(tables, argset) {
    cat(
      tables$out$nrow(),
      "\n",
      sep = "",
      file = file.path(ctl$dir, paste0("seen-", argset$plan)),
      append = TRUE
    )
    writeLines(
      live_rows(tables$out)$x,
      file.path(ctl$dir, paste0("seenx-", argset$plan))
    )
    return(invisible(NULL))
  })
}

seen_nrow <- function(ctl, plan) {
  f <- file.path(ctl$dir, paste0("seen-", plan))
  if (!file.exists(f)) {
    return(NA_real_)
  }
  return(as.numeric(readLines(f)))
}

seen_x <- function(ctl, plan) {
  f <- file.path(ctl$dir, paste0("seenx-", plan))
  if (!file.exists(f)) {
    return(NA_character_)
  }
  return(readLines(f))
}

# The cs9_run* and *_cs9stg tables in the anon and config SQLite files.
made_tables <- function() {
  made <- character()
  for (f in c(config$dbconfigs$anon$db, config$dbconfigs$config$db)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), f)
    made <- c(made, DBI::dbListTables(con))
    DBI::dbDisconnect(con)
  }
  return(grep("^cs9_run|_cs9stg$", made, value = TRUE))
}
