# The start of a task run, the resume of an earlier attempt, and the publish.
# See R/run_state.R for the design.

# Starts the run of a task, before Task$run() builds the plans. Returns the run
# state, or NULL when the run stages nothing. The run then writes its tables
# directly, as it did before 26.10.14, and touches no bookkeeping table.
#
# A run stages only when it has a run_key. A scheduler passes one. An
# interactive run, a vignette and a run of a task that a scheduler does not yet
# start with a key pass NULL. NULL MUST NOT become a fresh key: norsyss.cs9
# reaches production with each cs9 release, and two of its tasks lose rows
# under staging until they are rewritten.
#
# A task without a table of its own also stages nothing.
#
# On PostgreSQL the run holds one connection for the whole run. It holds the
# lock of the task on that connection, so a second run of the same task cannot
# start until this one ends. PostgreSQL releases the lock when the connection
# closes, also when the process dies. On SQLite there is no lock.
run_begin <- function(task, tables, run_key) {
  if (is.null(run_key)) {
    return(NULL)
  }
  run_key <- run_check_key(run_key)
  found <- run_targets(tables)
  if (!is.null(found$problem)) {
    warning(
      "cs9: task ",
      task,
      " writes its tables directly, without staging, ",
      "because ",
      found$problem,
      ".",
      call. = FALSE
    )
  }
  if (length(found$targets) == 0) {
    return(NULL)
  }
  dbconfig <- found$dbconfig
  sqlite <- identical(toupper(dbconfig$driver), "SQLITE")
  state <- list(
    task = task,
    run_key = run_key,
    today = NULL,
    plan_id = NULL,
    first_plan = NULL,
    staged = names(found$targets),
    children = run_children_names(found$targets),
    pid = Sys.getpid(),
    conn = run_new_connection(dbconfig),
    schema = if (is.null(dbconfig$schema)) "" else dbconfig$schema,
    sqlite = sqlite,
    published = FALSE
  )
  # Task$run() closes the connection when this function returns. Until then,
  # an error here closes it, so a run that cannot get the lock leaks none.
  started <- FALSE
  on.exit(
    if (!started) {
      state$conn$disconnect()
    },
    add = TRUE
  )
  con <- state$conn$autoconnection
  if (!sqlite) {
    run_lock_task(con, task)
  }
  run_store_create(con, state)
  run_fence(con, state, found$targets)
  row <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT run_date, published_datetime FROM ",
      run_store_ident(con, "cs9_run", state),
      " WHERE ",
      run_where_store(con, state)
    )
  )
  if (nrow(row) == 0) {
    state$today <- run_clock_today()
    DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO ",
        run_store_ident(con, "cs9_run", state),
        " (task, run_key, run_date) VALUES (",
        paste(
          run_q(con, c(task, run_key, as.character(state$today))),
          collapse = ", "
        ),
        ")"
      )
    )
  } else {
    state$today <- as.Date(row$run_date[1])
    state$published <- !is.na(row$published_datetime[1])
  }
  started <- TRUE
  return(state)
}

run_new_connection <- function(dbconfig) {
  return(csdb::DBConnection_v9$new(
    driver = dbconfig$driver,
    server = dbconfig$server,
    port = dbconfig$port,
    db = dbconfig$db,
    schema = dbconfig$schema,
    user = dbconfig$user,
    password = dbconfig$password,
    trusted_connection = dbconfig$trusted_connection,
    sslmode = dbconfig$sslmode,
    role_create_table = dbconfig$role_create_table
  ))
}

# A second run of the task stops at once. It does not wait, because the first
# run can take hours. The scheduler retries later.
run_lock_task <- function(con, task) {
  got <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT CASE WHEN pg_try_advisory_lock(hashtext(",
      run_q(con, paste("cs9 task", task)),
      ")) THEN 1 ELSE 0 END AS got"
    )
  )$got
  # psqlODBC can return a boolean as the text "1", so the query returns 1 or 0.
  if (!identical(as.integer(got), 1L)) {
    stop(
      "cs9: another run of task ",
      task,
      " holds its lock. That run is ",
      "still running or publishing. Run the task again when it ends.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Removes what other runs of this task left behind: staged rows, operations,
# done marks and run rows. Only a run that holds the lock of the task reaches
# this point, so no other run of the task is alive.
run_fence <- function(con, state, targets) {
  other <- paste0(
    "task = ",
    run_q(con, state$task),
    " AND run_key <> ",
    run_q(con, state$run_key)
  )
  for (name in names(run_store_definitions)) {
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM ",
        run_store_ident(con, name, state),
        " WHERE ",
        other
      )
    )
  }
  for (x in targets) {
    staging <- x$.__enclos_env__$private$staging()
    if (run_staging_exists(con, staging)) {
      DBI::dbExecute(
        con,
        paste0(
          "DELETE FROM ",
          run_table_ident(con, staging),
          " WHERE ",
          run_qi(con, "cs9_task"),
          " = ",
          run_q(con, state$task),
          " AND ",
          run_qi(con, "cs9_run_key"),
          " <> ",
          run_q(con, state$run_key)
        )
      )
    }
  }
  return(invisible(NULL))
}

run_staging_exists <- function(con, staging) {
  return(DBI::dbExistsTable(
    con,
    staging$table_name_short_for_mssql_fully_specified_for_postgres
  ))
}

# Ends a run: clears the state and closes the run connection, which releases
# the lock of the task.
run_end <- function(state) {
  config$run <- NULL
  config$planning <- FALSE
  if (!is.null(state)) {
    tryCatch(state$conn$disconnect(), error = function(e) invisible(NULL))
  }
  return(invisible(NULL))
}

# Prepares the plans of an attempt, after Task$run() built them. Returns a
# logical vector: TRUE for each plan that this attempt runs.
#
# A plan that an earlier attempt of the run marked as done is skipped. Plan 1
# runs on every attempt, because it can set state that later plans read. Each
# plan that runs first loses the rows and operations that it staged before.
run_prepare <- function(state, ids, targets) {
  con <- state$conn$autoconnection
  seen <- run_seen_plans(con, state, targets)
  gone <- setdiff(seen, c(ids, run_before_plans, run_after_plans))
  if (length(gone) > 0) {
    stop(
      "cs9: the plan list changed between attempts of one run; a planner read ",
      "something other than its settings and published tables. Task ",
      state$task,
      ", run '",
      state$run_key,
      "': ",
      length(gone),
      " plan(s) of an earlier attempt are not in the plan list now. ",
      "Start a new run with a new run_key.",
      call. = FALSE
    )
  }
  done <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT plan_id FROM ",
      run_store_ident(con, "cs9_run_plan_done", state),
      " WHERE ",
      run_where_store(con, state)
    )
  )$plan_id
  keep <- setdiff(intersect(done, ids), ids[1])
  run_clear_plans(con, state, targets, keep)
  return(!ids %in% keep)
}

# The plan ids that an earlier attempt of this run left anywhere.
run_seen_plans <- function(con, state, targets) {
  sql <- c(
    paste0(
      "SELECT DISTINCT plan_id AS p FROM ",
      run_store_ident(con, "cs9_run_plan_done", state),
      " WHERE ",
      run_where_store(con, state)
    ),
    paste0(
      "SELECT DISTINCT plan_id AS p FROM ",
      run_store_ident(con, "cs9_run_operation", state),
      " WHERE ",
      run_where_store(con, state)
    )
  )
  for (x in targets) {
    staging <- x$.__enclos_env__$private$staging()
    if (run_staging_exists(con, staging)) {
      sql <- c(
        sql,
        paste0(
          "SELECT DISTINCT ",
          run_qi(con, "cs9_plan"),
          " AS p FROM ",
          run_table_ident(con, staging),
          " WHERE ",
          run_where_run(con, state)
        )
      )
    }
  }
  out <- character()
  for (s in sql) {
    out <- c(out, DBI::dbGetQuery(con, s)$p)
  }
  return(unique(out))
}

# Removes every row of this run that belongs to a plan outside `keep`.
run_clear_plans <- function(con, state, targets, keep) {
  not_kept <- function(column) {
    if (length(keep) == 0) {
      return("1 = 1")
    }
    return(paste0(
      column,
      " NOT IN (",
      paste(run_q(con, keep), collapse = ", "),
      ")"
    ))
  }
  for (name in c("cs9_run_plan_done", "cs9_run_operation")) {
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM ",
        run_store_ident(con, name, state),
        " WHERE ",
        run_where_store(con, state),
        " AND ",
        not_kept("plan_id")
      )
    )
  }
  for (x in targets) {
    staging <- x$.__enclos_env__$private$staging()
    if (run_staging_exists(con, staging)) {
      DBI::dbExecute(
        con,
        paste0(
          "DELETE FROM ",
          run_table_ident(con, staging),
          " WHERE ",
          run_where_run(con, state),
          " AND ",
          not_kept(run_qi(con, "cs9_plan"))
        )
      )
    }
  }
  return(invisible(NULL))
}

# The publish ----

# Publishes the run in ONE transaction on the run connection: it runs the
# recorded operations on the live tables, inserts the staged rows, clears the
# staged rows, operations and done marks of the run, and marks the run as
# published. A failure rolls everything back, so the live tables keep their
# rows. The statements are plain SQL: no psql and no csdb load.
#
# config_tables_last_updated gets one row per changed live table. When it is in
# the run database, the publish transaction writes it too. Otherwise, as on
# SQLite, where each access is its own file, csdb writes it after the commit,
# and a failure there is a warning.
run_publish <- function(state, tables) {
  targets <- run_targets(tables)$targets
  con <- state$conn$autoconnection
  same_db <- run_config_in_run_database(con, state)
  changed <- character()
  DBI::dbWithTransaction(con, {
    if (!state$sqlite) {
      for (name in sort(names(targets))) {
        # pg_advisory_xact_lock() returns void, which odbc cannot read, so
        # the query selects a constant from it.
        DBI::dbGetQuery(
          con,
          paste0(
            "SELECT 1 AS got FROM pg_advisory_xact_lock(hashtext(",
            run_q(con, paste("cs9 table", name)),
            "))"
          )
        )
      }
    }
    for (name in names(targets)) {
      changed <- c(
        changed,
        run_publish_table(con, state, name, targets[[name]])
      )
    }
    run_clear_plans(con, state, targets, keep = character())
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE ",
        run_store_ident(con, "cs9_run", state),
        " SET published_datetime = ",
        run_q(con, cstime::now_c()),
        " WHERE ",
        run_where_store(con, state)
      )
    )
    if (same_db && length(changed) > 0) {
      run_last_updated_sql(con, changed)
    }
  })
  if (!same_db && length(changed) > 0) {
    tryCatch(
      update_config_tables_last_updated(table_name = changed),
      error = function(e) {
        warning(
          "cs9: the publish of task ",
          state$task,
          " committed, but recording ",
          "it in config_tables_last_updated failed: ",
          conditionMessage(e),
          call. = FALSE
        )
        return(invisible(NULL))
      }
    )
  }
  return(invisible(changed))
}

# Is config_tables_last_updated in the run database, and does it exist?
run_config_in_run_database <- function(con, state) {
  tab <- config$tables$config_tables_last_updated
  if (is.null(tab) || is.null(config$dbconfigs$config)) {
    return(FALSE)
  }
  if (
    run_database_id(config$dbconfigs$config) !=
      run_database_id(state$conn$config)
  ) {
    return(FALSE)
  }
  return(DBI::dbExistsTable(
    con,
    tab$table_name_short_for_mssql_fully_specified_for_postgres
  ))
}

run_last_updated_sql <- function(con, table_names) {
  tab <- run_table_ident(con, config$tables$config_tables_last_updated)
  table_names <- unique(table_names)
  DBI::dbExecute(
    con,
    paste0(
      "DELETE FROM ",
      tab,
      " WHERE table_name IN (",
      paste(run_q(con, table_names), collapse = ", "),
      ")"
    )
  )
  now <- cstime::now_c()
  rows <- paste0(
    "(",
    run_q(con, table_names),
    ", ",
    run_q(con, substr(now, 1, 10)),
    ", ",
    run_q(con, now),
    ")"
  )
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      tab,
      " (table_name, date, datetime) VALUES ",
      paste(rows, collapse = ", ")
    )
  )
  return(invisible(NULL))
}

# The live tables of one staged table: one for a plain table, one per partition
# for a partitioned table. `part` is the partition tag, or NULL.
run_live_parts <- function(x) {
  if (inherits(x, "DBPartitionedTableExtended_v9")) {
    return(lapply(names(x$tables), function(p) {
      return(list(table = x$tables[[p]], part = p))
    }))
  }
  return(list(list(table = x, part = NULL)))
}

# Publishes one staged table inside the transaction. Returns the names of the
# live tables that changed.
run_publish_table <- function(con, state, name, x) {
  staging <- x$.__enclos_env__$private$staging()
  part_col <- if (inherits(x, "DBPartitionedTableExtended_v9")) {
    x$column_name_partition
  } else {
    NULL
  }
  ops <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT DISTINCT operation, sql_condition FROM ",
      run_store_ident(con, "cs9_run_operation", state),
      " WHERE ",
      run_where_store(con, state),
      " AND table_name = ",
      run_q(con, name)
    )
  )
  staged <- run_staged_summary(con, state, staging, part_col)
  if (nrow(ops) == 0 && nrow(staged) == 0) {
    return(character())
  }
  changed <- character()
  for (live in run_live_parts(x)) {
    rows <- if (is.null(live$part)) {
      staged
    } else {
      staged[staged$part == live$part, , drop = FALSE]
    }
    if (nrow(ops) == 0 && nrow(rows) == 0) {
      next
    }
    run_publish_part(con, state, live, staging, part_col, ops, rows)
    changed <- c(changed, live$table$table_name)
  }
  return(changed)
}

# The staged rows of this run in one staging table: one row per partition and
# cs9_upsert, with the count.
run_staged_summary <- function(con, state, staging, part_col) {
  if (!run_staging_exists(con, staging)) {
    return(data.frame(part = character(), upsert = integer(), n = numeric()))
  }
  # PostgreSQL refuses a constant in GROUP BY, so a plain table groups by
  # cs9_upsert alone.
  group <- paste(run_qi(con, c(part_col, "cs9_upsert")), collapse = ", ")
  out <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ",
      group,
      ", COUNT(*) AS n FROM ",
      run_table_ident(con, staging),
      " WHERE ",
      run_where_run(con, state),
      " GROUP BY ",
      group
    )
  )
  part <- rep(NA_character_, nrow(out))
  if (!is.null(part_col)) {
    part <- as.character(out[[part_col]])
  }
  return(data.frame(
    part = part,
    upsert = as.numeric(out$cs9_upsert),
    n = as.numeric(out$n)
  ))
}

# Publishes one live table: its operations, then the delete of the live rows
# that a staged upsert replaces, then the insert of every staged row.
run_publish_part <- function(con, state, live, staging, part_col, ops, rows) {
  target <- run_table_ident(con, live$table)
  for (i in seq_len(nrow(ops))) {
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM ",
        target,
        " WHERE ",
        run_operation_predicate(ops$operation[i], ops$sql_condition[i])
      )
    )
  }
  if (nrow(rows) == 0) {
    return(invisible(NULL))
  }
  where <- run_where_run(con, state, prefix = "stg.")
  if (!is.null(part_col)) {
    where <- paste0(
      where,
      " AND stg.",
      run_qi(con, part_col),
      " = ",
      run_q(con, live$part)
    )
  }
  if (any(as.numeric(rows$upsert) == 1)) {
    bare <- run_qi(con, live$table$table_name)
    match <- paste0(
      "stg.",
      run_qi(con, live$table$keys),
      " = ",
      bare,
      ".",
      run_qi(con, live$table$keys),
      collapse = " AND "
    )
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM ",
        target,
        " WHERE EXISTS (SELECT 1 FROM ",
        run_table_ident(con, staging),
        " AS stg WHERE ",
        where,
        " AND stg.",
        run_qi(con, "cs9_upsert"),
        " = 1 AND ",
        match,
        ")"
      )
    )
  }
  columns <- run_qi(con, names(live$table$field_types))
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      target,
      " (",
      paste(columns, collapse = ", "),
      ") SELECT ",
      paste0("stg.", columns, collapse = ", "),
      " FROM ",
      run_table_ident(con, staging),
      " AS stg WHERE ",
      where
    )
  )
  return(invisible(NULL))
}
