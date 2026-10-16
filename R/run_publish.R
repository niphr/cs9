# The start of a task run, the resume of an earlier attempt, and the publish.
# See R/run_state.R for the design.

# Does a run with a key stage? A run with a key promises to stage. When it
# cannot, it stops before any write, rather than write directly.
#
# No staged table: a task without tables, such as an export, and a task whose
# every table is a config table run unstaged. Any other way to no staged table
# stops, so a keyed run never runs unstaged by surprise.
run_will_stage <- function(task, tables, found) {
  problem <- found$problem
  if (is.null(problem) && length(found$targets) == 0) {
    if (length(tables) == 0 || isTRUE(found$config_only)) {
      return(FALSE)
    }
    problem <- "none of its tables can be staged"
  }
  if (!is.null(problem)) {
    stop(
      "cs9: task ",
      task,
      " cannot stage its writes, because ",
      problem,
      ". Run it without a run_key to write its tables directly.",
      call. = FALSE
    )
  }
  return(TRUE)
}

# Starts the run of a task, before Task$run() builds the plans. Returns the run
# state, or NULL when the run stages nothing. The run then writes its tables
# directly, as it did before 26.10.14. A keyed run that stages nothing still
# keeps its date: see run_date_unstaged().
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
  if (!run_will_stage(task, tables, found)) {
    config$run_today <- run_date_unstaged(task, run_key)
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
  row <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT run_date, published_datetime FROM ",
      run_store_ident(con, "cs9_run", state),
      " WHERE ",
      run_where_store(con, state)
    )
  )
  # An attempt of a published run does nothing, so it does not fence either.
  if (nrow(row) > 0 && !is.na(row$published_datetime[1])) {
    state$today <- as.Date(row$run_date[1])
    state$published <- TRUE
    started <- TRUE
    return(state)
  }
  run_ensure_staging_indexes(con, found$targets)
  run_fence(con, state, found$targets)
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
  }
  started <- TRUE
  return(state)
}

# The date of a keyed run that stages nothing: its task has no table, or only
# config tables. Such a run has no run database. Its row in cs9_run is in the
# config database, on the connection of config_log, which Task$run() writes
# to anyway. The first attempt inserts the row, and a later attempt reads it.
#
# The run takes no lock and creates no other bookkeeping table. Its plans run
# again on every attempt, and nothing marks its row as published.
#
# On a config database other than PostgreSQL or SQLite it stores nothing and
# returns NULL, so run_today() reads the clock, as before 26.10.19.
run_date_unstaged <- function(task, run_key) {
  conn <- config$tables$config_log$dbconnection
  if (!run_driver_supported(conn$config$driver)) {
    return(NULL)
  }
  state <- list(
    task = task,
    run_key = run_key,
    conn = conn,
    schema = if (is.null(conn$config$schema)) "" else conn$config$schema,
    sqlite = identical(toupper(conn$config$driver), "SQLITE")
  )
  con <- conn$autoconnection
  run_store_create(con, state, "cs9_run")
  ident <- run_store_ident(con, "cs9_run", state)
  # Two attempts that start at once keep one row.
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      ident,
      " (task, run_key, run_date) VALUES (",
      paste(
        run_q(con, c(task, run_key, as.character(run_clock_today()))),
        collapse = ", "
      ),
      ") ON CONFLICT DO NOTHING"
    )
  )
  row <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT run_date FROM ",
      ident,
      " WHERE ",
      run_where_store(con, state)
    )
  )
  return(as.Date(row$run_date[1]))
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
#
# A known limit, kept as it is: a task name can run with tables and without
# them, for example after its tables change. The config tables and the tables
# of the task can share one database and schema. A staged run of that task then
# removes the row of every other key, also of a key that ran without tables. A
# retry of that key gets a new date. See run_date_unstaged().
#
# A published run keeps its row in cs9_run, so a later attempt with its key
# still does nothing. Only the rows of other keys that are not published are
# removed. A published run has no staged row, operation or done mark left.
run_fence <- function(con, state, targets) {
  published <- paste0(
    "SELECT run_key FROM ",
    run_store_ident(con, "cs9_run", state),
    " WHERE task = ",
    run_q(con, state$task),
    " AND published_datetime IS NOT NULL"
  )
  other <- paste0(
    "task = ",
    run_q(con, state$task),
    " AND run_key <> ",
    run_q(con, state$run_key),
    " AND run_key NOT IN (",
    published,
    ")"
  )
  # cs9_run goes last, because `other` reads it.
  for (name in rev(names(run_store_definitions))) {
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
          run_q(con, state$run_key),
          " AND ",
          run_qi(con, "cs9_run_key"),
          " NOT IN (",
          published,
          ")"
        )
      )
    }
  }
  return(invisible(NULL))
}

# Gives each staging table of the run that exists its declared indexes. csdb
# adds a missing declared index when a table object first reaches its table.
# Until 26.10.18 that happened at the first staged row. The fence and the
# clear then ran without the index, and a run that only records operations
# never got it. A staging table that does not exist holds no row to read.
run_ensure_staging_indexes <- function(con, targets) {
  for (x in targets) {
    staging <- x$.__enclos_env__$private$staging()
    if (run_staging_exists(con, staging)) {
      staging$.__enclos_env__$private$lazy_creation_of_table()
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
  run_check_plan_set(con, state, ids)
  done <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT plan_id FROM ",
      run_store_ident(con, "cs9_run_plan_done", state),
      " WHERE ",
      run_where_store(con, state)
    )
  )$plan_id
  # Plan 1 and the last plan run on every attempt. A check in the last plan
  # therefore runs again after a publish that failed.
  keep <- setdiff(intersect(done, ids), c(ids[1], ids[length(ids)]))
  run_clear_plans(con, state, targets, keep)
  return(!ids %in% keep)
}

# The first attempt of a run stores the list of its plan ids, in plan order.
# Every later attempt MUST build the identical list. A plan that is new, gone,
# changed or moved stops the attempt before any plan runs, also when the
# changed plan never started.
run_check_plan_set <- function(con, state, ids) {
  stored <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT plan_position, plan_id FROM ",
      run_store_ident(con, "cs9_run_plan_list", state),
      " WHERE ",
      run_where_store(con, state),
      " ORDER BY plan_position"
    )
  )
  if (nrow(stored) == 0) {
    rows <- paste0(
      "(",
      run_q(con, state$task),
      ", ",
      run_q(con, state$run_key),
      ", ",
      seq_along(ids),
      ", ",
      run_q(con, ids),
      ")"
    )
    DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO ",
        run_store_ident(con, "cs9_run_plan_list", state),
        " (task, run_key, plan_position, plan_id) VALUES ",
        paste(rows, collapse = ", ")
      )
    )
    return(invisible(NULL))
  }
  if (!identical(as.character(stored$plan_id), as.character(ids))) {
    stop(
      "cs9: the plan list changed between attempts of one run; a planner read ",
      "something other than its settings and published tables. Task ",
      state$task,
      ", run '",
      state$run_key,
      "': the first attempt built ",
      nrow(stored),
      " plans, this attempt built ",
      length(ids),
      ", and the lists differ in their plans or their order. ",
      "Start a new run with a new run_key.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
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
# rows. The statements are plain SQL, not a csdb load.
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
        "DELETE FROM ",
        run_store_ident(con, "cs9_run_plan_list", state),
        " WHERE ",
        run_where_store(con, state)
      )
    )
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

# The most operations in the OR list of one DELETE. A run can record one
# operation per plan, and on 2026-10-14 a run recorded 705. The limit keeps
# each statement short. 705 operations give 2 deletes per live table. SQLite
# stops at an expression depth of 1000, and an OR list of 1001 operations
# exceeds it.
run_publish_operations_per_delete <- 500L

# The WHERE clauses of the deletes that run the operations on one live table:
# one per run_publish_operations_per_delete operations. An empty operation
# removes every row, so it gives one clause "", a DELETE with no WHERE.
#
# Every operation deletes rows from the live table, and each one runs before
# any insert. A delete leaves each row it keeps unchanged, so the deletes in
# sequence remove each row that matches at least one predicate. One DELETE with
# the predicates joined by OR removes the same rows.
#
# This holds only while each condition tests the row itself. A condition of
# drop_rows_where() or keep_rows_where() MUST NOT read the same table through
# a subquery. Every condition of a run reads the live table as it stood before
# the publish. Before 26.10.18 such a condition was already undefined.
# Parallel plans record the operations, and SELECT DISTINCT returns them in no
# fixed order.
run_operations_where <- function(ops) {
  if (nrow(ops) == 0) {
    return(character())
  }
  if (any(ops$operation == "empty")) {
    return("")
  }
  predicates <- vapply(
    seq_len(nrow(ops)),
    function(i) {
      return(run_operation_predicate(ops$operation[i], ops$sql_condition[i]))
    },
    character(1)
  )
  chunk <- ceiling(seq_along(predicates) / run_publish_operations_per_delete)
  return(vapply(
    split(predicates, chunk),
    function(p) {
      return(paste0(" WHERE ", paste0("(", p, ")", collapse = " OR ")))
    },
    character(1),
    USE.NAMES = FALSE
  ))
}

# Publishes one live table: its operations, then the delete of the live rows
# that a staged upsert replaces, then the insert of every staged row.
run_publish_part <- function(con, state, live, staging, part_col, ops, rows) {
  target <- run_table_ident(con, live$table)
  for (where in run_operations_where(ops)) {
    DBI::dbExecute(con, paste0("DELETE FROM ", target, where))
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
