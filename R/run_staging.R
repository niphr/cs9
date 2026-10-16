# The staged writes of a task run. See R/run_state.R for the design.
#
# Each live table has ONE staging table, also when the live table is
# partitioned. The staging table has the live columns and four more:
#   cs9_task     the task that wrote the row.
#   cs9_run_key  the run that wrote the row.
#   cs9_plan     the plan that wrote the row.
#   cs9_upsert   1 when the publish replaces a live row with the same key.
# Its primary key is the live key, the partition column of a partitioned
# table, cs9_task and cs9_run_key. A run therefore cannot stage two rows with
# one live key: the second insert stops on the duplicate key.
#
# A destructive write is not staged. cs9 records it as an operation instead,
# and the publish runs it on the live table before it inserts the staged rows.

run_staging_columns <- c("cs9_task", "cs9_run_key", "cs9_plan", "cs9_upsert")

# The staging name: the live name and "_cs9stg". A live name that leaves no
# room under the 63-byte PostgreSQL limit is cut, and 8 characters of its MD5
# hash keep the name unique.
run_staging_name <- function(live_name) {
  suffix <- "_cs9stg"
  if (nchar(live_name, type = "bytes") + nchar(suffix) <= 63L) {
    return(paste0(live_name, suffix))
  }
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  writeLines(live_name, f, useBytes = TRUE)
  hash <- substr(unname(tools::md5sum(f)), 1, 8)
  return(paste0(
    run_cut_bytes(live_name, 63L - nchar(suffix) - 9L),
    "_",
    hash,
    suffix
  ))
}

# The first characters of `x` that fit in `max_bytes` bytes of UTF-8. It cuts
# whole characters, so it never leaves half of a multibyte character.
# strtrim() cuts by display width, and one "\u00f8" is 1 column but 2 bytes.
run_cut_bytes <- function(x, max_bytes) {
  x <- enc2utf8(x)
  while (nchar(x, type = "bytes") > max_bytes) {
    x <- substr(x, 1L, nchar(x, type = "chars") - 1L)
  }
  return(x)
}

# A live table MUST NOT end in the staging suffix. Its name could then be the
# staging name of a table of another task, and a run of that task would use it
# as staging. The cs9 table constructors call this, so add_table() and
# add_partitionedtable() refuse such a name. A partition table name ends in
# its tag, so a partition tagged "cs9stg" is refused through its constructor.
run_check_live_name <- function(table_name) {
  if (endsWith(table_name, "_cs9stg")) {
    stop(
      "cs9: the table name ",
      table_name,
      " is not allowed: names ending in _cs9stg are reserved for staging.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The staging table of a live table. It borrows the live table's connection, so
# it opens no connection of its own. csdb creates it at its first use.
#
# The index cs9_run leads with cs9_task and cs9_run_key, then the partition
# column of a partitioned table. The publish, the fence and the clear select
# the rows of one run, and the publish selects one partition. The primary key
# leads with the live key, so it cannot serve those reads. On a staging table
# that exists, csdb creates a missing declared index at the first use of the
# table in an R process.
run_new_staging_table <- function(
  dbconfig,
  live_name,
  field_types,
  keys,
  dbconnection,
  partition_column = NULL
) {
  return(csdb::DBTable_v9$new(
    dbconfig = dbconfig,
    table_name = run_staging_name(live_name),
    field_types = c(
      field_types,
      cs9_task = "TEXT",
      cs9_run_key = "TEXT",
      cs9_plan = "TEXT",
      cs9_upsert = "INTEGER"
    ),
    keys = unique(c(keys, "cs9_task", "cs9_run_key")),
    indexes = list(
      cs9_run = c("cs9_task", "cs9_run_key", partition_column)
    ),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank,
    dbconnection = dbconnection
  ))
}

# Is a write to the table `name` staged? A direct write to a partition of a
# staged partitioned table stops, because the publish would empty it.
run_stages <- function(name) {
  state <- config$run
  if (is.null(state)) {
    return(FALSE)
  }
  if (name %in% state$children) {
    stop(
      "cs9: ",
      name,
      " is a partition of a table that this task run stages. ",
      "Write through the partitioned table, not through one partition.",
      call. = FALSE
    )
  }
  return(name %in% state$staged)
}

# Runs `staged` when the write to the table `name` is staged, else `live`.
# The table classes call it, so their methods hold no branch of their own.
run_or_live <- function(name, staged, live) {
  if (run_stages(name)) {
    return(staged())
  }
  return(live())
}

# The column check of csdb::DBTable_v9$replace_all_rows(), for its staged
# branch.
run_check_replace_columns <- function(newdata, field_types, table_name) {
  if (
    anyDuplicated(names(newdata)) > 0 ||
      !setequal(names(newdata), names(field_types))
  ) {
    stop(
      "replace_all_rows: the columns of newdata (",
      paste0(names(newdata), collapse = ", "),
      ") differ from the fields of ",
      table_name,
      " (",
      paste0(names(field_types), collapse = ", "),
      ")",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# A unique-key error on the staging table means that the run wrote one row
# twice. On PostgreSQL the csdb load reports it as an error of class
# csdb_load_duplicate_key. SQLite reports it in the text of the error.
run_is_duplicate <- function(e) {
  return(
    inherits(e, "csdb_load_duplicate_key") ||
      grepl("UNIQUE constraint failed|duplicate key", conditionMessage(e))
  )
}

run_stop_same_row <- function(label, e) {
  stop(
    "cs9: two plans wrote the same row of ",
    label,
    " in run '",
    config$run$run_key,
    "' of task ",
    config$run$task,
    ", or one plan wrote it twice. A task run MUST write each row once. ",
    "The error was: ",
    conditionMessage(e),
    call. = FALSE
  )
}

# Stages `newdata`. The four cs9 columns go into `newdata` by reference and
# leave it again on exit, so a large data.table is not copied. `stamp` adds
# auto_last_updated_datetime the same way, for a partitioned table, whose
# insert does not change the caller's data.
run_stage_rows <- function(
  staging,
  newdata,
  upsert,
  validator,
  label,
  load_timeout = 3600,
  stamp = FALSE
) {
  if (is.null(newdata) || nrow(newdata) == 0) {
    return(invisible(NULL))
  }
  if (!data.table::is.data.table(newdata)) {
    newdata <- data.table::as.data.table(newdata)
  }
  added <- setdiff(run_staging_columns, names(newdata))
  if (stamp && !"auto_last_updated_datetime" %in% names(newdata)) {
    added <- c(added, "auto_last_updated_datetime")
  }
  on.exit(
    if (length(added) > 0) {
      newdata[, (added) := NULL]
    },
    add = TRUE
  )
  if (stamp) {
    # A quoted name, so R CMD check finds no undefined global variable.
    newdata[, ("auto_last_updated_datetime") := cstime::now_c()]
  }
  # The live table's validator, which csdb would run on a live insert.
  validated <- validator(newdata)
  if (!isTRUE(validated)) {
    stop(
      "load_data_infile not validated in ",
      label,
      ". ",
      paste0(attr(validated, "var"), collapse = ", "),
      call. = FALSE
    )
  }
  newdata[, `:=`(
    cs9_task = config$run$task,
    cs9_run_key = config$run$run_key,
    cs9_plan = run_plan_tag(),
    cs9_upsert = as.integer(isTRUE(upsert))
  )]
  tryCatch(
    staging$insert_data(newdata, verbose = FALSE, load_timeout = load_timeout),
    error = function(e) {
      if (run_is_duplicate(e)) {
        run_stop_same_row(label, e)
      }
      stop(e)
    }
  )
  return(invisible(NULL))
}

# The SQL condition that selects the rows an operation removes.
run_operation_predicate <- function(operation, condition) {
  if (operation == "empty") {
    return("1 = 1")
  }
  if (operation == "delete") {
    return(paste0("(", condition, ")"))
  }
  return(paste0("(", condition, ") IS NOT TRUE"))
}

# Records a destructive operation on a staged table.
#
# Within one plan, the operation also removes the matching rows that the plan
# staged, as the live write would. Matching rows from another plan stop the
# run: the other plan may run before or after this one, so the result would
# depend on the order. Plan 1 and action_before_fn are the exception. They run
# before every other plan, also on a retry. The rows that other plans staged on
# an earlier attempt therefore came later, and the operation leaves them.
run_stage_operation <- function(
  staging,
  dbconnection,
  live_name,
  operation,
  condition = NULL
) {
  state <- config$run
  con <- dbconnection$autoconnection
  plan <- run_plan_tag()
  predicate <- run_operation_predicate(operation, condition)
  if (staging$table_exists()) {
    stg <- run_table_ident(con, staging)
    mine <- run_where_run(con, state)
    if (!plan %in% c(state$first_plan, run_before_plans)) {
      other <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) AS n FROM ",
          stg,
          " WHERE ",
          mine,
          " AND ",
          run_qi(con, "cs9_plan"),
          " <> ",
          run_q(con, plan),
          " AND ",
          predicate
        )
      )$n
      if (as.numeric(other) > 0) {
        stop(
          "cs9: delete after write is not supported during a task run. ",
          "The ",
          operation,
          " on ",
          live_name,
          " in task ",
          state$task,
          " matches ",
          as.numeric(other),
          " rows that another plan wrote ",
          "in this run.",
          call. = FALSE
        )
      }
    }
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM ",
        stg,
        " WHERE ",
        mine,
        " AND ",
        run_qi(con, "cs9_plan"),
        " = ",
        run_q(con, plan),
        " AND ",
        predicate
      )
    )
  }
  values <- c(state$task, state$run_key, plan, live_name, operation)
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      run_store_ident(con, "cs9_run_operation"),
      " (task, run_key, plan_id, table_name, operation, sql_condition) VALUES (",
      paste(run_q(con, values), collapse = ", "),
      ", ",
      if (is.null(condition)) "NULL" else run_q(con, condition),
      ")"
    )
  )
  return(invisible(NULL))
}

# Reads the rows that this run staged in a table, with the live columns.
run_staged_tbl <- function(staging, live_name, columns) {
  if (isTRUE(config$planning)) {
    stop(
      "cs9: plans may only read published tables. staged_tbl() on ",
      live_name,
      " reads rows that are not published, and a plan analysis MUST NOT ",
      "call it.",
      call. = FALSE
    )
  }
  if (is.null(config$run) || !live_name %in% config$run$staged) {
    stop(
      "cs9: staged_tbl() on ",
      live_name,
      " works only during a run of a task that has the table in its tables.",
      call. = FALSE
    )
  }
  con <- staging$dbconnection$autoconnection
  # tbl() creates the staging table when it does not exist yet.
  staging$tbl()
  sql <- paste0(
    "SELECT ",
    paste(run_qi(con, columns), collapse = ", "),
    " FROM ",
    run_table_ident(con, staging),
    " WHERE ",
    run_where_run(con)
  )
  return(dplyr::tbl(con, dplyr::sql(sql)))
}

# Creates a live table when it does not exist yet. The publish inserts into it,
# so it MUST exist before the publish starts.
run_create_live <- function(table) {
  table$.__enclos_env__$private$lazy_creation_of_table()
  return(invisible(NULL))
}
