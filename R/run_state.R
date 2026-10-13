# A task run stages every write to its own tables and publishes them at the
# end, in one transaction. vignette("rebuilding-a-table-safely") explains it to
# a user. This file holds the state of a run, the plan ids and the bookkeeping
# tables. R/run_staging.R holds the staged writes, R/run_publish.R the start of
# a run and the publish.
#
# config$run is NULL outside a run. Task$run() sets it before it builds the
# plans and clears it on exit. A forked plan inherits it. Its elements:
#   task        the task name.
#   run_key     the key of the run. Attempts with one key resume each other.
#               A run without a key stages nothing and sets no state.
#   today       the date of the run, stored with the key on the first attempt.
#   plan_id     the id of the plan in progress. NULL outside a plan.
#   first_plan  the id of plan 1, which runs again on every attempt.
#   staged      the live names of the tables whose writes are staged.
#   children    the table names of the partitions of staged partitioned
#               tables. A direct write to one of them stops.
#   pid         the process that started the run.
#   conn        the csdb::DBConnection_v9 of the run database.
#   schema      the schema of the bookkeeping tables, or "" on SQLite.
#   sqlite      TRUE on SQLite.
#   published   TRUE when an earlier attempt published this run.

# The plan tag of a write outside every plan. action_before_fn and
# action_after_fn run again on every attempt, like plan 1.
run_before_plans <- "cs9_before_plans"
run_after_plans <- "cs9_after_plans"

# The clock of the run date. A test replaces lubridate::today().
run_clock_today <- function() {
  return(lubridate::today())
}

#' The date of the task run
#'
#' @description
#' Returns the date of the run with a `run_key` in progress: the date of its
#' first attempt, also on a retry after midnight. Outside such a run it returns
#' today's date, as `Sys.Date()` does.
#'
#' @details
#' cs9 sets `argset$today` to this date. Use `run_today()` in a planner instead
#' of `Sys.Date()`, so that every attempt of a run builds the same plans.
#'
#' @return A `Date`.
#' @export
#' @examples
#' run_today()
run_today <- function() {
  if (!is.null(config$run$today)) {
    return(config$run$today)
  }
  return(run_clock_today())
}

# psql loads the key in a tab-separated CSV file without quotes, so a tab, a
# line break or a double quote would break the load. NULL is not checked here:
# run_begin() reads it as "do not stage".
run_check_key <- function(run_key) {
  if (!run_key_ok(run_key)) {
    stop(
      "cs9: run_key MUST be NULL, or one non-empty string of at most 500 ",
      "bytes, without a tab, a line break or a double quote.",
      call. = FALSE
    )
  }
  return(run_key)
}

run_key_ok <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  return(
    nzchar(x) && !grepl("[\t\r\n\"]", x) && nchar(x, type = "bytes") <= 500L
  )
}

# The argset fields that cs9 adds to a plan. They change between two builds of
# the same plan, so the plan id ignores them. A task from add_task() also gets
# index, today and yesterday from cs9. A task built by hand sets those itself.
run_injected_fields <- function(built_by_cs9) {
  fields <- c(
    "index_plan",
    "index_analysis",
    "first_analysis",
    "last_analysis",
    "within_plan_first_analysis",
    "within_plan_last_analysis"
  )
  if (built_by_cs9) {
    fields <- c(fields, "index", "today", "yesterday")
  }
  return(fields)
}

# The id of a plan: the MD5 hash of the argsets of its analyses, without the
# injected fields. plnr names each analysis with a new UUID on every build, so
# the names are dropped. A file holds the text, because base R hashes files.
run_plan_id <- function(plan, injected) {
  argsets <- unname(lapply(plan$analyses, function(a) {
    x <- a$argset
    return(x[setdiff(names(x), injected)])
  }))
  text <- deparse(
    argsets,
    control = c(
      "keepNA",
      "keepInteger",
      "niceNames",
      "showAttributes",
      "digits17"
    )
  )
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  writeLines(text, f, useBytes = TRUE)
  return(unname(tools::md5sum(f)))
}

# Stops when two plans have the same id. cs9 tells the plans of one run apart by
# their ids, so two plans with one id would share a done mark and staged rows.
run_plan_ids <- function(plans, injected, task) {
  ids <- vapply(plans, run_plan_id, character(1), injected = injected)
  dup <- which(duplicated(ids))
  if (length(dup) > 0) {
    same <- which(ids == ids[dup[1]])
    stop(
      "cs9: plans ",
      paste(same, collapse = " and "),
      " of task ",
      task,
      " have the same settings. cs9 tells the plans of a run apart by their ",
      "settings. Each plan MUST differ from every other plan in a setting ",
      "that cs9 does not add (",
      paste(injected, collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  return(ids)
}

# The tag of a staged row or an operation: the plan in progress, or the hook
# that runs outside the plans.
run_plan_tag <- function() {
  id <- config$run$plan_id
  if (is.null(id)) {
    return(run_before_plans)
  }
  return(id)
}

# SQL text ----

run_q <- function(con, x) {
  return(as.character(DBI::dbQuoteString(con, as.character(x))))
}

run_qi <- function(con, x) {
  return(as.character(DBI::dbQuoteIdentifier(con, x)))
}

# A bookkeeping table in the schema of the run database.
run_store_ident <- function(con, name, state = config$run) {
  if (isTRUE(state$sqlite) || !nzchar(state$schema)) {
    return(run_qi(con, name))
  }
  return(run_qi(con, DBI::Id(schema = state$schema, table = name)))
}

# A csdb table object, quoted for SQL.
run_table_ident <- function(con, table) {
  return(run_qi(
    con,
    table$table_name_short_for_mssql_fully_specified_for_postgres
  ))
}

# The rows of this run of this task, in a table with cs9_task and cs9_run_key.
run_where_run <- function(con, state = config$run, prefix = "") {
  return(paste0(
    prefix,
    run_qi(con, "cs9_task"),
    " = ",
    run_q(con, state$task),
    " AND ",
    prefix,
    run_qi(con, "cs9_run_key"),
    " = ",
    run_q(con, state$run_key)
  ))
}

# The rows of this run of this task, in a bookkeeping table.
run_where_store <- function(con, state = config$run) {
  return(paste0(
    "task = ",
    run_q(con, state$task),
    " AND run_key = ",
    run_q(con, state$run_key)
  ))
}

# The bookkeeping tables ----

# Three tables in the run database, beside the staging tables. One publish
# transaction then covers the live tables, the staging tables and the marks.
run_store_definitions <- list(
  cs9_run = paste(
    "task VARCHAR NOT NULL, run_key VARCHAR NOT NULL,",
    "run_date VARCHAR NOT NULL, published_datetime VARCHAR,",
    "PRIMARY KEY (task, run_key)"
  ),
  cs9_run_plan_done = paste(
    "task VARCHAR NOT NULL, run_key VARCHAR NOT NULL,",
    "plan_id VARCHAR NOT NULL, datetime VARCHAR NOT NULL,",
    "PRIMARY KEY (task, run_key, plan_id)"
  ),
  cs9_run_operation = paste(
    "task VARCHAR NOT NULL, run_key VARCHAR NOT NULL,",
    "plan_id VARCHAR NOT NULL, table_name VARCHAR NOT NULL,",
    "operation VARCHAR NOT NULL, sql_condition VARCHAR"
  ),
  # The plan ids of the first attempt, in plan order. Every later attempt MUST
  # build the same list, in the same order, or it stops before any plan runs.
  cs9_run_plan_list = paste(
    "task VARCHAR NOT NULL, run_key VARCHAR NOT NULL,",
    "plan_position INTEGER NOT NULL, plan_id VARCHAR NOT NULL,",
    "PRIMARY KEY (task, run_key, plan_position)"
  )
)

# PostgreSQL creates a table under role_create_table, as csdb does.
run_with_role <- function(con, sql, state) {
  role <- state$conn$config$role_create_table
  if (state$sqlite || is.null(role) || is.na(role) || role == "x") {
    return(sql)
  }
  return(paste0("SET ROLE ", run_qi(con, role), "; ", sql, "; RESET ROLE"))
}

# Two runs that start at once can both try to create a table. The loser's error
# is ignored when the table exists afterwards.
run_store_create <- function(con, state) {
  for (name in names(run_store_definitions)) {
    sql <- paste0(
      "CREATE TABLE IF NOT EXISTS ",
      run_store_ident(con, name, state),
      " (",
      run_store_definitions[[name]],
      ")"
    )
    tryCatch(
      DBI::dbExecute(con, run_with_role(con, sql, state)),
      error = function(e) {
        id <- if (state$sqlite) {
          name
        } else {
          DBI::Id(schema = state$schema, table = name)
        }
        if (!DBI::dbExistsTable(con, id)) {
          stop(e)
        }
        return(invisible(NULL))
      }
    )
  }
  return(invisible(NULL))
}

# Marks a plan as done, after its writes. A forked plan uses the connection of
# one of its tables, which it holds already. The main process uses the run
# connection.
run_mark_plan_done <- function(plan_id, tables) {
  state <- config$run
  con <- run_connection(tables)
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      run_store_ident(con, "cs9_run_plan_done"),
      " (task, run_key, plan_id, datetime) VALUES (",
      paste(
        run_q(con, c(state$task, state$run_key, plan_id, cstime::now_c())),
        collapse = ", "
      ),
      ")"
    )
  )
  return(invisible(NULL))
}

# The connection for bookkeeping in the current process. The main process uses
# the run connection. A forked plan MUST NOT use it, so it uses a staged table's
# connection: one that is open, or else the first.
run_connection <- function(tables) {
  state <- config$run
  if (identical(Sys.getpid(), state$pid)) {
    return(state$conn$autoconnection)
  }
  conns <- lapply(run_targets(tables)$targets, function(x) {
    return(x$dbconnection)
  })
  for (conn in conns) {
    if (!is.null(conn$connection)) {
      return(conn$autoconnection)
    }
  }
  return(conns[[1]]$autoconnection)
}

# The tables of a task ----

# The staging works on PostgreSQL and SQLite.
run_driver_supported <- function(driver) {
  return(
    identical(driver, "PostgreSQL Unicode") ||
      identical(toupper(driver), "SQLITE")
  )
}

# The live name and the dbconfig of a table that cs9 can stage, or NULL.
run_target_info <- function(x) {
  if (inherits(x, "DBPartitionedTableExtended_v9")) {
    info <- list(name = x$table_name_base, dbconfig = x$tables[[1]]$dbconfig)
  } else if (inherits(x, "DBTable_v9")) {
    info <- list(name = x$table_name, dbconfig = x$dbconfig)
  } else {
    return(NULL)
  }
  # Only the cs9 table classes stage. A plain csdb::DBTable_v9, such as a
  # config table, takes part in the name check only.
  info$stageable <- inherits(x, c("DBPartitionedTableExtended_v9", "DBTableExtended_v9"))
  return(info)
}

# Is a table one of cs9's own config tables, the objects in config$tables? A
# table is identified by its name, its database and its schema. Sharing the
# database and the schema of the config access is NOT enough: in production
# the config and the anon accesses share both, and every task table then
# counted as a config table, so a keyed run ran unstaged (2026-10-13).
run_is_config_table <- function(info) {
  config_ids <- vapply(
    config$tables,
    function(t) run_table_identity(t$table_name, t$dbconfig),
    character(1)
  )
  return(run_table_identity(info$name, info$dbconfig) %in% config_ids)
}

run_table_identity <- function(name, dbconfig) {
  schema <- if (is.null(dbconfig$schema)) "" else as.character(dbconfig$schema)
  return(paste(name, run_database_id(dbconfig), schema, sep = "|"))
}

run_database_id <- function(dbconfig) {
  return(paste(
    dbconfig$driver,
    dbconfig$server,
    dbconfig$port,
    dbconfig$db,
    sep = "|"
  ))
}

# The tables of a task that its run stages, named by their live names, and the
# dbconfig of the run database. `problem` names why the run stages nothing.
run_targets <- function(tables) {
  out <- list(targets = list(), dbconfig = NULL, problem = NULL)
  # First pass, over every object in the order the task lists it, before any
  # filter or deduplication: each MUST be a cs9 table class that stages, or a
  # table of the config access. Anything else stops a keyed run, because the
  # run would write it outside the publish.
  checked <- run_targets_check(tables)
  if (!is.null(checked$problem)) {
    out$problem <- checked$problem
    return(out)
  }
  # After the first pass every object is a known table. Leaving out the
  # objects without info keeps the steps below safe, whatever the pass let
  # through.
  known <- !vapply(checked$infos, is.null, logical(1))
  infos <- checked$infos[known]
  targets <- tables[known]
  if (length(targets) == 0) {
    return(out)
  }
  # One table can appear twice in a task's tables. Two different tables with
  # one name, in two databases or schemas, cannot: cs9 tells staged tables
  # apart by their names. The check covers every table of the task, the
  # config tables included, so it runs before they are set aside.
  same <- vapply(
    infos,
    function(i) paste(i$name, run_database_id(i$dbconfig), i$dbconfig$schema, sep = "|"),
    character(1)
  )
  targets <- targets[!duplicated(same)]
  infos <- infos[!duplicated(same)]
  table_names <- vapply(infos, function(i) i$name, character(1))
  names(targets) <- table_names
  if (anyDuplicated(table_names) > 0) {
    out$problem <- paste0(
      "two tables share the name ",
      table_names[duplicated(table_names)][1]
    )
    return(out)
  }
  # Set aside every table that this run does not stage. After the first pass
  # these are cs9's own config tables only. They are never staged, also when
  # a task lists them, and are written directly, as before 26.10.14.
  aside <- vapply(
    infos,
    function(i) !i$stageable || run_is_config_table(i),
    logical(1)
  )
  # A task whose every table is a config table stages nothing, and that is
  # allowed. run_begin() stops on any other way to no staged table.
  out$config_only <- all(vapply(infos[aside], run_is_config_table, logical(1))) &&
    all(aside)
  targets <- targets[!aside]
  infos <- infos[!aside]
  if (length(targets) == 0) {
    return(out)
  }
  drivers <- vapply(
    infos,
    function(i) as.character(i$dbconfig$driver),
    character(1)
  )
  databases <- unique(vapply(
    infos,
    function(i) run_database_id(i$dbconfig),
    character(1)
  ))
  if (!all(vapply(drivers, run_driver_supported, logical(1)))) {
    out$problem <- "a table of the task uses a driver other than PostgreSQL or SQLite"
  } else if (length(databases) > 1) {
    out$problem <- "the tables of the task are in more than one database"
  } else {
    out$targets <- targets
    out$dbconfig <- infos[[1]]$dbconfig
  }
  return(out)
}

# The first pass of run_targets(): the info of every object of a task's
# tables, or the problem with the first object that is neither a cs9 table
# class that stages nor a table of the config access.
run_targets_check <- function(tables) {
  labels <- names(tables)
  if (is.null(labels)) {
    labels <- rep("", length(tables))
  }
  infos <- vector("list", length(tables))
  for (k in seq_along(tables)) {
    info <- run_target_info(tables[[k]])
    if (is.null(info) || !(info$stageable || run_is_config_table(info))) {
      return(list(problem = paste0(
        "the table ",
        run_object_label(info, labels[k], k, tables[[k]]),
        " is not a cs9 table class that cs9 can stage"
      )))
    }
    # `[<-` keeps the position. `[[<-` with NULL would drop the element.
    infos[k] <- list(info)
  }
  problem <- run_reserved_names(vapply(infos, function(i) i$name, character(1)))
  if (!is.null(problem)) {
    return(list(problem = problem))
  }
  return(list(infos = infos, problem = NULL))
}

# A staging table is named after its live table, with the suffix "_cs9stg".
# A live table MUST NOT use such a name: it would be the staging table of
# another table, or look like one. Every staging name ends with the suffix,
# also a cut one, so the suffix check also covers a cut name.
run_reserved_names <- function(table_names) {
  staging_names <- vapply(table_names, run_staging_name, character(1))
  for (k in seq_along(table_names)) {
    owner <- table_names[staging_names == table_names[k]]
    if (length(owner) > 0) {
      return(paste0(
        "the table ",
        table_names[k],
        " collides with the staging table of ",
        owner[1]
      ))
    }
    if (endsWith(table_names[k], "_cs9stg")) {
      return(paste0(
        "the table ",
        table_names[k],
        " uses a name reserved for staging"
      ))
    }
  }
  return(NULL)
}

# The name of a table for an error message: its table name, else the name it
# has in the task's tables, else its position, with the class of the object.
run_object_label <- function(info, label, k, x) {
  if (!is.null(info)) {
    return(info$name)
  }
  where <- if (nzchar(label)) paste0("'", label, "'") else paste0("number ", k)
  return(paste0(where, " (an object of class ", class(x)[1], ")"))
}

# The table names of the partitions of the staged partitioned tables.
run_children_names <- function(targets) {
  out <- character()
  for (x in targets) {
    if (inherits(x, "DBPartitionedTableExtended_v9")) {
      out <- c(
        out,
        vapply(x$tables, function(child) child$table_name, character(1))
      )
    }
  }
  return(unname(out))
}
