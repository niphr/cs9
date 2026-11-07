# Synthetic tables, synthetic mode and switchable sources of
# SurveillanceSystem_v9. The methods of the class call these functions, so the
# class file holds only their signatures and documentation.

#' @name SurveillanceSystem_v9
#' @rdname SurveillanceSystem_v9
#' @section Synthetic mode:
#' Every table that `add_table()` or `add_partitionedtable()` adds gets a
#' `csdb::SyntheticTable` in `synthetic_tables`. With `synthetic = NULL` it is
#' the schema-only default. The default uses the field types and keys of the
#' table, without `auto_last_updated_datetime`, and the table's
#' `validator_field_contents`.
#'
#' The default of a partitioned table gives row `i` the partition
#' `name_partitions[(i - 1) %% length(name_partitions) + 1]`. An invariant
#' refuses a partition value outside `name_partitions`.
#'
#' `set_synthetic(TRUE)` stops unless every database is a SQLite file under
#' `tempdir()`. It checks each entry of `config$dbconfigs` and the database of
#' each table in `tables` and `partitionedtables`. The check compares
#' normalised paths with `fs::path_has_parent()`.
#'
#' `populate_synthetic()` stops unless `synthetic_mode` is `TRUE`. It takes
#' the tables of `task`, a task name or a `Task`, and the tables named in
#' `tables`. It stops when it gets neither. It then works in this order:
#'
#' 1. It checks the database of every table, and of every partition. One
#'    database that is not SQLite under `tempdir()` stops the whole call.
#' 2. It gets the data of each table from `synthetic()`. A partitioned table
#'    gets `max(n, length(name_partitions))` rows. The default generator
#'    therefore fills every partition at any `n` of at least 1. A custom
#'    `SyntheticTable` decides which partitions get rows.
#' 3. It validates the data against the contract of the table that receives
#'    it. A `SyntheticTable` from the caller can be looser than the table.
#' 4. For each table, it checks the database of the table again, then calls
#'    `drop_all_rows()`, then `insert_data()`. `drop_all_rows()` empties every
#'    partition, so a partition that the data does not name is left empty.
#'
#' A failure in steps 1 to 3 therefore writes nothing. The check in step 4
#' refuses a database that changed after step 1.
#'
#' A detached run has its own `tempdir()`. With `run-task.sh --synthetic`, the
#' implementation package MUST point its `CS9_DBCONFIG_DB_*` variables under
#' `tempdir()` in its `.onLoad()`. It MUST then call [reload_db_config()].
#'
#' `source(name)` returns the `simulator` of `add_source()` when
#' `synthetic_mode` is `TRUE`, and the `reader` otherwise.
NULL

# The name under which a table is stored in `tables` or `partitionedtables`.
synthetic_table_name <- function(table) {
  if (inherits(table, "DBPartitionedTableExtended_v9")) {
    return(table$table_name_base)
  }
  return(table$table_name)
}

# The table that holds the contract. A partitioned table has none of its own,
# and every child holds the same field types, keys and validators.
synthetic_contract_table <- function(table) {
  if (inherits(table, "DBPartitionedTableExtended_v9")) {
    return(table$tables[[1]])
  }
  return(table)
}

# The schema-only SyntheticTable of a table. The field types are those of the
# table without auto_last_updated_datetime, which the table adds on insert.
synthetic_default <- function(table) {
  contract <- synthetic_contract_table(table)
  field_types <- contract$field_types
  field_types <- field_types[names(field_types) != "auto_last_updated_datetime"]
  generator <- NULL
  invariants <- list()
  if (inherits(table, "DBPartitionedTableExtended_v9")) {
    generator <- synthetic_partition_generator(
      table$column_name_partition,
      table$partitions
    )
    invariants$partition_known <- synthetic_partition_invariant(
      table$column_name_partition,
      table$partitions
    )
  }
  return(csdb::SyntheticTable$new(
    name = synthetic_table_name(table),
    field_types = field_types,
    keys = contract$keys,
    validator_field_contents = contract$validator_field_contents,
    invariants = invariants,
    generator = generator
  ))
}

# Row i gets the partition partitions[(i - 1) %% length(partitions) + 1].
synthetic_partition_generator <- function(column, partitions) {
  force(column)
  force(partitions)
  return(function(profile, n, seed, world) {
    d <- csdb::simulate_from_profile(profile, n = n, seed = seed, world = world)
    data.table::set(
      d,
      j = column,
      value = rep_len(as.character(partitions), nrow(d))
    )
    return(d)
  })
}

synthetic_partition_invariant <- function(column, partitions) {
  force(column)
  force(partitions)
  return(function(d) {
    bad <- setdiff(unique(d[[column]]), as.character(partitions))
    if (length(bad) == 0) {
      return(TRUE)
    }
    return(paste0(
      column,
      " holds values that are not partitions: ",
      paste0(bad, collapse = ", "),
      "."
    ))
  })
}

# Stores `synthetic`, or the default when it is NULL, under the table's name.
synthetic_register <- function(ss, table, synthetic) {
  if (is.null(synthetic)) {
    synthetic <- synthetic_default(table)
  }
  if (!inherits(synthetic, "SyntheticTable")) {
    stop("synthetic must be NULL or a csdb::SyntheticTable.", call. = FALSE)
  }
  ss$synthetic_tables[[synthetic_table_name(table)]] <- synthetic
  return(invisible(NULL))
}

# guard ----

# The dbconfigs that a table writes through. A partitioned table writes
# through its shared connection, and each child holds a dbconfig too.
synthetic_table_dbconfigs <- function(table) {
  if (inherits(table, "DBPartitionedTableExtended_v9")) {
    return(c(
      list(table$dbconnection$config),
      lapply(table$tables, function(x) x$dbconfig)
    ))
  }
  return(list(table$dbconfig))
}

# The absolute path with `..` removed and symbolic links resolved, as far as
# the path exists. A SQLite file can be absent until its first connection.
synthetic_norm_path <- function(path) {
  path <- fs::path_norm(fs::path_abs(path))
  rest <- character(0)
  while (!fs::file_exists(path) && fs::path_dir(path) != path) {
    rest <- c(fs::path_file(path), rest)
    path <- fs::path_dir(path)
  }
  if (fs::file_exists(path)) {
    path <- fs::path_real(path)
  }
  return(fs::path_norm(do.call(fs::path, as.list(c(path, rest)))))
}

synthetic_is_path <- function(x) {
  return(is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x))
}

# NULL when the dbconfig is a SQLite file under tempdir(), else the reason.
synthetic_db_problem <- function(dbconfig) {
  driver <- dbconfig$driver
  db <- dbconfig$db
  if (!identical(toupper(as.character(driver)), "SQLITE")) {
    return(paste0("the driver is '", driver, "', not SQLite"))
  }
  if (!synthetic_is_path(db)) {
    return("the SQLite file has no path")
  }
  under <- fs::path_has_parent(
    synthetic_norm_path(db),
    synthetic_norm_path(tempdir())
  )
  if (!isTRUE(under)) {
    return(paste0("the SQLite file '", db, "' is not under tempdir()"))
  }
  return(NULL)
}

synthetic_check_dbconfigs <- function(dbconfigs, what) {
  problems <- unlist(lapply(dbconfigs, synthetic_db_problem))
  if (length(problems) > 0) {
    stop(
      "Synthetic mode refuses ",
      what,
      ": ",
      paste0(unique(problems), collapse = "; "),
      ". Synthetic mode MUST write only to a SQLite file under tempdir().",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# mode ----

synthetic_set_mode <- function(ss, on) {
  if (!is.logical(on) || length(on) != 1 || is.na(on)) {
    stop("on must be TRUE or FALSE.", call. = FALSE)
  }
  if (on) {
    synthetic_check_dbconfigs(config$dbconfigs, "config$dbconfigs")
    for (table in c(ss$tables, ss$partitionedtables)) {
      synthetic_check_dbconfigs(
        synthetic_table_dbconfigs(table),
        paste0("table ", synthetic_table_name(table))
      )
    }
  }
  ss$synthetic_mode <- on
  return(invisible(ss))
}

synthetic_set_world <- function(ss, world) {
  if (!is.null(world) && (!is.list(world) || is.null(names(world)))) {
    stop("world must be NULL or a named list.", call. = FALSE)
  }
  ss$synthetic_world <- world
  return(invisible(ss))
}

# data ----

synthetic_get <- function(ss, table_name, dataset, n, seed) {
  st <- ss$synthetic_tables[[table_name]]
  if (is.null(st)) {
    stop("No SyntheticTable for table ", table_name, ".", call. = FALSE)
  }
  if (is.null(dataset)) {
    return(st$random(n = n, seed = seed, world = ss$synthetic_world))
  }
  return(st$dataset(dataset, world = ss$synthetic_world))
}

# The table objects to fill, named by table name.
synthetic_targets <- function(ss, task, tables) {
  if (is.null(task) && is.null(tables)) {
    stop("populate_synthetic() needs task or tables.", call. = FALSE)
  }
  if (is.character(task)) {
    if (is.null(ss$tasks[[task]])) {
      stop("No task ", task, ".", call. = FALSE)
    }
    task <- ss$tasks[[task]]
  }
  found <- list()
  for (table in task$tables) {
    found[[synthetic_table_name(table)]] <- table
  }
  all_tables <- c(ss$tables, ss$partitionedtables)
  for (x in tables) {
    if (is.null(all_tables[[x]])) {
      stop("No table ", x, ".", call. = FALSE)
    }
    found[[x]] <- all_tables[[x]]
  }
  return(found)
}

# The rows to draw for a table. A partitioned table gets at least one row for
# each partition.
synthetic_n <- function(table, n) {
  if (inherits(table, "DBPartitionedTableExtended_v9")) {
    return(max(n, length(table$partitions)))
  }
  return(n)
}

synthetic_populate <- function(ss, task, tables, dataset, n, seed) {
  if (!isTRUE(ss$synthetic_mode)) {
    stop(
      "populate_synthetic() needs synthetic mode. Call set_synthetic(TRUE).",
      call. = FALSE
    )
  }
  targets <- synthetic_targets(ss, task, tables)
  # Every table first, so one unsafe table stops the call before any write.
  for (x in names(targets)) {
    synthetic_check_dbconfigs(
      synthetic_table_dbconfigs(targets[[x]]),
      paste0("table ", x)
    )
  }
  # All data next, validated against the table that receives it, so a
  # failure writes nothing.
  data <- list()
  for (x in names(targets)) {
    d <- synthetic_get(ss, x, dataset, synthetic_n(targets[[x]], n), seed)
    synthetic_default(targets[[x]])$validate(d)
    data[[x]] <- d
  }
  for (x in names(targets)) {
    # Immediately before the first write to this table.
    synthetic_check_dbconfigs(
      synthetic_table_dbconfigs(targets[[x]]),
      paste0("table ", x)
    )
    targets[[x]]$drop_all_rows()
    targets[[x]]$insert_data(data.table::copy(data[[x]]))
  }
  return(invisible(vapply(data, nrow, integer(1))))
}

# sources ----

synthetic_add_source <- function(ss, name, reader, simulator) {
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    stop("name must be one string.", call. = FALSE)
  }
  if (!is.function(reader) || !is.function(simulator)) {
    stop("reader and simulator must be functions.", call. = FALSE)
  }
  ss$sources[[name]] <- list(reader = reader, simulator = simulator)
  return(invisible(ss))
}

synthetic_source <- function(ss, name) {
  src <- ss$sources[[name]]
  if (is.null(src)) {
    stop("No source ", name, ".", call. = FALSE)
  }
  if (isTRUE(ss$synthetic_mode)) {
    return(src$simulator)
  }
  return(src$reader)
}
