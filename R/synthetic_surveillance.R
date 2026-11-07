# Synthetic tables, synthetic mode and switchable sources of
# SurveillanceSystem_v9. The methods of the class call these functions, so the
# class file holds only their signatures and documentation.

#' @name SurveillanceSystem_v9
#' @rdname SurveillanceSystem_v9
#' @section Synthetic mode:
#' Every table that `add_table()` or `add_partitionedtable()` adds gets a
#' `csdb::SyntheticTable` in `synthetic_tables`. The argument `synthetic` takes
#' one of these:
#'
#' - `NULL`: the schema-only default. It is the spec `list()`.
#' - a spec: a named list with the elements `generator`, `dir`, `invariants`
#'   and `profile`. Each element is optional.
#' - a `csdb::SyntheticTable`, which cs9 stores as given.
#'
#' cs9 builds the `SyntheticTable` of a spec from the contract of the table.
#' The contract is the field types of the table without
#' `auto_last_updated_datetime`, the keys, and `validator_field_contents`.
#' With `dir`, cs9 calls `csdb::synthetic_table_from_dir()`, which reads
#' `profile.yaml` from `dir`. A spec MUST NOT have both `dir` and `profile`.
#'
#' The keys of the contract of a partitioned table also hold the partition
#' column. Each partition is its own database table, so two partitions can
#' hold the same series. The spec of a partitioned table also gets the
#' invariant `partition_known`, which refuses a partition value outside
#' `name_partitions`. Without `generator`, row `i` gets the partition
#' `name_partitions[(i - 1) %% length(name_partitions) + 1]`.
#'
#' `set_synthetic(TRUE)` stops unless every database is a SQLite file under
#' `tempdir()`. It checks each entry of `config$dbconfigs` and the database of
#' each table in `tables` and `partitionedtables`. The check compares
#' normalised paths with `fs::path_has_parent()`.
#'
#' `populate_synthetic()` stops unless `synthetic_mode` is `TRUE`. It stops
#' when it gets neither `task`, a task name or a `Task`, nor `tables`. It
#' fills these tables:
#'
#' - `task` alone: every table in the `tables` of the task.
#' - `tables` alone: the tables named in `tables`.
#' - `task` and `tables`: only the tables named in `tables`. Each of them
#'   MUST be in the `tables` of the task. Otherwise the call stops, and the
#'   error names the tables that are not.
#'
#' cs9 cannot tell the input tables of a task from its output tables. When
#' the `tables` of a task hold output tables, name the input tables in
#' `tables`.
#'
#' It then works in this order:
#'
#' 1. It checks the database of every table, and of every partition. One
#'    database that is not SQLite under `tempdir()` stops the whole call.
#' 2. It gets the data of each table from `synthetic()`. With a number `n`, a
#'    partitioned table gets `max(n, length(name_partitions))` rows. Every
#'    table gets `n = NULL` unchanged, which means the generator's own size.
#'    With `n = NULL`, the default generator of a partitioned table draws the
#'    size of `csdb::simulate_from_profile()`, and at least
#'    `length(name_partitions)` rows. The default generator therefore fills
#'    every partition. A custom generator decides which partitions get rows.
#' 3. It validates the data against the contract of the table that receives
#'    it. A `SyntheticTable` from the caller can be looser than the table.
#' 4. With `task`, it checks the join contract of the tables that it fills.
#'    Two of these tables that share key columns MUST have a non-empty inner
#'    join on the shared key columns. Otherwise the call stops, and the error
#'    names the two tables and the shared columns. A pair with no shared key
#'    column is not checked. With `tables` alone, there is no join check.
#' 5. For each table, it checks the database of the table again, then calls
#'    `drop_all_rows()`, then `insert_data()`. `drop_all_rows()` empties every
#'    partition, so a partition that the data does not name is left empty.
#'
#' A failure in steps 1 to 4 therefore writes nothing. The check in step 5
#' refuses a database that changed after step 1.
#'
#' Each table gets its own seed, so two tables do not share a stream of
#' random values. The seed of a table is `seed` plus the first 7 hexadecimal
#' digits of the md5 of the table name, modulo `.Machine$integer.max`.
#' `synthetic(table_name, seed = s)` uses `s` as given.
#'
#' A detached run has its own `tempdir()`. With `run-task.sh --synthetic`, the
#' implementation package MUST point its `CS9_DBCONFIG_DB_*` variables under
#' `tempdir()` in its `.onLoad()`. It MUST then call [reload_db_config()].
#'
#' `source(name)` returns the `simulator` of `add_source()` when
#' `synthetic_mode` is `TRUE`, and the `reader` otherwise. `add_source()`
#' stops unless the `reader` and the `simulator` have the same names of
#' formal arguments, in the same order. The error names both lists.
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

# The keys of the contract. A partitioned table adds its partition column,
# because each partition is its own database table and two partitions can
# hold the same series.
synthetic_contract_keys <- function(table) {
  keys <- synthetic_contract_table(table)$keys
  if (inherits(table, "DBPartitionedTableExtended_v9")) {
    keys <- union(keys, table$column_name_partition)
  }
  return(keys)
}

synthetic_spec_elements <- c("generator", "dir", "invariants", "profile")

synthetic_check_spec <- function(spec, partitioned) {
  if (!is.list(spec) || (length(spec) > 0 && is.null(names(spec)))) {
    stop(
      "synthetic must be NULL, a csdb::SyntheticTable or a named list.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(spec), synthetic_spec_elements)
  if (length(unknown) > 0) {
    stop(
      "synthetic has unknown elements: '",
      paste0(unknown, collapse = "', '"),
      "'. The elements are generator, dir, invariants and profile.",
      call. = FALSE
    )
  }
  if (!is.null(spec$dir) && !is.null(spec$profile)) {
    stop(
      "synthetic MUST NOT have both dir and profile. ",
      "csdb::synthetic_table_from_dir() reads the profile from dir.",
      call. = FALSE
    )
  }
  if (partitioned && "partition_known" %in% names(spec$invariants)) {
    stop(
      "synthetic$invariants MUST NOT have the name partition_known. ",
      "cs9 adds that invariant to a partitioned table.",
      call. = FALSE
    )
  }
  return(invisible(spec))
}

# The SyntheticTable of a table and a spec, a list with the optional elements
# generator, dir, invariants and profile. The field types are those of the
# table without auto_last_updated_datetime, which the table adds on insert.
synthetic_from_spec <- function(table, spec) {
  partitioned <- inherits(table, "DBPartitionedTableExtended_v9")
  synthetic_check_spec(spec, partitioned)
  contract <- synthetic_contract_table(table)
  field_types <- contract$field_types
  field_types <- field_types[names(field_types) != "auto_last_updated_datetime"]
  generator <- spec$generator
  invariants <- if (is.null(spec$invariants)) list() else spec$invariants
  if (partitioned) {
    column <- table$column_name_partition
    if (is.null(generator)) {
      generator <- synthetic_partition_generator(column, table$partitions)
    }
    invariants <- c(
      list(
        partition_known = synthetic_partition_invariant(
          column,
          table$partitions
        )
      ),
      invariants
    )
  }
  args <- list(
    name = synthetic_table_name(table),
    field_types = field_types,
    keys = synthetic_contract_keys(table),
    validator_field_contents = contract$validator_field_contents,
    invariants = invariants,
    generator = generator
  )
  if (is.null(spec$dir)) {
    return(do.call(
      csdb::SyntheticTable$new,
      c(args, list(profile = spec$profile))
    ))
  }
  return(do.call(
    csdb::synthetic_table_from_dir,
    c(list(dir = spec$dir), args)
  ))
}

# The schema-only SyntheticTable of a table.
synthetic_default <- function(table) {
  return(synthetic_from_spec(table, list()))
}

# Row i gets the partition partitions[(i - 1) %% length(partitions) + 1].
# With n = NULL it draws the size of simulate_from_profile(), and draws again
# with one row for each partition when that size is smaller.
synthetic_partition_generator <- function(column, partitions) {
  force(column)
  force(partitions)
  return(function(profile, n, seed, world) {
    d <- csdb::simulate_from_profile(profile, n = n, seed = seed, world = world)
    if (is.null(n) && nrow(d) < length(partitions)) {
      d <- csdb::simulate_from_profile(
        profile,
        n = length(partitions),
        seed = seed,
        world = world
      )
    }
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

# Stores `synthetic` under the table's name: a SyntheticTable as given, or the
# SyntheticTable of a spec. NULL is the spec list().
synthetic_register <- function(ss, table, synthetic) {
  if (!inherits(synthetic, "SyntheticTable")) {
    if (is.null(synthetic)) {
      synthetic <- list()
    }
    synthetic <- synthetic_from_spec(table, synthetic)
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

# The tables of a task, named by table name. NULL gives none.
synthetic_task_tables <- function(ss, task) {
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
  return(found)
}

# The table objects to fill, named by table name.
synthetic_targets <- function(ss, task, tables) {
  if (is.null(task) && is.null(tables)) {
    stop("populate_synthetic() needs task or tables.", call. = FALSE)
  }
  found <- synthetic_task_tables(ss, task)
  if (is.null(tables)) {
    return(found)
  }
  if (!is.null(task)) {
    # The caller names the inputs of the task.
    stranger <- setdiff(tables, names(found))
    if (length(stranger) > 0) {
      stop(
        "populate_synthetic() fills only tables of the task. These tables ",
        "are not in the tables of the task: ",
        paste0(stranger, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    return(found[tables])
  }
  all_tables <- c(ss$tables, ss$partitionedtables)
  found <- list()
  for (x in tables) {
    if (is.null(all_tables[[x]])) {
      stop("No table ", x, ".", call. = FALSE)
    }
    found[[x]] <- all_tables[[x]]
  }
  return(found)
}

# The rows to draw for a table. With a number, a partitioned table gets at
# least one row for each partition. NULL, the generator's own size, stays NULL.
synthetic_n <- function(table, n) {
  if (!is.null(n) && inherits(table, "DBPartitionedTableExtended_v9")) {
    return(max(n, length(table$partitions)))
  }
  return(n)
}

# The seed of one table: seed plus the first 7 hexadecimal digits of the md5
# of the table name, modulo .Machine$integer.max. NULL stays NULL.
synthetic_table_seed <- function(seed, table_name) {
  if (is.null(seed)) {
    return(NULL)
  }
  ok <- is.numeric(seed) && length(seed) == 1 && !is.na(seed)
  if (!ok || seed != round(seed)) {
    stop("seed must be NULL or one whole number.", call. = FALSE)
  }
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  writeBin(charToRaw(enc2utf8(table_name)), f)
  offset <- strtoi(substr(unname(tools::md5sum(f)), 1, 7), 16L)
  return(as.integer((seed + offset) %% .Machine$integer.max))
}

# The distinct values of `columns` in `d`, as text, so that two tables with
# different R types of a column can still be compared.
synthetic_key_values <- function(d, columns) {
  values <- lapply(d[, columns, with = FALSE], as.character)
  return(unique(data.table::as.data.table(values)))
}

# Stops when two tables of a task share key columns and their data have no
# row in common on those columns. A pair with no shared key column passes.
synthetic_check_joins <- function(task_tables, data) {
  nms <- names(task_tables)
  if (length(nms) < 2) {
    return(invisible(NULL))
  }
  keys <- lapply(task_tables, synthetic_contract_keys)
  for (pair in utils::combn(nms, 2, simplify = FALSE)) {
    shared <- intersect(keys[[pair[1]]], keys[[pair[2]]])
    if (length(shared) == 0) {
      next
    }
    joined <- merge(
      synthetic_key_values(data[[pair[1]]], shared),
      synthetic_key_values(data[[pair[2]]], shared),
      by = shared
    )
    if (nrow(joined) == 0) {
      stop(
        "populate_synthetic() refuses the task: the synthetic data of the ",
        "tables ",
        pair[1],
        " and ",
        pair[2],
        " have no row in common on their shared key columns ",
        paste0(shared, collapse = ", "),
        ". Nothing was written.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
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
    d <- synthetic_get(
      ss,
      x,
      dataset,
      synthetic_n(targets[[x]], n),
      synthetic_table_seed(seed, x)
    )
    synthetic_default(targets[[x]])$validate(d)
    data[[x]] <- d
  }
  if (!is.null(task)) {
    synthetic_check_joins(targets, data)
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
  args_reader <- names(formals(reader))
  args_simulator <- names(formals(simulator))
  if (!identical(args_reader, args_simulator)) {
    stop(
      "reader and simulator of source ",
      name,
      " MUST have the same formal arguments. reader has (",
      paste0(args_reader, collapse = ", "),
      "), simulator has (",
      paste0(args_simulator, collapse = ", "),
      ").",
      call. = FALSE
    )
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
