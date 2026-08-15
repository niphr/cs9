# The class is not exported, so @noRd is right and no .Rd is wanted. The class
# block is also what keeps roxygen2 quiet. Without one, roxygen2 8.1.0 reports
# every other method as undocumented and then skips the topic anyway. Measured
# on the pod on 2026-08-14.

#' Partitioned database table with one shared connection
#'
#' @description
#' One logical table, split across one `csdb::DBTable_v9` per partition. Every
#' write and every read routes to the child that the partition value names.
#'
#' @details
#' The object owns one `csdb::DBConnection_v9` and every child borrows it. A
#' child's `disconnect()` therefore closes nothing, and
#' `DBPartitionedTableExtended_v9$disconnect()` closes the connection once.
#'
#' One connection per child would need one database backend per partition at
#' the same time. A table with 106 partitions then exceeded the NorSySS
#' PostgreSQL `max_connections` of 100, measured on 2026-08-13.
#'
#' @noRd
DBPartitionedTableExtended_v9 <- R6::R6Class(
  "DBPartitionedTableExtended_v9",
  public = list(
    tables = list(),
    partitions = c(),
    column_name_partition = "",
    # A generated partition column goes into a copy of `newdata`. A write then
    # does not add a column to the caller's data.table. The copy is inside that
    # branch only. A production import is memory-bound, so a copy of every
    # `newdata` would double the peak memory.
    value_generator_partition = NULL,
    dbconnection = NULL,
    # The indexes the constructor gave to every child. `drop_indexes` in
    # `upsert_data()` and in `drop_all_rows_and_then_upsert_data()` defaults to
    # NULL, not to `names(self$indexes)`. Each child therefore receives NULL,
    # which is what it received before this field existed. A caller who wants
    # the drop passes `drop_indexes = names(pt$indexes)`.
    indexes = NULL,
    initialize = function(
      dbconfig,
      table_name_base,
      table_name_partitions,
      column_name_partition,
      value_generator_partition = NULL,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = validator_field_types_blank,
      validator_field_contents = validator_field_contents_blank
    ) {
      force(table_name_partitions)
      # This runs before `csdb::DBConnection_v9$new()` below. A rejected
      # partition set therefore leaves no database connection open.
      private$check_for_correct_partition_set(table_name_partitions)
      self$partitions <- table_name_partitions

      force(column_name_partition)
      self$column_name_partition <- column_name_partition

      force(value_generator_partition)
      self$value_generator_partition <- value_generator_partition

      force(indexes)
      self$indexes <- indexes

      # ensure that partition name is last in dataset
      if (column_name_partition %in% names(field_types)) {
        field_types <- field_types[!names(field_types) == column_name_partition]
      }
      field_types <- c(field_types, "TEXT")
      names(field_types)[length(field_types)] <- column_name_partition

      # One connection for the whole partitioned table. Each child borrows it.
      # csdb::DBTable_v9 would otherwise build one connection per partition. A
      # table with 106 partitions then needs 106 simultaneous connections,
      # which exceeded the NorSySS PostgreSQL limit of 100 on 2026-08-13.
      # Every child has an identical dbconfig, so one connection serves all of
      # them.
      self$dbconnection <- csdb::DBConnection_v9$new(
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
      )

      self$tables <- vector("list", length(self$partitions))
      # `names()` coerces, so every child is reachable under as.character() of
      # its partition value. Every `self$tables[[...]]` below indexes by that
      # character. `[[` on a list takes a numeric index as a POSITION, so
      # `self$tables[[5]]` returns the fifth child rather than the child named
      # "5", and it grows the list to five elements when it assigns.
      names(self$tables) <- self$partitions
      for (i in self$partitions) {
        if (toupper(dbconfig$driver) == "SQLITE") {
          # PARTITION is a keyword in SQLite's window-function grammar, so the
          # separator PostgreSQL uses is reused here.
          table_name <- paste0(c(table_name_base, "xxpxx", i), collapse = "_")
        } else if (dbconfig$driver %in% c("PostgreSQL Unicode")) {
          table_name <- paste0(c(table_name_base, "xxpxx", i), collapse = "_")
        } else {
          table_name <- paste0(
            c(table_name_base, "PARTITION", i),
            collapse = "_"
          )
        }

        dbtable <- DBTableExtended_v9$new(
          dbconfig = dbconfig,
          table_name = table_name,
          field_types = field_types,
          keys = keys,
          indexes = indexes,
          validator_field_types = validator_field_types,
          validator_field_contents = validator_field_contents,
          dbconnection = self$dbconnection
        )
        self$tables[[as.character(i)]] <- dbtable
      }
    },
    #' @description
    #' Close the shared connection.
    #'
    #' The parent owns the connection and closes it here, once. A child borrows
    #' the same connection, so a child's `disconnect()` closes nothing.
    #'
    #' @details
    #' A child that a caller keeps after this call can reopen the shared
    #' connection. `csdb::DBConnection_v9$autoconnection` calls `connect()` on
    #' every access, so any later use of that child opens a new connection. The
    #' child cannot close that connection again, because it does not own it.
    #' Only `DBPartitionedTableExtended_v9$disconnect()` closes it. Callers MUST
    #' NOT use a child after they disconnect the parent. A caller that does MUST
    #' disconnect the parent again.
    disconnect = function() {
      self$dbconnection$disconnect()
    },
    insert_data = function(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE
    ) {
      if (!is.null(self$value_generator_partition)) {
        part <- do.call(self$value_generator_partition, newdata)
        newdata <- data.table::copy(newdata)
        newdata[, (self$column_name_partition) := part]
      }
      route <- private$check_for_correct_partitions_in_data(newdata)

      partitions_in_use <- unique(route)
      # randomize order
      partitions_in_use <- partitions_in_use[sample.int(length(
        partitions_in_use
      ))]
      for (i in partitions_in_use) {
        index <- route == as.character(i)
        self$tables[[as.character(i)]]$insert_data(
          newdata[index, ],
          confirm_insert_via_nrow,
          verbose
        )
      }
    },
    upsert_data = function(
      newdata,
      drop_indexes = NULL,
      verbose = TRUE
    ) {
      if (!is.null(self$value_generator_partition)) {
        part <- do.call(self$value_generator_partition, newdata)
        newdata <- data.table::copy(newdata)
        newdata[, (self$column_name_partition) := part]
      }
      route <- private$check_for_correct_partitions_in_data(newdata)

      partitions_in_use <- unique(route)
      for (i in partitions_in_use) {
        index <- route == as.character(i)
        self$tables[[as.character(i)]]$upsert_data(
          newdata[index, ],
          drop_indexes,
          verbose
        )
      }
    },
    drop_all_rows = function() {
      for (i in self$partitions_randomized) {
        self$tables[[as.character(i)]]$drop_all_rows()
      }
    },
    drop_rows_where = function(condition, verbose = FALSE) {
      partition <- 0
      for (i in self$partitions_randomized) {
        partition <- partition + 1
        if (verbose) {
          message(
            "Deleting inside partition ",
            partition,
            "/",
            length(self$partitions)
          )
        }
        self$tables[[as.character(i)]]$drop_rows_where(condition)
      }
    },
    keep_rows_where = function(condition, verbose = FALSE) {
      for (i in self$partitions_randomized) {
        self$tables[[as.character(i)]]$keep_rows_where(condition)
      }
    },
    drop_all_rows_and_then_upsert_data = function(
      newdata,
      drop_indexes = NULL,
      verbose = TRUE
    ) {
      if (!is.null(self$value_generator_partition)) {
        part <- do.call(self$value_generator_partition, newdata)
        newdata <- data.table::copy(newdata)
        newdata[, (self$column_name_partition) := part]
      }
      route <- private$check_for_correct_partitions_in_data(newdata)

      for (i in self$partitions_randomized) {
        index <- route == as.character(i)
        self$tables[[as.character(i)]]$drop_all_rows_and_then_upsert_data(
          newdata[index, ],
          drop_indexes,
          verbose
        )
      }
    },
    drop_all_rows_and_then_insert_data = function(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE
    ) {
      if (!is.null(self$value_generator_partition)) {
        part <- do.call(self$value_generator_partition, newdata)
        newdata <- data.table::copy(newdata)
        newdata[, (self$column_name_partition) := part]
      }
      route <- private$check_for_correct_partitions_in_data(newdata)

      for (i in self$partitions_randomized) {
        index <- route == as.character(i)
        self$tables[[as.character(i)]]$drop_all_rows_and_then_insert_data(
          newdata[index, ],
          confirm_insert_via_nrow,
          verbose
        )
      }
    },
    remove_table = function() {
      for (i in self$partitions_randomized) {
        self$tables[[as.character(i)]]$remove_table()
      }
    },
    drop_indexes = function() {
      for (i in self$partitions_randomized) {
        self$tables[[as.character(i)]]$drop_indexes()
      }
    },
    add_indexes = function() {
      for (i in self$partitions_randomized) {
        self$tables[[as.character(i)]]$add_indexes()
      }
    },
    confirm_indexes = function() {
      for (i in self$partitions_randomized) {
        self$tables[[as.character(i)]]$confirm_indexes()
      }
    },
    # The loop below already knows the partition tag. `i` IS that tag, and the
    # row it marks is the row of that partition's table. The tag therefore
    # travels out with the row, in the `partition` column.
    #
    # A caller that needs the tag would otherwise split `table_name` on the
    # separator. The separator is not one string: it is `xxpxx` for PostgreSQL
    # and for SQLite, and `PARTITION` for the MSSQL fallback. A caller that
    # split on `_PARTITION_` therefore got `NA` against PostgreSQL, and
    # `self$tables[[NA]]` is `NULL`. `norsyss.cs9` did exactly that, and the
    # consultations import died on `attempt to apply non-function`. Measured on
    # a NorSySS pod on 2026-08-14.
    #
    #' @description
    #' Count the rows across every partition.
    #'
    #' @param collapse TRUE returns one total. FALSE returns one row per
    #'   partition, with the columns `table_name`, `nrow` and `partition`.
    #'
    #' @details
    #' `partition` is a character column holding the partition tag. It matches
    #' `names(self$tables)`, so it indexes `self$tables[[...]]` directly.
    #'
    #' `partition` is the LAST column. `table_name` stays at position 1 and
    #' `nrow` at position 2, so a caller that reads either one by position
    #' reads what it read before.
    nrow = function(collapse = TRUE) {
      table_rows <- self$tables[[
        as.character(self$partitions[1])
      ]]$dbconnection$autoconnection %>%
        csdb::get_table_names_and_info()
      # This line declares `partition` beside `keep`, so the column is character
      # from the start. The declaration is not what creates the column. A `:=`
      # whose `i` matches no row still creates it, and gives it the type of the
      # assigned value. Measured with data.table 1.18.4 on 2026-08-14.
      table_rows[, `:=`(keep = FALSE, partition = NA_character_)]
      for (i in self$partitions_randomized) {
        table_rows[
          table_name == self$tables[[as.character(i)]]$table_name,
          `:=`(keep = TRUE, partition = as.character(i))
        ]
      }
      table_rows <- table_rows[
        keep == T,
        .(
          table_name,
          nrow,
          partition
        )
      ]
      if (collapse) {
        return(sum(table_rows$nrow))
      }

      data.table::shouldPrint(table_rows)
      return(table_rows)
    },
    #' @description
    #' Report the row count and the storage size of every partition.
    #'
    #' @param collapse FALSE returns one row per partition, with `partition`
    #'   appended as the last column. TRUE sums every size and every row count
    #'   into one row, and drops `partition`, because an aggregate spans every
    #'   partition and has no single tag.
    #'
    #' @details
    #' `partition` is a character column holding the partition tag, the same
    #' one that `nrow(collapse = FALSE)` returns. The columns before it are the
    #' ones `csdb::get_table_names_and_info()` returns, in that order.
    info = function(collapse = FALSE) {
      table_rows <- self$tables[[
        as.character(self$partitions[1])
      ]]$dbconnection$autoconnection %>%
        csdb::get_table_names_and_info()
      table_rows[, `:=`(keep = FALSE, partition = NA_character_)]
      for (i in self$partitions_randomized) {
        table_rows[
          table_name == self$tables[[as.character(i)]]$table_name,
          `:=`(keep = TRUE, partition = as.character(i))
        ]
      }
      table_rows <- table_rows[keep == T]
      # A `:=` appends, so the columns run: the columns csdb returns, then
      # `keep`, then `partition`. Dropping `keep` leaves `partition` last.
      table_rows[, keep := NULL]

      if (collapse) {
        # An aggregate spans every partition, so it has no single tag. The
        # select below therefore drops `partition`.
        table_rows <- table_rows[,
          .(
            size_total_gb = sum(size_total_gb),
            size_data_gb = sum(size_data_gb),
            size_index_gb = sum(size_index_gb),
            nrow = sum(nrow)
          )
        ]
      }
      data.table::shouldPrint(table_rows)
      return(table_rows)
    }
  ),
  active = list(
    # sometimes we want this in a randomized order, so that the SQL table isnt locked and blocked
    #
    # `sample(x, length(x))` reads a length-1 numeric `x` as the range `1:x`.
    # One partition numbered 9 then becomes a draw from 1 to 9. Indexing by a
    # permutation has no such special case, and it is a no-op below length 2.
    partitions_randomized = function() {
      self$partitions[sample.int(length(self$partitions))]
    }
  ),
  private = list(
    # Reject a partition set that the class cannot use. `initialize()` runs this
    # before it opens the connection, so a rejected construction leaves no
    # database connection open.
    #
    # `names(self$tables) <- self$partitions` coerces, and all 16
    # `self$tables[[...]]` sites index by `as.character()`. The character form
    # is therefore the identity of a partition, and three of these four checks
    # read it.
    #
    # A duplicate is the reason this method exists. `self$tables[[name]]` takes
    # the FIRST match, so a duplicated partition makes one child unreachable
    # while `length(self$partitions)` still counts it.
    #
    # An empty string is worse. `self$tables[[""]] <- child` matches no name, so
    # it APPENDS. A set of `c("a", "")` built three children for two partitions,
    # measured on 2026-08-14.
    check_for_correct_partition_set = function(table_name_partitions) {
      if (length(table_name_partitions) == 0) {
        stop(
          "table_name_partitions is empty. ",
          "A partitioned table needs at least one partition."
        )
      }
      if (any(is.na(table_name_partitions))) {
        stop("table_name_partitions holds NA.")
      }
      partition_names <- as.character(table_name_partitions)
      if (any(partition_names == "")) {
        stop("table_name_partitions holds an empty string.")
      }
      duplicates <- unique(partition_names[duplicated(partition_names)])
      if (length(duplicates) > 0) {
        stop(
          "table_name_partitions holds duplicate values: ",
          paste0(duplicates, collapse = ", "),
          "."
        )
      }
      invisible(partition_names)
    },
    # Reject data that the four write methods cannot route, and RETURN the route
    # vector they route with, before any of them destroys a row.
    #
    # That returned vector is the only expression in this file that decides
    # which partition a row belongs to. Validation and routing cannot disagree,
    # because they read the same vector. `setdiff()` here and `==` in the write
    # methods were two independent answers to one question before. They agreed.
    # Three silent-data-loss defects in this class in one week were each a
    # routing expression that looked right. Agreement today is therefore not a
    # guarantee for tomorrow.
    #
    # Every write method pairs `route == as.character(i)` with
    # `self$tables[[as.character(i)]]`. One coercion names the child and selects
    # its rows, so a row cannot reach a child that its value does not name.
    #
    # `[[` returns NULL for a column that `newdata` does not carry, so the
    # earlier check read `sum(!NULL %in% self$partitions)`, which is 0, and it
    # passed. `drop_all_rows_and_then_upsert_data()` then computed
    # `NULL == i`, which is `logical(0)`, took zero rows for every partition,
    # dropped all of them and wrote nothing back.
    #
    # A list column passed the same check for a second reason. `setdiff()`
    # converts a list to character, so `setdiff(list("a"), c("a", "b"))` is
    # empty. `drop_all_rows_and_then_upsert_data()` then dropped every partition
    # and died inside csdb on `is.infinite(get(i))`, which has no list method. A
    # three-partition table lost every row, measured on 2026-08-14. The
    # `is.atomic()` check below rejects that column before the first drop.
    #
    # A zero-row `newdata` that carries the partition column is accepted. That
    # is how a caller clears every partition.
    #
    # Every caller runs this AFTER the `value_generator_partition` branch. A
    # generated partition column is absent until that branch adds it.
    check_for_correct_partitions_in_data = function(newdata) {
      if (!is.data.frame(newdata)) {
        stop(
          "newdata must be a data.frame. Cannot route a ",
          class(newdata)[1],
          "."
        )
      }
      if (!self$column_name_partition %in% names(newdata)) {
        stop(
          "newdata has no partition column. Expected a column named '",
          self$column_name_partition,
          "'."
        )
      }
      values <- newdata[[self$column_name_partition]]
      # A factor is atomic, so this accepts one. `as.character()` on a factor
      # gives its labels, which is what `==` against a factor already compared.
      if (!is.atomic(values)) {
        stop(
          "The partition column '",
          self$column_name_partition,
          "' must be atomic. Cannot route a ",
          class(values)[1],
          " column."
        )
      }
      route <- as.character(values)
      if (any(is.na(route))) {
        stop(
          "The partition column '",
          self$column_name_partition,
          "' holds NA."
        )
      }
      unknown <- setdiff(route, as.character(self$partitions))
      if (length(unknown) > 0) {
        stop(
          "The partition column '",
          self$column_name_partition,
          "' holds values that no partition covers: ",
          paste0(unknown, collapse = ", "),
          "."
        )
      }
      invisible(route)
    }
  )
)
