DBPartitionedTableExtended_v9 <- R6::R6Class(
  "DBPartitionedTableExtended_v9",
  public = list(
    tables = list(),
    partitions = c(),
    column_name_partition = "",
    value_generator_partition = NULL,
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
      self$partitions <- table_name_partitions

      force(column_name_partition)
      self$column_name_partition <- column_name_partition

      force(value_generator_partition)
      self$value_generator_partition <- value_generator_partition

      # ensure that partition name is last in dataset
      if (column_name_partition %in% names(field_types)) {
        field_types <- field_types[!names(field_types) == column_name_partition]
      }
      field_types <- c(field_types, "TEXT")
      names(field_types)[length(field_types)] <- column_name_partition

      self$tables <- vector("list", length(self$partitions))
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
          validator_field_contents = validator_field_contents
        )
        self$tables[[i]] <- dbtable
      }
    },
    disconnect = function() {
      for (i in self$partitions_randomized) {
        self$tables[[i]]$disconnect()
      }
    },
    insert_data = function(
      newdata,
      confirm_insert_via_nrow = FALSE,
      verbose = TRUE
    ) {
      if (!is.null(self$value_generator_partition)) {
        part <- do.call(self$value_generator_partition, newdata)
        newdata[, .(self$column_name_partition) := part]
      }
      private$check_for_correct_partitions_in_data(newdata)

      partitions_in_use <- unique(newdata[[self$column_name_partition]])
      partitions_in_use <- sample(partitions_in_use, length(partitions_in_use)) # randomize order
      for (i in partitions_in_use) {
        index <- newdata[[self$column_name_partition]] == i
        self$tables[[i]]$insert_data(
          newdata[index, ],
          confirm_insert_via_nrow,
          verbose
        )
      }
    },
    upsert_data = function(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE
    ) {
      if (!is.null(self$value_generator_partition)) {
        part <- do.call(self$value_generator_partition, newdata)
        newdata[, .(self$column_name_partition) := part]
      }
      private$check_for_correct_partitions_in_data(newdata)

      partitions_in_use <- unique(newdata[[self$column_name_partition]])
      partitions_in_use <- unique(newdata[[self$column_name_partition]])
      for (i in partitions_in_use) {
        index <- newdata[[self$column_name_partition]] == i
        self$tables[[i]]$upsert_data(newdata[index, ], drop_indexes, verbose)
      }
    },
    drop_all_rows = function() {
      for (i in self$partitions_randomized) {
        self$tables[[i]]$drop_all_rows()
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
        self$tables[[i]]$drop_rows_where(condition)
      }
    },
    keep_rows_where = function(condition, verbose = FALSE) {
      for (i in self$partitions_randomized) {
        self$tables[[i]]$keep_rows_where(condition)
      }
    },
    drop_all_rows_and_then_upsert_data = function(
      newdata,
      drop_indexes = names(self$indexes),
      verbose = TRUE
    ) {
      if (!is.null(self$value_generator_partition)) {
        part <- do.call(self$value_generator_partition, newdata)
        newdata[, .(self$column_name_partition) := part]
      }
      private$check_for_correct_partitions_in_data(newdata)

      for (i in self$partitions_randomized) {
        index <- self[[self$column_name_partition]] == i
        self$tables[[i]]$drop_all_rows_and_then_upsert_data(
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
        newdata[, .(self$column_name_partition) := part]
      }
      private$check_for_correct_partitions_in_data(newdata)

      for (i in self$partitions_randomized) {
        index <- newdata[[self$column_name_partition]] == i
        self$tables[[i]]$drop_all_rows_and_then_insert_data(
          newdata[index, ],
          confirm_insert_via_nrow,
          verbose
        )
      }
    },
    remove_table = function() {
      for (i in self$partitions_randomized) {
        self$tables[[i]]$remove_table()
      }
    },
    drop_indexes = function() {
      for (i in self$partitions_randomized) {
        self$tables[[i]]$drop_indexes()
      }
    },
    add_indexes = function() {
      for (i in self$partitions_randomized) {
        self$tables[[i]]$add_indexes()
      }
    },
    confirm_indexes = function() {
      for (i in self$partitions_randomized) {
        self$tables[[i]]$confirm_indexes()
      }
    },
    nrow = function(collapse = TRUE) {
      table_rows <- self$tables[[1]]$dbconnection$autoconnection %>%
        csdb::get_table_names_and_info()
      table_rows[, keep := FALSE]
      for (i in self$partitions_randomized) {
        table_rows[table_name == self$tables[[i]]$table_name, keep := TRUE]
      }
      table_rows <- table_rows[
        keep == T,
        .(
          table_name,
          nrow
        )
      ]
      if (collapse) {
        return(sum(table_rows$nrow))
      }

      data.table::shouldPrint(table_rows)
      return(table_rows)
    },
    info = function(collapse = FALSE) {
      table_rows <- self$tables[[1]]$dbconnection$autoconnection %>%
        csdb::get_table_names_and_info()
      table_rows[, keep := FALSE]
      for (i in self$partitions_randomized) {
        table_rows[table_name == self$tables[[i]]$table_name, keep := TRUE]
      }
      table_rows <- table_rows[keep == T]
      table_rows[, keep := NULL]

      if (collapse) {
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
    partitions_randomized = function() {
      sample(self$partitions, length(self$partitions))
    }
  ),
  private = list(
    check_for_correct_partitions_in_data = function(newdata) {
      partitions_in_use <- unique(newdata[[self$column_name_partition]])
      if (sum(!partitions_in_use %in% self$partitions)) {
        stop(
          "Some partitions exist in the data that do not exist in the partion"
        )
      }
    }
  )
)
