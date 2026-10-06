DBTableExtended_v9 <- R6::R6Class(
  "DBTableExtended_v9",
  inherit = csdb::DBTable_v9,
  public = list(
    initialize = function(
      dbconfig,
      table_name,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = validator_field_types_blank,
      validator_field_contents = validator_field_contents_blank,
      dbconnection = NULL
    ) {
      field_types <- c(field_types, "DATETIME")
      names(field_types)[length(field_types)] <- "auto_last_updated_datetime"

      # csdb::DBTable_v9 takes dbconnection as its eighth and last argument.
      # A supplied connection is borrowed, so the child does not close it.
      return(super$initialize(
        dbconfig,
        table_name,
        field_types,
        keys,
        indexes,
        validator_field_types,
        validator_field_contents,
        dbconnection
      ))
    },
    # Every load method passes load_timeout to csdb. csdb::DBTable_v9 also
    # calls insert_data() and upsert_data() with it from its own methods.
    insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE, load_timeout = 3600){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose, load_timeout = load_timeout)
      return(update_config_tables_last_updated(table_name = self$table_name))
    },
    upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE, load_timeout = 3600){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$upsert_data(newdata, drop_indexes, verbose, load_timeout = load_timeout)
      return(update_config_tables_last_updated(table_name = self$table_name))
    },
    # update_last_updated = FALSE skips the config_tables_last_updated row.
    # DBPartitionedTableExtended_v9 passes FALSE and records every child in
    # one upsert instead.
    drop_all_rows = function(update_last_updated = TRUE){
      super$drop_all_rows()
      if (update_last_updated) {
        return(update_config_tables_last_updated(table_name = self$table_name))
      }
    },
    drop_rows_where = function(condition, update_last_updated = TRUE){
      super$drop_rows_where(condition)
      if (update_last_updated) {
        return(update_config_tables_last_updated(table_name = self$table_name))
      }
    },
    keep_rows_where = function(condition, update_last_updated = TRUE){
      super$keep_rows_where(condition)
      if (update_last_updated) {
        return(update_config_tables_last_updated(table_name = self$table_name))
      }
    },
    drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE, load_timeout = 3600) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$drop_all_rows_and_then_upsert_data(newdata, drop_indexes, verbose, load_timeout = load_timeout)
      return(update_config_tables_last_updated(table_name = self$table_name))
    },
    drop_all_rows_and_then_insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE, load_timeout = 3600) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$drop_all_rows_and_then_insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose, load_timeout = load_timeout)
      return(update_config_tables_last_updated(table_name = self$table_name))
    }
  )
)
