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
      super$initialize(
        dbconfig,
        table_name,
        field_types,
        keys,
        indexes,
        validator_field_types,
        validator_field_contents,
        dbconnection
      )
    },
    insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose)
      update_config_tables_last_updated(table_name = self$table_name)
    },
    upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$upsert_data(newdata, drop_indexes, verbose)
      update_config_tables_last_updated(table_name = self$table_name)
    },
    drop_all_rows = function(){
      super$drop_all_rows()
      update_config_tables_last_updated(table_name = self$table_name)
    },
    drop_rows_where = function(condition){
      super$drop_rows_where(condition)
      update_config_tables_last_updated(table_name = self$table_name)
    },
    keep_rows_where = function(condition){
      super$keep_rows_where(condition)
      update_config_tables_last_updated(table_name = self$table_name)
    },
    drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$drop_all_rows_and_then_upsert_data(newdata, drop_indexes, verbose)
      update_config_tables_last_updated(table_name = self$table_name)
    },
    drop_all_rows_and_then_insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      super$drop_all_rows_and_then_insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose)
      update_config_tables_last_updated(table_name = self$table_name)
    }
  )
)
