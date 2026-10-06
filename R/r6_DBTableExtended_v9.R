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
    #
    # Each method below writes at most ONE config_tables_last_updated row.
    # The csdb method it calls can call other methods of this class, for
    # example drop_all_rows() and then upsert_data(). Those calls run inside
    # private$run_suppressed(), so they write no row. The method then writes
    # its one row when update_last_updated is TRUE.
    # DBPartitionedTableExtended_v9 passes FALSE and records every child in
    # one upsert instead.
    insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      private$run_suppressed(function() {
        return(super$insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose, load_timeout = load_timeout))
      })
      return(private$write_last_updated(update_last_updated))
    },
    upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      private$run_suppressed(function() {
        return(super$upsert_data(newdata, drop_indexes, verbose, load_timeout = load_timeout))
      })
      return(private$write_last_updated(update_last_updated))
    },
    drop_all_rows = function(update_last_updated = TRUE){
      private$run_suppressed(function() super$drop_all_rows())
      return(private$write_last_updated(update_last_updated))
    },
    drop_rows_where = function(condition, update_last_updated = TRUE){
      private$run_suppressed(function() super$drop_rows_where(condition))
      return(private$write_last_updated(update_last_updated))
    },
    keep_rows_where = function(condition, update_last_updated = TRUE){
      private$run_suppressed(function() super$keep_rows_where(condition))
      return(private$write_last_updated(update_last_updated))
    },
    drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      private$run_suppressed(function() {
        return(super$drop_all_rows_and_then_upsert_data(newdata, drop_indexes, verbose, load_timeout = load_timeout))
      })
      return(private$write_last_updated(update_last_updated))
    },
    drop_all_rows_and_then_insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      private$run_suppressed(function() {
        return(super$drop_all_rows_and_then_insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose, load_timeout = load_timeout))
      })
      return(private$write_last_updated(update_last_updated))
    }
  ),
  private = list(
    # The number of enclosing calls that suppress last-updated writes. It is
    # a counter, so a nested call cannot clear the suppression of the call
    # around it.
    suppress_last_updated = 0L,
    # on.exit() restores the counter, so an error cannot leave it raised.
    run_suppressed = function(fn) {
      private$suppress_last_updated <- private$suppress_last_updated + 1L
      on.exit(
        private$suppress_last_updated <- private$suppress_last_updated - 1L,
        add = TRUE
      )
      fn()
      return(invisible(NULL))
    },
    write_last_updated = function(update_last_updated) {
      if (update_last_updated && private$suppress_last_updated == 0L) {
        return(update_config_tables_last_updated(table_name = self$table_name))
      }
      return(invisible(NULL))
    }
  )
)
