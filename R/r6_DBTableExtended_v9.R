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
      run_check_live_name(table_name)
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
    #
    # During a task run, every write to a table in the task's `tables` goes to
    # the staging table instead, and a destructive write is recorded as an
    # operation. The publish at the end of the run applies both to this table.
    # See R/run_staging.R. Reads are not staged: tbl() and nrow() read this
    # table, and staged_tbl() reads the staged rows.
    insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      return(run_or_live(
        self$table_name,
        staged = function() private$stage_rows(newdata, isTRUE(confirm_insert_via_nrow), load_timeout),
        live = function() {
          return(private$write_live(function() {
            return(super$insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose, load_timeout = load_timeout))
          }, update_last_updated))
        }
      ))
    },
    upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE){
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      return(run_or_live(
        self$table_name,
        staged = function() private$stage_rows(newdata, TRUE, load_timeout),
        live = function() {
          return(private$write_live(function() {
            return(super$upsert_data(newdata, drop_indexes, verbose, load_timeout = load_timeout))
          }, update_last_updated))
        }
      ))
    },
    drop_all_rows = function(update_last_updated = TRUE){
      return(run_or_live(
        self$table_name,
        staged = function() private$stage_operation("empty"),
        live = function() private$write_live(function() super$drop_all_rows(), update_last_updated)
      ))
    },
    drop_rows_where = function(condition, update_last_updated = TRUE){
      return(run_or_live(
        self$table_name,
        staged = function() private$stage_operation("delete", condition),
        live = function() private$write_live(function() super$drop_rows_where(condition), update_last_updated)
      ))
    },
    keep_rows_where = function(condition, update_last_updated = TRUE){
      return(run_or_live(
        self$table_name,
        staged = function() private$stage_operation("keep", condition),
        live = function() private$write_live(function() super$keep_rows_where(condition), update_last_updated)
      ))
    },
    drop_all_rows_and_then_upsert_data = function(newdata, drop_indexes = names(self$indexes), verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      return(run_or_live(
        self$table_name,
        staged = function() private$stage_replace(newdata, TRUE, load_timeout, "drop_all_rows_and_then_upsert_data"),
        live = function() {
          return(private$write_live(function() {
            return(super$drop_all_rows_and_then_upsert_data(newdata, drop_indexes, verbose, load_timeout = load_timeout))
          }, update_last_updated))
        }
      ))
    },
    drop_all_rows_and_then_insert_data = function(newdata, confirm_insert_via_nrow = FALSE, verbose = TRUE, load_timeout = 3600, update_last_updated = TRUE) {
      newdata[, auto_last_updated_datetime := cstime::now_c()]
      return(run_or_live(
        self$table_name,
        staged = function() private$stage_replace(newdata, isTRUE(confirm_insert_via_nrow), load_timeout, "drop_all_rows_and_then_insert_data"),
        live = function() {
          return(private$write_live(function() {
            return(super$drop_all_rows_and_then_insert_data(newdata, confirm_insert_via_nrow = confirm_insert_via_nrow, verbose, load_timeout = load_timeout))
          }, update_last_updated))
        }
      ))
    },
    # csdb::DBTable_v9$replace_all_rows() writes no last-updated row, and
    # neither does the staged branch. The publish writes it.
    replace_all_rows = function(newdata) {
      return(run_or_live(
        self$table_name,
        staged = function() {
          run_check_replace_columns(newdata, self$field_types, self$table_name)
          private$stage_operation("empty")
          return(private$stage_rows(newdata, FALSE, 3600))
        },
        live = function() super$replace_all_rows(newdata)
      ))
    },
    # The rows that the task run in progress staged for this table, as a lazy
    # dplyr table. The publish at the end of the run moves them into this
    # table. It stops outside a task run and while the run builds its plans.
    staged_tbl = function() {
      return(run_staged_tbl(private$staging(), self$table_name, names(self$field_types)))
    }
  ),
  private = list(
    staging_object = NULL,
    # The staging table, built at its first use. See R/run_staging.R.
    staging = function() {
      if (is.null(private$staging_object)) {
        private$staging_object <- run_new_staging_table(
          dbconfig = self$dbconfig,
          live_name = self$table_name,
          field_types = self$field_types,
          keys = self$keys,
          dbconnection = self$dbconnection
        )
      }
      return(private$staging_object)
    },
    stage_rows = function(newdata, upsert, load_timeout) {
      private$lazy_creation_of_table()
      return(run_stage_rows(
        private$staging(),
        newdata,
        upsert = upsert,
        validator = self$validator_field_contents,
        label = self$table_name,
        load_timeout = load_timeout
      ))
    },
    stage_operation = function(operation, condition = NULL) {
      private$lazy_creation_of_table()
      return(run_stage_operation(
        private$staging(),
        self$dbconnection,
        self$table_name,
        operation,
        condition
      ))
    },
    # A drop-and-write method: the checks that csdb runs before its drop, the
    # recorded empty, then the staged rows. run_stage_rows() stages nothing
    # for zero rows.
    stage_replace = function(newdata, upsert, load_timeout, method) {
      private$check_newdata_before_drop_all_rows(newdata = newdata, method = method)
      private$stage_operation("empty")
      return(private$stage_rows(newdata, upsert, load_timeout))
    },
    # The write to this table, outside a task run, with its one last-updated
    # row.
    write_live = function(fn, update_last_updated) {
      private$run_suppressed(fn)
      return(private$write_last_updated(update_last_updated))
    },
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
