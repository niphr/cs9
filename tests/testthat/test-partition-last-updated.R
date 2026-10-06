# `drop_all_rows()`, `drop_rows_where()` and `keep_rows_where()` on
# `DBPartitionedTableExtended_v9` each make ONE upsert into
# config_tables_last_updated. Before, each child made its own upsert, so a
# table with 106 partitions made 106. The row set is unchanged: one row per
# completed child, under the same `table_name`.
#
# A failure inside a child stops the loop. The children that completed before
# it are still recorded, in one upsert, and the error still reaches the caller.
#
# The fixtures repeat `tests/testthat/test-partition-safety.R`. testthat sources
# each test file into its own environment, so a helper in one file is not
# visible in another.

local_sqlite_dbconfig <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  withr::local_envvar(
    c(
      CS9_AUTO = "0",
      CS9_PATH = d,
      CS9_DBCONFIG_ACCESS = "config/anon",
      CS9_DBCONFIG_DRIVER = "SQLite",
      CS9_DBCONFIG_DB_CONFIG = file.path(d, "config.sqlite"),
      CS9_DBCONFIG_DB_ANON = file.path(d, "anon.sqlite"),
      CS9_DBCONFIG_PORT = NA,
      CS9_DBCONFIG_SERVER = NA,
      CS9_DBCONFIG_USER = NA,
      CS9_DBCONFIG_PASSWORD = NA,
      CS9_DBCONFIG_SCHEMA_CONFIG = NA,
      CS9_DBCONFIG_SCHEMA_ANON = NA
    ),
    .local_envir = envir
  )

  reload_db_config()
  withr::defer(
    for (tab in config$tables) {
      tab$disconnect()
    },
    envir = envir
  )

  return(config$dbconfigs$anon)
}

# Five partitions, each with two rows. Every child table then exists before the
# method under test runs.
new_five_partition_table <- function(dbconfig) {
  pt <- DBPartitionedTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name_base = "lastupd",
    table_name_partitions = c("a", "b", "c", "d", "e"),
    column_name_partition = "part",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = c("x", "part"),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  rows <- data.table::data.table(
    x = paste0("r", 1:10),
    n = 1:10,
    part = rep(c("a", "b", "c", "d", "e"), each = 2)
  )
  suppressMessages(pt$insert_data(rows))
  return(pt)
}

# Count every call to config_tables_last_updated$upsert_data(). The wrapper
# calls the original, so the rows still reach the database.
local_count_last_updated_upserts <- function(envir = parent.frame()) {
  tab <- config$tables$config_tables_last_updated
  original <- tab$upsert_data
  counter <- new.env()
  counter$n <- 0L
  unlockBinding("upsert_data", tab)
  assign(
    "upsert_data",
    function(...) {
      counter$n <- counter$n + 1L
      original(...)
    },
    envir = tab
  )
  withr::defer(
    {
      assign("upsert_data", original, envir = tab)
      lockBinding("upsert_data", tab)
    },
    envir = envir
  )
  return(counter)
}

read_last_updated_names <- function() {
  d <- dplyr::collect(config$tables$config_tables_last_updated$tbl())
  return(sort(d$table_name))
}

clear_last_updated <- function() {
  suppressMessages(config$tables$config_tables_last_updated$drop_all_rows())
  return(invisible(NULL))
}

# The three partitioned calls, and the per-child call that the old code made.
partitioned_calls <- list(
  drop_all_rows = list(
    parent = function(pt) pt$drop_all_rows(),
    child = function(child) child$drop_all_rows()
  ),
  drop_rows_where = list(
    parent = function(pt) pt$drop_rows_where("n > 100"),
    child = function(child) child$drop_rows_where("n > 100")
  ),
  keep_rows_where = list(
    parent = function(pt) pt$keep_rows_where("n > 0"),
    child = function(child) child$keep_rows_where("n > 0")
  )
)

for (method in names(partitioned_calls)) {
  test_that(
    paste0(method, "() makes one upsert with one row per partition"),
    {
      dbconfig <- local_sqlite_dbconfig()
      pt <- new_five_partition_table(dbconfig)
      withr::defer(pt$disconnect())
      counter <- local_count_last_updated_upserts()

      # The old path: every child records itself.
      clear_last_updated()
      counter$n <- 0L
      for (child in pt$tables) {
        suppressMessages(partitioned_calls[[method]]$child(child))
      }
      expect_equal(counter$n, 5L)
      old_names <- read_last_updated_names()
      expect_length(old_names, 5L)

      clear_last_updated()
      counter$n <- 0L
      suppressMessages(partitioned_calls[[method]]$parent(pt))
      expect_equal(counter$n, 1L)
      expect_equal(read_last_updated_names(), old_names)
    }
  )
}

# The partition order is random, so the failure is set on the third CALL, not
# on a named partition. Each child is wrapped in a stand-in that forwards the
# method under test to the real child.
for (method in names(partitioned_calls)) {
  test_that(
    paste0(method, "() records the partitions completed before a failure"),
    {
      dbconfig <- local_sqlite_dbconfig()
      pt <- new_five_partition_table(dbconfig)
      withr::defer(pt$disconnect())
      counter <- local_count_last_updated_upserts()
      clear_last_updated()
      counter$n <- 0L

      calls <- new.env()
      calls$n <- 0L
      calls$completed <- character(0)
      for (nm in names(pt$tables)) {
        local({
          real <- pt$tables[[nm]]
          stand_in <- list(table_name = real$table_name)
          stand_in[[method]] <- function(...) {
            calls$n <- calls$n + 1L
            if (calls$n == 3L) {
              stop("cs9test partition failure")
            }
            real[[method]](...)
            calls$completed <- c(calls$completed, real$table_name)
          }
          pt$tables[[nm]] <- stand_in
        })
      }

      expect_error(
        suppressMessages(partitioned_calls[[method]]$parent(pt)),
        "cs9test partition failure"
      )
      expect_equal(calls$n, 3L)
      expect_length(calls$completed, 2L)
      expect_equal(counter$n, 1L)
      expect_equal(read_last_updated_names(), sort(calls$completed))
    }
  )
}
