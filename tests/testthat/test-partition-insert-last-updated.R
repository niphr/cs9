# Each partitioned `insert_data()`, `upsert_data()`,
# `drop_all_rows_and_then_insert_data()` and
# `drop_all_rows_and_then_upsert_data()` makes ONE upsert into
# config_tables_last_updated, with one row per completed child. A single
# `DBTableExtended_v9` call writes one row. Before 26.10.6, one child
# `drop_all_rows_and_then_upsert_data()` wrote 3, because csdb calls
# `drop_all_rows()` and `upsert_data()` on the same object.
#
# The fixtures repeat `tests/testthat/test-partition-last-updated.R`. testthat
# sources each test file into its own environment, so a helper in one file is
# not visible in another.

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

five_partitions <- c("a", "b", "c", "d", "e")

# Five partitions, each with two rows. Every child table then exists before the
# method under test runs.
new_five_partition_table <- function(dbconfig) {
  pt <- DBPartitionedTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name_base = "lastupdins",
    table_name_partitions = five_partitions,
    column_name_partition = "part",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = c("x", "part"),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  suppressMessages(pt$insert_data(new_rows(1:10, five_partitions)))
  return(pt)
}

# Two rows per partition, with keys r<from>, r<from + 1>, ...
new_rows <- function(n, partitions) {
  return(data.table::data.table(
    x = paste0("r", n),
    n = n,
    part = rep(partitions, each = 2)
  ))
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

child_names <- function(pt, partitions) {
  return(sort(vapply(
    partitions,
    function(p) pt$tables[[p]]$table_name,
    character(1),
    USE.NAMES = FALSE
  )))
}

# newdata covers partitions a, c and e. `expected` is the partitions the old
# path recorded: the ones present in newdata for insert and upsert, and all 5
# for the drop methods, because they empty b and d.
partitioned_load_calls <- list(
  insert_data = list(
    call = function(pt, d) pt$insert_data(d),
    rows = 11:16,
    expected = c("a", "c", "e")
  ),
  upsert_data = list(
    call = function(pt, d) pt$upsert_data(d),
    rows = 1:6,
    expected = c("a", "c", "e")
  ),
  drop_all_rows_and_then_insert_data = list(
    call = function(pt, d) pt$drop_all_rows_and_then_insert_data(d),
    rows = 11:16,
    expected = five_partitions
  ),
  drop_all_rows_and_then_upsert_data = list(
    call = function(pt, d) pt$drop_all_rows_and_then_upsert_data(d),
    rows = 11:16,
    expected = five_partitions
  )
)

for (method in names(partitioned_load_calls)) {
  test_that(
    paste0("partitioned ", method, "() makes one upsert"),
    {
      spec <- partitioned_load_calls[[method]]
      dbconfig <- local_sqlite_dbconfig()
      pt <- new_five_partition_table(dbconfig)
      withr::defer(pt$disconnect())
      counter <- local_count_last_updated_upserts()
      clear_last_updated()
      counter$n <- 0L

      d <- new_rows(spec$rows, c("a", "c", "e"))
      suppressMessages(spec$call(pt, d))

      expect_equal(counter$n, 1L)
      expect_equal(read_last_updated_names(), child_names(pt, spec$expected))
    }
  )
}

new_single_table <- function(dbconfig) {
  tab <- DBTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name = "lastupdsingle",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = c("x"),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  return(tab)
}

single_rows <- function(n) {
  return(data.table::data.table(x = paste0("s", n), n = n))
}

test_that("single drop_all_rows_and_then_*_data() writes one row", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_single_table(dbconfig)
  withr::defer(tab$disconnect())
  suppressMessages(tab$insert_data(single_rows(1:3)))
  counter <- local_count_last_updated_upserts()

  counter$n <- 0L
  suppressMessages(tab$drop_all_rows_and_then_upsert_data(single_rows(4:5)))
  expect_equal(counter$n, 1L)

  counter$n <- 0L
  suppressMessages(tab$drop_all_rows_and_then_insert_data(single_rows(6:7)))
  expect_equal(counter$n, 1L)
  expect_equal(tab$nrow(use_count = TRUE), 2L)

  # An empty newdata empties the table. csdb then skips the upsert.
  counter$n <- 0L
  suppressMessages(tab$drop_all_rows_and_then_upsert_data(single_rows(
    integer(0)
  )))
  expect_equal(counter$n, 1L)
  expect_equal(tab$nrow(use_count = TRUE), 0L)
})

# SQLite has no duplicate-key load error, so the row-count check starts the
# fallback. The first nrow() call returns 0, which is fewer rows than newdata.
test_that("single insert_data() that falls back to upsert writes one row", {
  dbconfig <- local_sqlite_dbconfig()
  tab <- new_single_table(dbconfig)
  withr::defer(tab$disconnect())
  suppressMessages(tab$insert_data(single_rows(1:3)))
  counter <- local_count_last_updated_upserts()

  real_nrow <- tab$nrow
  nrow_calls <- new.env()
  nrow_calls$n <- 0L
  unlockBinding("nrow", tab)
  assign(
    "nrow",
    function(...) {
      nrow_calls$n <- nrow_calls$n + 1L
      if (nrow_calls$n == 1L) {
        return(0L)
      }
      real_nrow(...)
    },
    envir = tab
  )

  counter$n <- 0L
  expect_message(
    tab$insert_data(single_rows(4:5), confirm_insert_via_nrow = TRUE),
    "Now trying upsert"
  )
  expect_equal(nrow_calls$n, 2L)
  expect_equal(counter$n, 1L)
})

# The partition order is random, so the failure is set on the third child the
# method reaches, not on a named partition. The failure is raised inside the
# child's csdb call, by the child's own validator.
for (method in names(partitioned_load_calls)) {
  test_that(
    paste0(
      "partitioned ",
      method,
      "() records the children completed before a failure"
    ),
    {
      spec <- partitioned_load_calls[[method]]
      dbconfig <- local_sqlite_dbconfig()
      pt <- new_five_partition_table(dbconfig)
      withr::defer(pt$disconnect())
      counter <- local_count_last_updated_upserts()
      clear_last_updated()
      counter$n <- 0L

      reached <- new.env()
      reached$names <- character(0)
      for (nm in names(pt$tables)) {
        local({
          child <- pt$tables[[nm]]
          child$validator_field_contents <- function(data) {
            if (!child$table_name %in% reached$names) {
              reached$names <- c(reached$names, child$table_name)
            }
            if (length(reached$names) == 3L) {
              stop("cs9test partition failure")
            }
            return(TRUE)
          }
        })
      }

      d <- new_rows(seq_len(10) + 20L, five_partitions)
      expect_error(
        suppressMessages(spec$call(pt, d)),
        "cs9test partition failure"
      )
      expect_length(reached$names, 3L)
      expect_equal(counter$n, 1L)
      expect_equal(read_last_updated_names(), sort(reached$names[1:2]))

      # The failed child writes its own row again afterwards.
      names_by_part <- vapply(pt$tables, function(t) t$table_name, "")
      part <- names(names_by_part)[names_by_part == reached$names[3]]
      failed <- pt$tables[[part]]
      failed$validator_field_contents <- csdb::validator_field_contents_blank
      counter$n <- 0L
      suppressMessages(failed$insert_data(new_rows(41:42, part)))
      expect_equal(counter$n, 1L)
    }
  )
}
