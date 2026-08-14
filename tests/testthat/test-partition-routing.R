# `DBPartitionedTableExtended_v9$drop_all_rows_and_then_upsert_data()` MUST send
# each row to the partition that its `part` column names. This file asserts that
# routing.
#
# The method read `self[[self$column_name_partition]]` before this fix. `self` is
# the R6 object, and it holds no field with that name. `[[` on an environment
# returns NULL for a name it does not hold. `NULL == "a"` returns `logical(0)`.
# A data.table indexed by `logical(0)` holds zero rows. Each partition therefore
# received `drop_all_rows()` and then an empty upsert. The table lost every row,
# with no error and no warning.
#
# The fixtures below repeat `tests/testthat/test-shared-connection.R`. testthat
# sources each test file into its own environment. A helper in one test file is
# not visible in another.

# A SQLite dbconfig for the "anon" access, plus the four configuration tables.
#
# `.local_envir = envir` is mandatory, not decoration. withr ties the deferred
# cleanup to that environment. The default is this function's own frame. The
# temporary directory would then be deleted as soon as the function returns.
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

  # DBTableExtended_v9$insert_data() writes to
  # config$tables$config_tables_last_updated. The configuration tables must
  # therefore exist before the first insert.
  reload_db_config()
  withr::defer(for (tab in config$tables) tab$disconnect(), envir = envir)

  config$dbconfigs$anon
}

# Three partitions, which is the smallest count that separates a routing defect
# from a swap of two partitions.
#
# `keys` is `c("x", "part")` because an upsert needs a key. csdb omits the
# PRIMARY KEY clause when `keys` is empty, and the upsert then fails.
new_routing_table <- function(dbconfig, table_name_base = "anon_routing") {
  DBPartitionedTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name_base = table_name_base,
    table_name_partitions = c("a", "b", "c"),
    column_name_partition = "part",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = c("x", "part"),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
}

# One row per partition, written through the already-correct insert_data().
old_rows <- function() {
  data.table::data.table(
    x = c("old_a", "old_b", "old_c"),
    n = c(91L, 92L, 93L),
    part = c("a", "b", "c")
  )
}

# Six rows over three partitions, in the counts 2, 1 and 3. The counts differ per
# partition, so a row that reaches the wrong partition changes a count.
new_rows <- function() {
  data.table::data.table(
    x = c("new_a1", "new_a2", "new_b1", "new_c1", "new_c2", "new_c3"),
    n = c(1L, 2L, 3L, 4L, 5L, 6L),
    part = c("a", "a", "b", "c", "c", "c")
  )
}

# The rows of one partition, ordered by n so the comparisons below are stable.
collect_partition <- function(pt, i) {
  out <- dplyr::collect(pt$tables[[i]]$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "n")
  out[]
}

test_that("upsert sends every row to the partition its part column names", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_routing_table(dbconfig)
  withr::defer(pt$disconnect())

  # Populate every partition first. These four assertions separate "the rows
  # went to the wrong partition" from "no partition ever held a row".
  suppressMessages(pt$insert_data(old_rows()))
  expect_equal(nrow(collect_partition(pt, "a")), 1L)
  expect_equal(nrow(collect_partition(pt, "b")), 1L)
  expect_equal(nrow(collect_partition(pt, "c")), 1L)

  suppressMessages(pt$drop_all_rows_and_then_upsert_data(new_rows()))

  # nrow() comes first in each block. A routing defect empties the partition, and
  # the reported mismatch then names the row count directly.
  pa <- collect_partition(pt, "a")
  expect_equal(nrow(pa), 2L)
  expect_equal(pa$x, c("new_a1", "new_a2"))
  expect_equal(pa$n, c(1L, 2L))
  expect_equal(pa$part, c("a", "a"))

  pb <- collect_partition(pt, "b")
  expect_equal(nrow(pb), 1L)
  expect_equal(pb$x, "new_b1")
  expect_equal(pb$n, 3L)
  expect_equal(pb$part, "b")

  pc <- collect_partition(pt, "c")
  expect_equal(nrow(pc), 3L)
  expect_equal(pc$x, c("new_c1", "new_c2", "new_c3"))
  expect_equal(pc$n, c(4L, 5L, 6L))
  expect_equal(pc$part, c("c", "c", "c"))

  # The three partitions together hold the six rows, and nothing else.
  all_rows <- data.table::rbindlist(list(pa, pb, pc))
  expect_equal(nrow(all_rows), 6L)
  expect_equal(sort(all_rows$x), sort(new_rows()$x))

  # The drop half of the method ran. No row of old_rows() survives.
  expect_false(any(grepl("^old_", all_rows$x)))
})

test_that("upsert and insert write the same rows to the same partitions", {
  dbconfig <- local_sqlite_dbconfig()
  pt_upsert <- new_routing_table(dbconfig, "anon_routing_upsert")
  pt_insert <- new_routing_table(dbconfig, "anon_routing_insert")
  withr::defer(pt_upsert$disconnect())
  withr::defer(pt_insert$disconnect())

  # drop_all_rows_and_then_insert_data() is the sibling that already routed
  # correctly. It is the reference for the method under test.
  suppressMessages(pt_upsert$drop_all_rows_and_then_upsert_data(new_rows()))
  suppressMessages(pt_insert$drop_all_rows_and_then_insert_data(new_rows()))

  # auto_last_updated_datetime differs between the two writes, so the comparison
  # is column by column rather than whole table.
  for (i in c("a", "b", "c")) {
    u <- collect_partition(pt_upsert, i)
    v <- collect_partition(pt_insert, i)
    expect_equal(nrow(u), nrow(v))
    expect_equal(u$x, v$x)
    expect_equal(u$n, v$n)
    expect_equal(u$part, v$part)
  }
})

test_that("upsert empties a partition that newdata does not name", {
  # The partition "a" assertion goes red under the routing defect. The partition
  # "b" and "c" assertions do not, because that defect empties every partition.
  # They pin the replace semantic: the method loops over every partition, not
  # only over the partitions that newdata names.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_routing_table(dbconfig)
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(old_rows()))

  suppressMessages(pt$drop_all_rows_and_then_upsert_data(
    data.table::data.table(x = "new_a1", n = 1L, part = "a")
  ))

  pa <- collect_partition(pt, "a")
  expect_equal(nrow(pa), 1L)
  expect_equal(pa$x, "new_a1")
  expect_equal(pa$n, 1L)

  expect_equal(nrow(collect_partition(pt, "b")), 0L)
  expect_equal(nrow(collect_partition(pt, "c")), 0L)
})
