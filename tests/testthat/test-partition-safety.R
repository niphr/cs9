# Safety guards for `DBPartitionedTableExtended_v9`.
#
# Seven defects are pinned here. Each one destroyed data, broke a write, or made
# the object misreport its own state.
#
#  1. A `newdata` with no partition column passed validation. Both
#     `drop_all_rows_and_then_*` methods then dropped every partition and wrote
#     nothing back. The tests below assert that the rows SURVIVE. An error
#     alone does not prove that.
#  2. `newdata[, .(col) := part]` is a syntax error, so a table with a
#     `value_generator_partition` failed on its first write. The fix also takes
#     a copy, so a write leaves the caller's data.table alone.
#  3. `sample(x, length(x))` reads a length-1 numeric as a range.
#     `self$tables[[i]]` with a numeric `i` indexes the list by position.
#  4. `indexes` was a constructor argument that no field held.
#  5. A list partition column passed validation, because `setdiff()` converts a
#     list to character. Every partition was then dropped and the write died
#     inside csdb. A three-partition table lost every row.
#  6. The constructor accepted a partition set it cannot use: zero length, NA,
#     an empty string, or a duplicate.
#  7. `nrow(collapse = FALSE)` and `info()` knew the partition tag of each row
#     and discarded it. A caller then parsed the table name to get it back, and
#     the separator is not one string. The tests below never name a separator.
#
# The duplicated `partitions_in_use` line in `upsert_data()` has no test. It
# assigned the same value twice, so its removal changes nothing observable.
#
# Three tests carry no defect of their own and guard the compatibility
# contract of the new `partition` column. The column goes LAST, so `table_name`
# stays column 1 of `nrow(collapse = FALSE)` and `nrow` stays column 2.
# `info(collapse = FALSE)` appends the column and moves nothing else.
# `info(collapse = TRUE)` aggregates over every partition, so it drops it.
#
# Two tests carry no defect of their own and guard the routing contract.
# `check_for_correct_partitions_in_data()` returns the route vector, and the
# four write methods route with it. Validation and routing therefore cannot
# disagree. A factor column is the case an adversarial review said would
# diverge. It does not, and the test below keeps it that way.
#
# The fixtures repeat `tests/testthat/test-partition-routing.R`. testthat
# sources each test file into its own environment, so a helper in one file is
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

# `keys` is `c("x", "part")` because an upsert needs a key. csdb omits the
# PRIMARY KEY clause when `keys` is empty, and the upsert then fails.
new_safety_table <- function(
  dbconfig,
  table_name_base,
  partitions = c("a", "b", "c"),
  value_generator_partition = NULL,
  indexes = NULL
) {
  DBPartitionedTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name_base = table_name_base,
    table_name_partitions = partitions,
    column_name_partition = "part",
    value_generator_partition = value_generator_partition,
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = c("x", "part"),
    indexes = indexes,
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
}

# Two rows per partition. A partial wipe is then visible as a row count, and
# not only as an empty table.
sentinel_rows <- function() {
  data.table::data.table(
    x = c(
      "sentinel_a1",
      "sentinel_a2",
      "sentinel_b1",
      "sentinel_b2",
      "sentinel_c1",
      "sentinel_c2"
    ),
    n = c(11L, 12L, 21L, 22L, 31L, 32L),
    part = c("a", "a", "b", "b", "c", "c")
  )
}

# The rows of one partition, ordered by n so the comparisons below are stable.
# `i` is the character name of the child, never a number.
collect_partition <- function(pt, i) {
  out <- dplyr::collect(pt$tables[[i]]$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "n")
  out[]
}

# Every sentinel row is still in the partition its `part` column names.
expect_sentinels_intact <- function(pt) {
  s <- sentinel_rows()
  for (i in c("a", "b", "c")) {
    p <- collect_partition(pt, i)
    want <- s[s$part == i, ]
    expect_equal(nrow(p), 2L)
    expect_equal(p$x, want$x)
    expect_equal(p$n, want$n)
    expect_equal(p$part, want$part)
  }
}

test_that("upsert keeps every row when newdata has no partition column", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_upsert_nocol")
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(sentinel_rows()))
  expect_sentinels_intact(pt)

  # Nonempty, and it carries no `part` column.
  bad <- data.table::data.table(x = c("new1", "new2"), n = c(1L, 2L))
  expect_error(
    suppressMessages(pt$drop_all_rows_and_then_upsert_data(bad)),
    "Expected a column named 'part'",
    fixed = TRUE
  )

  # This is the point of the test. The error alone proves nothing.
  expect_sentinels_intact(pt)
})

test_that("insert keeps every row when newdata has no partition column", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_insert_nocol")
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(sentinel_rows()))
  expect_sentinels_intact(pt)

  bad <- data.table::data.table(x = c("new1", "new2"), n = c(1L, 2L))
  expect_error(
    suppressMessages(pt$drop_all_rows_and_then_insert_data(bad)),
    "Expected a column named 'part'",
    fixed = TRUE
  )

  expect_sentinels_intact(pt)
})

test_that("the guard names the partition values that no partition covers", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_unknown")
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(sentinel_rows()))

  bad <- data.table::data.table(x = "new1", n = 1L, part = "zz")
  expect_error(
    suppressMessages(pt$drop_all_rows_and_then_upsert_data(bad)),
    "holds values that no partition covers: zz",
    fixed = TRUE
  )

  expect_sentinels_intact(pt)
})

test_that("the guard rejects an NA in the partition column", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_na")
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(sentinel_rows()))

  bad <- data.table::data.table(
    x = c("new1", "new2"),
    n = c(1L, 2L),
    part = c("a", NA_character_)
  )
  expect_error(
    suppressMessages(pt$drop_all_rows_and_then_upsert_data(bad)),
    "The partition column 'part' holds NA.",
    fixed = TRUE
  )

  expect_sentinels_intact(pt)
})

test_that("the guard rejects a NULL newdata", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_null")
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(sentinel_rows()))

  expect_error(
    suppressMessages(pt$drop_all_rows_and_then_upsert_data(NULL)),
    "newdata must be a data.frame. Cannot route a NULL.",
    fixed = TRUE
  )

  expect_sentinels_intact(pt)
})

test_that("a zero-row newdata that carries the column clears every partition", {
  # Clearing every partition is legitimate. The guard must not break it.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_zero_row")
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(sentinel_rows()))
  expect_sentinels_intact(pt)

  empty <- data.table::data.table(
    x = character(0),
    n = integer(0),
    part = character(0)
  )
  expect_no_error(
    suppressMessages(pt$drop_all_rows_and_then_upsert_data(empty))
  )

  for (i in c("a", "b", "c")) {
    expect_equal(nrow(collect_partition(pt, i)), 0L)
  }
})

test_that("a value_generator_partition routes every row to the partition it names", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_generator",
    value_generator_partition = function(x, ...) substr(x, 1, 1)
  )
  withr::defer(pt$disconnect())

  d <- data.table::data.table(
    x = c("a1", "a2", "b1", "c1"),
    n = c(1L, 2L, 3L, 4L)
  )
  expect_no_error(suppressMessages(pt$insert_data(d)))

  pa <- collect_partition(pt, "a")
  expect_equal(nrow(pa), 2L)
  expect_equal(pa$x, c("a1", "a2"))
  expect_equal(pa$part, c("a", "a"))

  pb <- collect_partition(pt, "b")
  expect_equal(nrow(pb), 1L)
  expect_equal(pb$x, "b1")

  pc <- collect_partition(pt, "c")
  expect_equal(nrow(pc), 1L)
  expect_equal(pc$x, "c1")
})

test_that("a value_generator_partition write leaves the caller's data.table alone", {
  dbconfig <- local_sqlite_dbconfig()
  gen <- function(x, ...) substr(x, 1, 1)
  method_names <- c(
    "insert_data",
    "upsert_data",
    "drop_all_rows_and_then_upsert_data",
    "drop_all_rows_and_then_insert_data"
  )

  # One table per method, and one deferred loop. `withr::defer(pt$disconnect())`
  # inside the loop below would evaluate `pt` at exit, and would then close the
  # last table four times.
  pts <- list()
  for (i in seq_along(method_names)) {
    pts[[i]] <- new_safety_table(
      dbconfig,
      paste0("anon_safety_nocopy_", i),
      value_generator_partition = gen
    )
  }
  withr::defer(for (p in pts) p$disconnect())

  # The observed value carries the method name, so a failure names the method
  # that mutated its caller.
  after <- character(length(method_names))
  names(after) <- method_names
  for (i in seq_along(method_names)) {
    d <- data.table::data.table(x = c("a1", "b1"), n = c(1L, 2L))
    suppressMessages(pts[[i]][[method_names[i]]](d))
    after[i] <- paste0(names(d), collapse = ",")
  }

  expect_equal(
    after,
    c(
      insert_data = "x,n",
      upsert_data = "x,n",
      drop_all_rows_and_then_upsert_data = "x,n",
      drop_all_rows_and_then_insert_data = "x,n"
    )
  )
})

test_that("partitions_randomized returns the value of a single numeric partition", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_num_one", partitions = 9L)
  withr::defer(pt$disconnect())

  # `sample(9L, 1)` draws from 1:9, because it reads a length-1 numeric as a
  # range. Under seed 5 that draw is 2, measured on R 4.6.1 on 2026-08-14.
  withr::with_seed(5, expect_equal(pt$partitions_randomized, 9L))

  # Seed-free, so the assertion does not rest on the RNG stream staying put.
  expect_equal(unique(replicate(30, pt$partitions_randomized)), 9L)
})

test_that("a numeric partition names every child by its partition value", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_num_names",
    partitions = c(5L, 7L)
  )
  withr::defer(pt$disconnect())

  # `self$tables[[5L]] <- child` assigns to POSITION five. The list grows to
  # five elements, three of them NULL, and the child named "5" is one of them.
  expect_equal(length(pt$tables), 2L)
  expect_equal(names(pt$tables), c("5", "7"))
})

test_that("a numeric partition routes a row to the child its value names", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_num_route",
    partitions = c(5L, 7L)
  )
  withr::defer(pt$disconnect())

  d <- data.table::data.table(
    x = c("row5", "row7"),
    n = c(1L, 2L),
    part = c(5L, 7L)
  )
  suppressMessages(pt$insert_data(d))

  expect_equal(collect_partition(pt, "5")$x, "row5")
  expect_equal(collect_partition(pt, "7")$x, "row7")
})

test_that("insert_data routes a newdata that names one numeric partition", {
  # `insert_data()` shuffles `partitions_in_use`, which is its own call site and
  # not the `partitions_randomized` binding. A newdata that names exactly one
  # partition makes that vector a length-1 numeric.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_num_single",
    partitions = c(5L, 7L)
  )
  withr::defer(pt$disconnect())

  # `sample(5L, 1)` draws from 1:5. Under seed 1 that draw is 1, measured on
  # R 4.6.1 on 2026-08-14. The list holds no child named "1".
  d <- data.table::data.table(x = "row5", n = 1L, part = 5L)
  withr::with_seed(1, suppressMessages(pt$insert_data(d)))

  expect_equal(collect_partition(pt, "5")$x, "row5")
  expect_equal(nrow(collect_partition(pt, "7")), 0L)
})

test_that("indexes is a field of the partitioned table", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_indexes",
    indexes = list("ind1" = c("x"))
  )
  withr::defer(pt$disconnect())

  expect_equal(pt$indexes, list("ind1" = "x"))
  expect_equal(names(pt$indexes), "ind1")
})

test_that("upsert with the default drop_indexes writes every row", {
  # The default is a plain NULL, which is what `names(self$indexes)` evaluated
  # to before `indexes` was a field. This pins that the write still works.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_indexes_upsert",
    indexes = list("ind1" = c("x"))
  )
  withr::defer(pt$disconnect())

  suppressMessages(pt$upsert_data(sentinel_rows()))
  expect_sentinels_intact(pt)
})

test_that("a factor partition column routes every row and keeps every row", {
  # An adversarial review said a factor column against numeric partitions
  # passes the guard and then wipes every partition. Measured on 2026-08-14, it
  # does not. `setdiff()` converts a factor to character, and `==` against a
  # factor compares its labels, so both sides already agreed. This test pins
  # that, so a later change cannot regress it in silence.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_factor_route",
    partitions = c(5L, 7L)
  )
  withr::defer(pt$disconnect())

  d <- data.table::data.table(
    x = c("f5a", "f5b", "f7a"),
    n = c(1L, 2L, 3L),
    part = factor(c("5", "5", "7"), levels = c("5", "7"))
  )
  # csdb warns "Factors converted to character" when it writes the column.
  suppressWarnings(
    suppressMessages(pt$drop_all_rows_and_then_upsert_data(d))
  )

  p5 <- collect_partition(pt, "5")
  expect_equal(nrow(p5), 2L)
  expect_equal(p5$x, c("f5a", "f5b"))
  expect_equal(p5$n, c(1L, 2L))
  expect_equal(p5$part, c("5", "5"))

  p7 <- collect_partition(pt, "7")
  expect_equal(nrow(p7), 1L)
  expect_equal(p7$x, "f7a")
  expect_equal(p7$n, 3L)
  expect_equal(p7$part, "7")
})

test_that("the guard rejects a list partition column", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(dbconfig, "anon_safety_list_col")
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(sentinel_rows()))
  expect_sentinels_intact(pt)

  bad <- data.table::data.table(x = "new1", n = 1L, part = list("a"))
  expect_error(
    suppressMessages(pt$drop_all_rows_and_then_upsert_data(bad)),
    "The partition column 'part' must be atomic. Cannot route a list column.",
    fixed = TRUE
  )

  # This is the point of the test. `setdiff(list("a"), c("a", "b", "c"))` is
  # empty, so validation passed. Every partition was then dropped, and the
  # write died inside csdb on `is.infinite(get(i))`, which has no list method.
  # All six rows were lost, with no partial write. Measured on 2026-08-14.
  expect_sentinels_intact(pt)
})

test_that("the constructor rejects a zero-length partition set", {
  dbconfig <- local_sqlite_dbconfig()
  expect_error(
    new_safety_table(
      dbconfig,
      "anon_safety_parts_empty",
      partitions = character(0)
    ),
    "table_name_partitions is empty.",
    fixed = TRUE
  )
})

test_that("the constructor rejects an NA in the partition set", {
  dbconfig <- local_sqlite_dbconfig()
  expect_error(
    new_safety_table(
      dbconfig,
      "anon_safety_parts_na",
      partitions = c("a", NA_character_)
    ),
    "table_name_partitions holds NA.",
    fixed = TRUE
  )
})

test_that("the constructor rejects an empty string in the partition set", {
  # `self$tables[[""]] <- child` matches no name, so it APPENDS. A set of
  # c("a", "") built three children for two partitions. Measured on 2026-08-14.
  dbconfig <- local_sqlite_dbconfig()
  expect_error(
    new_safety_table(
      dbconfig,
      "anon_safety_parts_blank",
      partitions = c("a", "")
    ),
    "table_name_partitions holds an empty string.",
    fixed = TRUE
  )
})

test_that("the constructor rejects a duplicate in the partition set", {
  # `self$tables[[name]]` takes the FIRST match, so the second child is
  # unreachable while `length(self$partitions)` still counts it.
  dbconfig <- local_sqlite_dbconfig()
  expect_error(
    new_safety_table(
      dbconfig,
      "anon_safety_parts_dup",
      partitions = c("a", "b", "a")
    ),
    "table_name_partitions holds duplicate values: a.",
    fixed = TRUE
  )
})

# Four rows over three partitions, so the row counts differ per partition. A
# `partition` column attached to the wrong row is then visible as a count, and
# not only as a name.
#
# No tag is a substring of another, so a partial match cannot pass. "5" is
# numeric-looking, and that catches a coercion mistake. The returned value must
# index `self$tables` by NAME, because `[[` takes a number as a POSITION.
partition_col_rows <- function() {
  data.table::data.table(
    x = c("pc1", "pc2", "pc3", "pc4"),
    n = c(1L, 2L, 3L, 4L),
    part = c("alpha", "5", "5", "zulu")
  )
}

# `pt$tables[[row$partition]]$table_name` equals `row$table_name`, for every
# row. This is the assertion that the discarded tag fails.
#
# It names no separator, and that is the point of the change. cs9 builds a
# partition table name with `xxpxx` on two drivers and with `PARTITION` on the
# third. A test that encoded one of those would rebuild the defect it pins.
expect_partition_matches_table_name <- function(pt, x) {
  expect_type(x$partition, "character")
  expect_setequal(x$partition, names(pt$tables))
  expect_equal(anyDuplicated(x$partition), 0L)
  for (j in seq_len(nrow(x))) {
    expect_equal(pt$tables[[x$partition[j]]]$table_name, x$table_name[j])
  }
}

test_that("nrow(collapse = FALSE) carries the partition tag of every row", {
  dbconfig <- local_sqlite_dbconfig()
  parts <- c("alpha", "5", "zulu")
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_nrow_partition",
    partitions = parts
  )
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(partition_col_rows()))

  x <- pt$nrow(collapse = FALSE)

  expect_true("partition" %in% names(x))
  expect_equal(nrow(x), length(parts))
  # `partitions_randomized` shuffles, so this is set equality and never row
  # order.
  expect_partition_matches_table_name(pt, x)

  # Each count is read by tag, not by position.
  expect_equal(x$nrow[match("alpha", x$partition)], 1)
  expect_equal(x$nrow[match("5", x$partition)], 2)
  expect_equal(x$nrow[match("zulu", x$partition)], 1)
})

test_that("nrow keeps its old columns and its collapsed number", {
  # The change is additive. `table_name` and `nrow` keep their names, their
  # types and their values, so an existing caller is unaffected.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_nrow_additive",
    partitions = c("alpha", "5", "zulu")
  )
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(partition_col_rows()))

  x <- pt$nrow(collapse = FALSE)
  expect_equal(names(x), c("table_name", "nrow", "partition"))
  expect_type(x$table_name, "character")
  expect_setequal(
    x$table_name,
    vapply(pt$tables, function(tab) tab$table_name, character(1))
  )

  total <- pt$nrow(collapse = TRUE)
  expect_length(total, 1L)
  expect_equal(total, 4)
})

test_that("nrow(collapse = FALSE) holds table_name at 1 and nrow at 2", {
  # This is the compatibility contract, measured rather than asserted. The new
  # column goes LAST. An external caller that reads `x[[1]]` or `x[, 1]` gets
  # what it got before `partition` existed, and by position, not only by name.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_nrow_positions",
    partitions = c("alpha", "5", "zulu")
  )
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(partition_col_rows()))

  x <- pt$nrow(collapse = FALSE)

  expect_equal(names(x)[1], "table_name")
  expect_equal(names(x)[2], "nrow")
  expect_equal(names(x), c("table_name", "nrow", "partition"))

  # The exact types of the two pre-existing columns, measured on the pod on
  # 2026-08-14. `nrow` is double and not integer, because csdb returns it so.
  expect_type(x[[1]], "character")
  expect_type(x[[2]], "double")

  # Access by position and access by name return the same vector.
  expect_identical(x[[1]], x$table_name)
  expect_identical(x[[2]], x$nrow)

  # The count of each partition is read by position, through column 1 and
  # column 2 alone. A caller that never heard of `partition` still works.
  expect_equal(x[[2]][match(pt$tables[["alpha"]]$table_name, x[[1]])], 1)
  expect_equal(x[[2]][match(pt$tables[["5"]]$table_name, x[[1]])], 2)
  expect_equal(x[[2]][match(pt$tables[["zulu"]]$table_name, x[[1]])], 1)
})

test_that("info(collapse = TRUE) returns its four aggregates and no partition", {
  # An aggregate spans every partition, so it has no single tag. These four
  # columns are the whole return, and they are what `info(collapse = TRUE)`
  # returned before this change.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_info_collapsed",
    partitions = c("alpha", "5", "zulu")
  )
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(partition_col_rows()))

  x <- pt$info(collapse = TRUE)

  expect_equal(
    names(x),
    c("size_total_gb", "size_data_gb", "size_index_gb", "nrow")
  )
  expect_false("partition" %in% names(x))
  expect_false("keep" %in% names(x))
  expect_equal(nrow(x), 1L)
  expect_equal(x$nrow, 4)
})

test_that("info(collapse = FALSE) puts partition in the last column", {
  # `partition` is appended, and nothing else moves. The expectation reads the
  # base columns from csdb itself, so a change inside csdb does not turn this
  # test red for the wrong reason.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_info_last_column",
    partitions = c("alpha", "5", "zulu")
  )
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(partition_col_rows()))

  base_cols <- names(csdb::get_table_names_and_info(
    pt$tables[["alpha"]]$dbconnection$autoconnection
  ))

  x <- pt$info(collapse = FALSE)

  expect_equal(names(x)[length(names(x))], "partition")
  expect_equal(names(x), c(base_cols, "partition"))
  expect_false("keep" %in% names(x))
})

test_that("info() carries the partition tag of every row", {
  dbconfig <- local_sqlite_dbconfig()
  parts <- c("alpha", "5", "zulu")
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_info_partition",
    partitions = parts
  )
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(partition_col_rows()))

  x <- pt$info()

  expect_true("partition" %in% names(x))
  expect_false("keep" %in% names(x))
  expect_true(all(c("table_name", "nrow") %in% names(x)))
  expect_equal(nrow(x), length(parts))
  expect_partition_matches_table_name(pt, x)
})

test_that("a numeric partition comes back as a character partition", {
  # `self$tables[[5L]]` returns the fifth child, and this list holds two. An
  # integer in the `partition` column would therefore fail the lookup below,
  # and a caller reading it would index the wrong child.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_nrow_partition_num",
    partitions = c(5L, 7L)
  )
  withr::defer(pt$disconnect())

  d <- data.table::data.table(
    x = c("row5", "row7"),
    n = c(1L, 2L),
    part = c(5L, 7L)
  )
  suppressMessages(pt$insert_data(d))

  x <- pt$nrow(collapse = FALSE)
  expect_partition_matches_table_name(pt, x)
  expect_equal(sort(x$partition), c("5", "7"))

  x_info <- pt$info()
  expect_partition_matches_table_name(pt, x_info)
  expect_equal(sort(x_info$partition), c("5", "7"))
})

test_that("the guard returns the route vector that the write methods use", {
  # The four write methods index with the vector this returns. Its type and its
  # length are the contract that keeps validation and routing from diverging.
  # Reaching into private is deliberate, and it is confined to this test.
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_safety_table(
    dbconfig,
    "anon_safety_route_vector",
    partitions = c(5L, 7L)
  )
  withr::defer(pt$disconnect())
  priv <- pt$.__enclos_env__$private

  d <- data.table::data.table(
    x = c("r1", "r2", "r3"),
    n = c(1L, 2L, 3L),
    part = factor(c("5", "7", "5"), levels = c("5", "7"))
  )
  route <- priv$check_for_correct_partitions_in_data(d)
  expect_type(route, "character")
  expect_equal(length(route), nrow(d))
  expect_equal(route, c("5", "7", "5"))

  # A zero-row newdata still returns a character vector, of length zero.
  empty <- data.table::data.table(
    x = character(0),
    n = integer(0),
    part = character(0)
  )
  route_empty <- priv$check_for_correct_partitions_in_data(empty)
  expect_type(route_empty, "character")
  expect_equal(length(route_empty), nrow(empty))
})
