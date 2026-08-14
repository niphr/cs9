# Every partition of a DBPartitionedTableExtended_v9 shares one
# csdb::DBConnection_v9, and the parent closes it exactly once. "Exactly once"
# is counted here, not inferred: see count_parent_closes() below.
#
# The last test records the limitation that the shared connection carries. A
# child kept after the parent's disconnect() can reopen the connection, and
# only the parent can close it again.
#
# Each test sets its own environment with withr::local_envvar(). cs9 builds
# config$dbconfigs from Sys.getenv(), and a production ~/.Renviron points
# CS9_DBCONFIG_* at a PostgreSQL server. Without that override a test can reach
# the real server and still report success.
#
# cs9 has no testthat helper file. csdb's sqlite_dbconfig() and
# sqlite_connection() live in csdb/tests/testthat/helper-sqlite.R, and testthat
# loads a helper only into its own package's suite. The fixtures below are
# therefore built inline.
#
# There is deliberately no skip_if_not_installed("RSQLite") here. csdb imports
# RSQLite, and cs9 imports csdb, so RSQLite is present whenever this suite
# runs. A skip would turn a missing package into a silently green run.

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

# Three partitions, which is the smallest count that separates "one shared
# connection" from "one connection per partition" by more than a pair.
new_partitioned_table <- function(dbconfig) {
  DBPartitionedTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name_base = "anon_shared",
    table_name_partitions = c("a", "b", "c"),
    column_name_partition = "part",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = c("x", "part"),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
}

# TRUE only when the connection exists and is open.
#
# $connection returns private$pconnection without connecting, which is the
# point: $autoconnection calls connect() and would hide a close. That binding
# is NULL until the first connect(), and DBI::dbIsValid() has no method for
# NULL, so a bare call errors instead of answering.
is_open <- function(dbconnection) {
  con <- dbconnection$connection
  !is.null(con) && DBI::dbIsValid(con)
}

# Counts the closes that DBPartitionedTableExtended_v9$disconnect() performs.
#
# R6 locks a method binding, so the disconnect() method on the connection
# object cannot be replaced. R reports "cannot change value of locked binding
# for 'disconnect'". R6 does not lock a field binding, and the parent reads the
# connection out of its public dbconnection field. The replacement below
# forwards to the real connection, so the close is a real close and the count
# is a count of real closes. Each child keeps its own reference to the real
# object, so the children are untouched by the replacement.
#
# Read the real connection back as probe$shared. After the replacement,
# pt$dbconnection is the counter and is_open() on it returns FALSE.
count_parent_closes <- function(pt) {
  probe <- new.env(parent = emptyenv())
  probe$closes <- 0L
  probe$shared <- pt$dbconnection
  pt$dbconnection <- list(
    disconnect = function() {
      probe$closes <- probe$closes + 1L
      probe$shared$disconnect()
    }
  )
  probe
}

# One row per partition, so every child opens a connection. A partition table
# is created lazily, and a partition that receives no rows opens nothing.
three_rows <- function() {
  data.table::data.table(
    x = c("row_a", "row_b", "row_c"),
    n = c(1L, 2L, 3L),
    part = c("a", "b", "c")
  )
}

test_that("every partition holds one shared connection object", {
  # Provenance, not a skip. csdb 2026.8.14 is the version where
  # DBTable_v9$initialize() accepts dbconnection. An older csdb MUST fail this
  # file loudly rather than skip it.
  expect_true(utils::packageVersion("csdb") >= "2026.8.14")

  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partitioned_table(dbconfig)
  withr::defer(pt$disconnect())

  # format() prints identical text for two distinct R6 objects, so a count
  # built on format() reports 1 whatever the code does. address() reads where
  # the object lives and cannot do that.
  addresses <- vapply(
    pt$tables,
    function(z) data.table::address(z$dbconnection),
    character(1)
  )
  expect_length(pt$tables, 3L)
  expect_length(unique(addresses), 1L)

  # The same fact by pairwise identity, which does not depend on address().
  expect_true(identical(
    pt$tables[["a"]]$dbconnection,
    pt$tables[["b"]]$dbconnection
  ))
  expect_true(identical(
    pt$tables[["a"]]$dbconnection,
    pt$tables[["c"]]$dbconnection
  ))

  # The parent owns that one object.
  expect_true(identical(pt$dbconnection, pt$tables[["a"]]$dbconnection))
})

test_that("a write reaches the partition that its part column names", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partitioned_table(dbconfig)
  withr::defer(pt$disconnect())

  d <- three_rows()
  suppressMessages(pt$insert_data(data.table::copy(d)))

  for (i in c("a", "b", "c")) {
    out <- dplyr::collect(pt$tables[[i]]$tbl())
    data.table::setDT(out)
    expect_equal(nrow(out), 1L)
    expect_equal(out$x, d[part == i]$x)
    expect_equal(out$n, d[part == i]$n)
    expect_equal(out$part, i)
  }
})

test_that("the task teardown loop closes the shared connection exactly once", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partitioned_table(dbconfig)

  suppressMessages(pt$insert_data(three_rows()))

  # The writes went through the parent's own connection, and it is open. This
  # is what stops the three expect_false() calls below passing on a connection
  # that was never opened at all. It reads pt$dbconnection before the counter
  # replaces that field.
  expect_true(is_open(pt$dbconnection))

  probe <- count_parent_closes(pt)

  # r6_Task.R holds `for (s in tables) s$disconnect()`, where `tables` is the
  # task's own list of tables. A partitioned table appears in that list as the
  # parent object, so production calls the parent method.
  tables <- list("anon_shared" = pt)
  for (s in tables) s$disconnect()

  # The count is what makes "exactly once" a measurement. A disconnect() that
  # looped over the three children instead would count 0, because a borrowing
  # child does not close a connection it does not own.
  expect_identical(probe$closes, 1L)

  expect_false(is_open(probe$shared))
  expect_false(is_open(pt$tables[["a"]]$dbconnection))
  expect_false(is_open(pt$tables[["b"]]$dbconnection))
  expect_false(is_open(pt$tables[["c"]]$dbconnection))

  # A second call reaches the connection and does not error. The count rises to
  # 2, which shows the call was not stopped somewhere before it got there.
  expect_no_error(pt$disconnect())
  expect_identical(probe$closes, 2L)
})

test_that("a disconnect loop over every child leaves the table usable", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partitioned_table(dbconfig)
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(three_rows()))

  # The same loop shape as r6_Task.R, applied one level down. Each child
  # borrows the connection, so no child closes it.
  for (s in pt$tables) s$disconnect()
  expect_true(is_open(pt$tables[["a"]]$dbconnection))

  # The table still writes and still reads after that loop.
  suppressMessages(pt$insert_data(
    data.table::data.table(x = "row_a2", n = 4L, part = "a")
  ))

  out <- dplyr::collect(pt$tables[["a"]]$tbl())
  data.table::setDT(out)
  data.table::setorderv(out, "n")
  expect_equal(out$x, c("row_a", "row_a2"))
  expect_equal(out$n, c(1L, 4L))
})

test_that("a child kept past the parent teardown reopens the shared connection", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partitioned_table(dbconfig)
  withr::defer(pt$disconnect())

  suppressMessages(pt$insert_data(three_rows()))
  child <- pt$tables[["a"]]

  expect_true(is_open(pt$dbconnection))

  # The parent owns the connection, so the parent closes it.
  pt$disconnect()
  expect_false(is_open(pt$dbconnection))

  # Any use of the kept child reopens the connection. DBTable_v9$tbl() reads
  # $autoconnection, and that binding calls connect() on every access.
  out <- dplyr::collect(child$tbl())
  data.table::setDT(out)
  expect_equal(out$x, "row_a")
  expect_true(is_open(pt$dbconnection))

  # The child borrows the connection, so its disconnect() does not close it.
  # This is the limitation the shared connection carries, asserted here rather
  # than left unknown.
  child$disconnect()
  expect_true(is_open(pt$dbconnection))

  # The parent can still close it. The connection is reachable, not stranded.
  pt$disconnect()
  expect_false(is_open(pt$dbconnection))
})
