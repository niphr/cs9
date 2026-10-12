# A partitioned insert_data() loads every partition with ONE call to
# csdb::insert_data_many(). On PostgreSQL that call is one psql session and one
# transaction, so one insert needs one psql login for the load, plus one for
# the last-updated bookkeeping. By default a failed insert stores no row; the
# exception is a duplicate key with confirm_insert_via_nrow = TRUE. Before
# 26.10.13 the method called insert_data() once for each partition, and each
# call had its own psql login.
#
# These tests run on SQLite. A recorder wraps csdb::insert_data_many() and
# then calls the real function, so the rows still reach the database. The
# real-server check is in the cs9 transcript of phase 20.
#
# The fixtures repeat `tests/testthat/test-partition-insert-last-updated.R`.
# testthat sources each test file into its own environment, so a helper in one
# file is not visible in another.

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

new_partition_table <- function(dbconfig) {
  return(DBPartitionedTableExtended_v9$new(
    dbconfig = dbconfig,
    table_name_base = "onecall",
    table_name_partitions = five_partitions,
    column_name_partition = "part",
    field_types = c("x" = "TEXT", "n" = "INTEGER"),
    keys = c("x", "part"),
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  ))
}

# Two rows per partition, with keys r<from>, r<from + 1>, ...
new_rows <- function(n, partitions) {
  return(data.table::data.table(
    x = paste0("r", n),
    n = n,
    part = rep(partitions, each = 2)
  ))
}

# Record every call to csdb::insert_data_many(). `fail = TRUE` stops each call
# instead of loading. A copy of the column names is kept at the time of the
# call: on SQLite csdb calls each child's insert_data(), which adds columns to
# the same data.tables by reference afterwards. names() of a data.table
# returns the attribute that `:=` grows in place, so paste0() makes the copy.
local_record_insert_data_many <- function(
  fail = FALSE,
  envir = parent.frame()
) {
  real <- csdb::insert_data_many
  rec <- new.env()
  rec$calls <- list()
  local_mocked_bindings(
    insert_data_many = function(tables, newdata, ...) {
      rec$calls <- c(
        rec$calls,
        list(list(
          tables = tables,
          newdata = newdata,
          columns = lapply(newdata, function(d) {
            return(paste0(names(d)))
          }),
          args = list(...)
        ))
      )
      if (fail) {
        stop("cs9test insert_data_many failure", call. = FALSE)
      }
      return(real(tables, newdata, ...))
    },
    .package = "csdb",
    .env = envir
  )
  return(rec)
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

test_that("partitioned insert_data() makes one csdb::insert_data_many() call with every partition in newdata", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partition_table(dbconfig)
  withr::defer(pt$disconnect())
  rec <- local_record_insert_data_many()

  suppressMessages(pt$insert_data(new_rows(1:6, c("a", "c", "e"))))

  expect_length(rec$calls, 1L)
  # No call leaves an empty record, so the checks below fail and do not error.
  call <- if (length(rec$calls) > 0) {
    rec$calls[[1]]
  } else {
    list(tables = list(), newdata = list(), columns = list(), args = list())
  }
  names_sent <- vapply(
    call$tables,
    function(t) {
      return(t$table_name)
    },
    character(1)
  )
  expect_setequal(
    names_sent,
    vapply(
      c("a", "c", "e"),
      function(p) {
        return(pt$tables[[p]]$table_name)
      },
      character(1),
      USE.NAMES = FALSE
    )
  )
  # Each data.table holds the rows of its own partition, stamped.
  for (k in seq_along(call$tables)) {
    part_of_table <- names(pt$tables)[vapply(
      pt$tables,
      function(t) {
        return(identical(t, call$tables[[k]]))
      },
      logical(1)
    )]
    expect_identical(unique(call$newdata[[k]]$part), part_of_table)
    expect_identical(nrow(call$newdata[[k]]), 2L)
    expect_true("auto_last_updated_datetime" %in% call$columns[[k]])
  }
  expect_false(call$args$update_last_updated)
  expect_equal(pt$nrow(), 6)
})

test_that("a failed partitioned insert_data() records no partition", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partition_table(dbconfig)
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(new_rows(1:10, five_partitions)))
  counter <- local_count_last_updated_upserts()
  suppressMessages(config$tables$config_tables_last_updated$drop_all_rows())
  counter$n <- 0L

  rec <- local_record_insert_data_many(fail = TRUE)
  expect_error(
    suppressMessages(pt$insert_data(new_rows(11:16, c("a", "c", "e")))),
    "cs9test insert_data_many failure"
  )
  expect_length(rec$calls, 1L)
  expect_equal(counter$n, 0L)
  expect_length(read_last_updated_names(), 0L)
})

test_that("a successful partitioned insert_data() records every partition in one upsert", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partition_table(dbconfig)
  withr::defer(pt$disconnect())
  suppressMessages(pt$insert_data(new_rows(1:10, five_partitions)))
  counter <- local_count_last_updated_upserts()
  suppressMessages(config$tables$config_tables_last_updated$drop_all_rows())
  counter$n <- 0L

  suppressMessages(pt$insert_data(new_rows(11:16, c("a", "c", "e"))))
  expect_equal(counter$n, 1L)
  expect_identical(
    read_last_updated_names(),
    sort(vapply(
      c("a", "c", "e"),
      function(p) {
        return(pt$tables[[p]]$table_name)
      },
      character(1),
      USE.NAMES = FALSE
    ))
  )
})

test_that("a partitioned insert_data() with zero rows calls csdb with no table", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partition_table(dbconfig)
  withr::defer(pt$disconnect())
  rec <- local_record_insert_data_many()
  counter <- local_count_last_updated_upserts()
  counter$n <- 0L
  suppressMessages(pt$insert_data(new_rows(integer(0), character(0))))
  expect_length(rec$calls, 1L)
  expect_length(unlist(lapply(rec$calls, `[[`, "tables")), 0L)
  expect_equal(counter$n, 0L)
})

# The rows are committed before cs9 records the insert. A failure of that
# bookkeeping write MUST NOT report a failed insert, because a caller that
# ran the insert again would hit a duplicate key.
test_that("a failed last-updated write after a successful insert gives a warning, and the insert returns", {
  dbconfig <- local_sqlite_dbconfig()
  pt <- new_partition_table(dbconfig)
  withr::defer(pt$disconnect())
  local_mocked_bindings(
    update_config_tables_last_updated = function(...) {
      stop("cs9test bookkeeping failure", call. = FALSE)
    }
  )
  warnings <- character()
  result <- withCallingHandlers(
    tryCatch(
      suppressMessages(pt$insert_data(new_rows(1:6, c("a", "c", "e")))),
      error = function(e) e
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_null(result)
  expect_length(warnings, 1L)
  expect_match(
    warnings,
    "succeeded, but recording it in config_tables_last_updated failed: cs9test bookkeeping failure",
    fixed = TRUE,
    all = FALSE
  )
  for (p in c("a", "c", "e")) {
    expect_match(warnings, pt$tables[[p]]$table_name, fixed = TRUE, all = FALSE)
  }
  expect_equal(pt$nrow(), 6)
})
