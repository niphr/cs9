# cs9 writes its own config tables with one SQL statement over the connection
# of the table, on PostgreSQL and SQLite. Before 26.10.20 each write went
# through the csdb load, which runs psql on PostgreSQL: one login for each
# write. The fixtures are in helper-run-staging.R.

# Replaces one method of an R6 object until the calling test ends.
local_method <- function(object, name, fn, envir = parent.frame()) {
  original <- object[[name]]
  unlockBinding(name, object)
  assign(name, fn, envir = object)
  withr::defer(
    {
      assign(name, original, envir = object)
      lockBinding(name, object)
    },
    envir = envir
  )
  return(invisible(NULL))
}

# Each csdb load of a config table stops.
local_refuse_csdb_load <- function(envir = parent.frame()) {
  for (tab in config$tables) {
    for (m in c("upsert_data", "insert_data")) {
      local_method(
        tab,
        m,
        function(...) stop("cs9test: the csdb load ran", call. = FALSE),
        envir = envir
      )
    }
  }
  return(invisible(NULL))
}

# One row for each config table, with the R classes that the writers pass.
# `key` goes into a key column. The NA values test NULL.
config_sample_rows <- list(
  config_log = function(key) {
    return(data.table::data.table(
      auto_interactive = "auto",
      ss = "s",
      task = key,
      date = as.Date("2026-10-15"),
      datetime = "2026-10-15 10:00:00.123",
      message = "it's"
    ))
  },
  config_tables_last_updated = function(key) {
    return(data.table::data.table(
      table_name = key,
      date = as.Date("2026-10-15"),
      datetime = "2026-10-15 10:00:00"
    ))
  },
  config_tasks_stats = function(key) {
    return(data.table::data.table(
      auto_interactive = "auto",
      ss = "s",
      task = key,
      cs_version = "26.10.20",
      implementation_version = "unspecified",
      cores_n = 4,
      plans_n = 10L,
      analyses_n = 20L,
      start_date = "2026-10-15",
      start_datetime = "2026-10-15 10:00:00.123",
      stop_date = "2026-10-15",
      stop_datetime = "2026-10-15 10:01:00.456",
      runtime_minutes = 1.08,
      ram_all_cores_mb = 12.3,
      ram_per_core_mb = NA_real_,
      status = "succeeded"
    ))
  },
  config_data_hash_for_each_plan = function(key) {
    return(data.table::data.table(
      task = key,
      index_plan = 2L,
      element_tag = "e",
      date = as.Date("2026-10-15"),
      datetime = "2026-10-15 10:00:00.123",
      element_hash = "h",
      all_hash = NA_character_
    ))
  }
)

# The column of each config table that config_sample_rows() fills with `key`.
config_key_column <- c(
  config_log = "task",
  config_tables_last_updated = "table_name",
  config_tasks_stats = "task",
  config_data_hash_for_each_plan = "task"
)

# The storage class and the text of every column of one row, read with SQL.
stored_row <- function(name, key) {
  tab <- config$tables[[name]]
  con <- tab$dbconnection$autoconnection
  cols <- names(tab$field_types)
  sql <- paste0(
    "SELECT ",
    paste0("typeof(", cols, ") AS t_", cols, ", ", cols, collapse = ", "),
    " FROM ",
    name,
    " WHERE ",
    config_key_column[[name]],
    " = ?"
  )
  d <- DBI::dbGetQuery(con, sql, params = list(key))
  d[[config_key_column[[name]]]] <- NULL
  return(d)
}

test_that("the config writers do not call the csdb load", {
  local_staging_db()
  local_refuse_csdb_load()

  update_config_tables_last_updated(c("cs9test_a", "cs9test_b"))
  update_config_data_hash_for_each_plan(
    task = "cs9test",
    index_plan = 2L,
    element_tag = c("x", "y"),
    element_hash = c("h1", "h2"),
    all_hash = "all"
  )
  update_config_tasks_stats(
    task = "cs9test",
    cores_n = 4,
    plans_n = 10L,
    analyses_n = 20L,
    start_datetime = as.POSIXct("2026-10-15 10:00:00"),
    stop_datetime = as.POSIXct("2026-10-15 10:01:00"),
    ram_all_cores_mb = 12.3,
    ram_per_core_mb = 3.1,
    status = "succeeded"
  )
  suppressMessages(update_config_log(
    ss = "cs9test",
    task = "cs9test",
    "a message"
  ))

  expect_setequal(
    get_config_tables_last_updated()$table_name,
    c("cs9test_a", "cs9test_b")
  )
  expect_setequal(
    get_config_data_hash_for_each_plan(task = "cs9test")$element_hash,
    c("h1", "h2")
  )
  expect_identical(get_config_tasks_stats(task = "cs9test")$cores_n, 4L)
  expect_identical(config_log_messages(), "a message")
})

for (name in names(config_sample_rows)) {
  test_that(
    paste0("a plain SQL write stores what the csdb load stored (", name, ")"),
    {
      local_staging_db()
      tab <- config$tables[[name]]
      rows <- config_sample_rows[[name]]
      if (name == "config_log") {
        tab$insert_data(rows("cs9test_csdb"))
      } else {
        tab$upsert_data(rows("cs9test_csdb"))
      }

      config_write(tab, rows("cs9test_sql"), upsert = name != "config_log")

      csdb_row <- stored_row(name, "cs9test_csdb")
      expect_identical(nrow(csdb_row), 1L)
      expect_identical(stored_row(name, "cs9test_sql"), csdb_row)
    }
  )
}

test_that("a second plain SQL upsert of a key replaces the other columns", {
  local_staging_db()
  tab <- config$tables$config_tables_last_updated
  first <- config_sample_rows$config_tables_last_updated("cs9test")
  second <- data.table::copy(first)
  second[, datetime := "2026-10-16 11:00:00"]

  config_write(tab, first, upsert = TRUE)
  config_write(tab, second, upsert = TRUE)

  got <- get_config_tables_last_updated(table_name = "cs9test")
  expect_identical(nrow(got), 1L)
  expect_identical(format(got$datetime), "2026-10-16 11:00:00")
})

test_that("a plain SQL insert into config_log stops on a duplicate key", {
  local_staging_db()
  tab <- config$tables$config_log
  row <- config_sample_rows$config_log("cs9test")

  config_write(tab, row, upsert = FALSE)

  expect_error(config_write(tab, row, upsert = FALSE), "UNIQUE")
})

test_that("a PostgreSQL write sends each value as a quoted literal", {
  rows <- data.table::data.table(
    a = "it's",
    d = as.Date("2026-10-15"),
    n = 4L,
    x = 1.5,
    m = NA_character_
  )

  expect_identical(
    config_sql_values(DBI::ANSI(), rows),
    "('it''s', '2026-10-15', '4', '1.5', NULL)"
  )
})

# Outside a worker each data hash call closes its connection, as before
# 26.10.20. A forked worker sets config$in_parallel, and keeps the connection
# open from one plan to the next.
hash_calls <- function() {
  update_config_data_hash_for_each_plan(
    task = "cs9test",
    index_plan = 2L,
    element_tag = "x",
    element_hash = "h",
    all_hash = "all"
  )
  return(get_config_data_hash_for_each_plan(task = "cs9test"))
}

test_that("a data hash call outside a worker closes its connection", {
  local_staging_db()
  conn <- config$tables$config_data_hash_for_each_plan$dbconnection
  expect_false(isTRUE(config$in_parallel))

  got <- hash_calls()

  expect_identical(got$element_hash, "h")
  expect_false(conn$is_connected())
})

test_that("a data hash call in a worker keeps its connection open", {
  local_staging_db()
  conn <- config$tables$config_data_hash_for_each_plan$dbconnection
  config$in_parallel <- TRUE
  withr::defer(config$in_parallel <- FALSE)

  got <- hash_calls()

  expect_identical(got$element_hash, "h")
  expect_true(conn$is_connected())
})

# run_set_context() names an open PostgreSQL connection by the rule that csdb
# uses for a new connection.
test_that("the application_name of a context is the name csdb gives it", {
  contexts <- c(
    "norsyss_data_import_consultations|17",
    "task|parent",
    "it's; DROP TABLE x; --|3",
    "tab\there\nline\r|4",
    "n\u00e6r|\u00f8l",
    strrep("abcdefghij", 8),
    ""
  )

  ours <- vapply(contexts, run_application_name, character(1), USE.NAMES = FALSE)
  theirs <- vapply(contexts, csdb:::pg_application_name, character(1), USE.NAMES = FALSE)

  expect_identical(ours, theirs)
  expect_identical(ours[3], "itsDROPTABLEx--|3")
  expect_identical(nchar(ours[6], type = "bytes"), 63L)
  expect_false(any(grepl("[^A-Za-z0-9_|.:-]", ours)))
})
