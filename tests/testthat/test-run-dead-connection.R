# The cleanup SQL of a keyed run on a connection that dropped. On such a
# connection the second statement crashes R with a segfault in odbc.
# run_unlock_tables() and the RESET ROLE of run_as_creator() therefore send no
# SQL when csdb::csdb_connection_is_alive() is FALSE. The PostgreSQL test at the end
# runs only when CS9_TEST_PG_SERVER is set: see helper-run-staging-pg.R.

# Records each statement that DBI::dbGetQuery() or DBI::dbExecute() gets,
# until the calling test ends. With `fail = TRUE` each statement fails.
local_sql_recorder <- function(fail = FALSE, envir = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$sql <- character()
  record <- function(conn, statement, ...) {
    rec$sql <- c(rec$sql, statement)
    if (fail) {
      stop("cs9 test: the connection is down", call. = FALSE)
    }
    return(data.frame(got = 1L))
  }
  testthat::local_mocked_bindings(
    dbGetQuery = record,
    dbExecute = record,
    .package = "DBI",
    .env = envir
  )
  return(rec)
}

local_alive <- function(alive, envir = parent.frame()) {
  testthat::local_mocked_bindings(
    csdb_connection_is_alive = function(connection) alive,
    .package = "csdb",
    .env = envir
  )
}

local_sqlite_con <- function(envir = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = envir)
  return(con)
}

test_that("run_unlock_tables() sends no SQL on a connection that dropped", {
  con <- local_sqlite_con()
  local_alive(FALSE)
  rec <- local_sql_recorder()
  run_unlock_tables(con, c("anon_a", "anon_b"))
  expect_identical(rec$sql, character())
})

test_that("run_unlock_tables() unlocks each table on a live connection", {
  con <- local_sqlite_con()
  local_alive(TRUE)
  rec <- local_sql_recorder()
  run_unlock_tables(con, c("anon_a", "anon_b"))
  expect_length(rec$sql, 2)
  expect_match(rec$sql, "pg_advisory_unlock(", fixed = TRUE)
})

# The real guard, csdb::csdb_connection_is_alive(), on a live connection whose
# catalog request fails for a reason other than a lost connection.
test_that("run_unlock_tables() unlocks each table when the catalog request fails for another reason", {
  con <- local_sqlite_con()
  testthat::local_mocked_bindings(
    dbExistsTable = function(conn, name, ...) {
      stop("permission denied for schema anon", call. = FALSE)
    },
    .package = "DBI"
  )
  rec <- local_sql_recorder()
  run_unlock_tables(con, c("anon_a", "anon_b"))
  expect_length(rec$sql, 2)
})

test_that("run_unlock_tables() stops at the first unlock that fails", {
  con <- local_sqlite_con()
  local_alive(TRUE)
  rec <- local_sql_recorder(fail = TRUE)
  expect_no_error(run_unlock_tables(con, c("anon_a", "anon_b")))
  expect_length(rec$sql, 1)
})

test_that("run_as_creator() sends no RESET ROLE on a connection that dropped", {
  con <- local_sqlite_con()
  local_alive(FALSE)
  rec <- local_sql_recorder()
  state <- list(conn = list(config = list(role_create_table = "creator")))
  run_as_creator(con, state, "CREATE TABLE x (a INTEGER)")
  expect_identical(
    rec$sql,
    c("SET ROLE `creator`", "CREATE TABLE x (a INTEGER)")
  )
})

test_that("run_as_creator() resets the role on a live connection", {
  con <- local_sqlite_con()
  local_alive(TRUE)
  rec <- local_sql_recorder()
  state <- list(conn = list(config = list(role_create_table = "creator")))
  run_as_creator(con, state, "CREATE TABLE x (a INTEGER)")
  expect_identical(
    rec$sql,
    c("SET ROLE `creator`", "CREATE TABLE x (a INTEGER)", "RESET ROLE")
  )
})

# Runs fn(args) in a new Rscript process. Returns the exit status of the
# process, and the value of fn when the status is 0. A segfault in the child
# gives a status other than 0. Base R starts the child, not callr. With callr,
# the whole suite ended with "Error while shutting down parallel: unable to
# terminate some child processes", measured on 2026-10-22.
pg_in_child <- function(fn, args) {
  script <- withr::local_tempfile(fileext = ".R")
  result <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c(
      paste0(".libPaths(", paste(deparse(.libPaths()), collapse = ""), ")"),
      paste0("fn <- ", paste(deparse(fn), collapse = "\n")),
      paste0("args <- ", paste(deparse(args), collapse = "\n")),
      paste0("dput(do.call(fn, args), file = ", deparse(result), ")")
    ),
    script
  )
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    shQuote(script),
    stdout = FALSE,
    stderr = FALSE
  )
  value <- if (identical(status, 0L)) dget(result) else NULL
  return(list(status = status, value = value))
}

# The child process of the next test. A regression ends it with a segfault,
# and the test then fails. The child loads the cs9 under test: the source tree
# after load_all(), or else the installed package.
pg_dead_child <- function(path) {
  if (nzchar(path)) {
    devtools::load_all(path, quiet = TRUE)
  } else {
    loadNamespace("cs9")
  }
  ns <- asNamespace("cs9")
  open <- function() {
    return(csdb::DBConnection_v9$new(
      driver = "PostgreSQL Unicode",
      server = Sys.getenv("CS9_TEST_PG_SERVER"),
      port = as.integer(Sys.getenv("CS9_TEST_PG_PORT", "5432")),
      db = Sys.getenv("CS9_TEST_PG_DB"),
      user = Sys.getenv("CS9_TEST_PG_USER"),
      password = Sys.getenv("CS9_TEST_PG_PASSWORD")
    ))
  }
  outcome <- function(expr) {
    return(tryCatch(
      {
        force(expr)
        "returned"
      },
      error = function(e) "R error"
    ))
  }
  # A statement that ends its own session, as a failover ends every session.
  end_session <- "SELECT pg_terminate_backend(pg_backend_pid())"
  out <- list()
  unlock_conn <- open()
  con <- unlock_conn$autoconnection
  ns$run_lock_tables(con, c("cs9t_dead_a", "cs9t_dead_b"))
  out$ended <- outcome(DBI::dbGetQuery(con, end_session))
  out$unlock <- outcome(ns$run_unlock_tables(
    con,
    c("cs9t_dead_a", "cs9t_dead_b")
  ))
  creator_conn <- open()
  state <- list(
    conn = list(
      config = list(role_create_table = Sys.getenv("CS9_TEST_PG_USER"))
    )
  )
  out$creator <- outcome(
    ns$run_as_creator(creator_conn$autoconnection, state, end_session)
  )
  return(out)
}

test_that("on a PostgreSQL connection that the server ended, the unlock and the RESET ROLE end without a segfault", {
  skip_without_pg()
  dev <- exists(".__DEVTOOLS__", envir = asNamespace("cs9"), inherits = FALSE)
  if (dev) {
    skip_if_not_installed("devtools")
  }
  path <- if (dev) getNamespaceInfo("cs9", "path") else ""
  child <- pg_in_child(pg_dead_child, list(path = path))
  expect_identical(child$status, 0L)
  out <- child$value
  expect_identical(out$ended, "R error")
  expect_identical(out$unlock, "returned")
  expect_identical(out$creator, "R error")
  # The server released both locks when it ended the session.
  conn <- pg_test_connect()
  on.exit(conn$disconnect(), add = TRUE)
  expect_false(pg_lock_is_held(conn$autoconnection, "cs9 table cs9t_dead_a"))
  expect_false(pg_lock_is_held(conn$autoconnection, "cs9 table cs9t_dead_b"))
})
