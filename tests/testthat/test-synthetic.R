# Synthetic tables, synthetic mode and switchable sources of
# SurveillanceSystem_v9, and run-task.sh --synthetic.
#
# local_staging_db() is in helper-run-staging.R.

synth_fields <- c(x = "TEXT", part = "TEXT", n = "INTEGER")

# The message of the error that `expr` signals, or "" when it succeeds. A
# call that should stop and does not then fails an expectation.
synth_error <- function(expr) {
  return(tryCatch(
    {
      force(expr)
      ""
    },
    error = function(e) conditionMessage(e)
  ))
}

# A system whose tables are disconnected when the test ends.
local_synth_system <- function(envir = parent.frame()) {
  ss <- SurveillanceSystem_v9$new(name = "synthtest")
  withr::defer(
    for (t in c(ss$tables, ss$partitionedtables)) {
      t$disconnect()
    },
    envir = envir
  )
  return(ss)
}

# Counts every DBI::dbConnect() call to a database that is not a SQLite file
# under tempdir(), and stops it. No test here can connect to such a database,
# or create a file outside tempdir(). A connection to a SQLite file under
# tempdir() goes through. csdb calls DBI::dbConnect() for SQLite and for ODBC.
local_connect_spy <- function(envir = parent.frame()) {
  spy <- new.env(parent = emptyenv())
  spy$calls <- 0L
  real <- DBI::dbConnect
  under_tempdir <- function(db) {
    return(
      is.character(db) &&
        length(db) == 1 &&
        fs::path_has_parent(
          fs::path_norm(fs::path_abs(db)),
          fs::path_norm(fs::path_abs(tempdir()))
        )
    )
  }
  testthat::local_mocked_bindings(
    dbConnect = function(drv, ...) {
      if (inherits(drv, "SQLiteDriver") && under_tempdir(list(...)$dbname)) {
        return(real(drv, ...))
      }
      spy$calls <- spy$calls + 1L
      stop("cs9 test: a connection to an unsafe database was attempted")
    },
    .package = "DBI",
    .env = envir
  )
  return(spy)
}

# Adds the access "bad" to config$dbconfigs: a PostgreSQL server that does not
# answer, or a SQLite file outside tempdir(). Returns the SQLite path, or NA.
local_bad_access <- function(bad, envir = parent.frame()) {
  path <- NA_character_
  if (bad == "pg") {
    dbconfig <- list(
      access = "bad",
      driver = "PostgreSQL Unicode",
      server = "127.0.0.1",
      port = 1L,
      db = "cs9_synthetic_unreachable",
      schema = "public",
      user = "nobody",
      password = "",
      trusted_connection = "no",
      sslmode = "disable",
      role_create_table = ""
    )
  } else {
    # The parent of tempdir() is outside tempdir(). The test never creates the
    # file, and deletes it if a broken guard did.
    path <- file.path(
      dirname(tempdir()),
      paste0("cs9-synthetic-outside-", basename(tempfile()), ".sqlite")
    )
    withr::defer(unlink(path), envir = envir)
    dbconfig <- list(access = "bad", driver = "SQLite", db = path)
  }
  old <- config$dbconfigs
  config$dbconfigs$bad <- dbconfig
  withr::defer(config$dbconfigs <- old, envir = envir)
  return(path)
}

# A plain table with key x, or a table with the partitions p1 and p2.
add_synth_table <- function(ss, kind, access, variant = kind, ...) {
  if (kind == "plain") {
    ss$add_table(
      name_access = access,
      name_grouping = "synth",
      name_variant = variant,
      field_types = synth_fields[c("x", "n")],
      keys = "x",
      ...
    )
    return(ss$tables[[paste0(access, "_synth_", variant)]])
  }
  ss$add_partitionedtable(
    name_access = access,
    name_grouping = "synth",
    name_variant = variant,
    name_partitions = c("p1", "p2"),
    column_name_partition = "part",
    field_types = synth_fields,
    keys = c("x", "part"),
    ...
  )
  return(ss$partitionedtables[[paste0(access, "_synth_", variant)]])
}

synth_name <- function(table) {
  if (inherits(table, "DBPartitionedTableExtended_v9")) {
    return(table$table_name_base)
  }
  return(table$table_name)
}

# Every row of a table, ordered by x.
synth_rows <- function(table) {
  children <- if (inherits(table, "DBPartitionedTableExtended_v9")) {
    table$tables
  } else {
    list(table)
  }
  d <- data.table::rbindlist(lapply(children, function(t) {
    return(data.table::as.data.table(dplyr::collect(t$tbl())))
  }))
  if (nrow(d) > 0) {
    data.table::setorderv(d, "x")
  }
  return(d)
}

# Writes the rows old-1 and old-2 with DBI, outside the write path under test.
seed_synth_rows <- function(table) {
  suppressMessages(invisible(table$tbl()))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = table$dbconfig$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbAppendTable(
    con,
    table$table_name,
    data.frame(
      x = c("old-1", "old-2"),
      n = 0L,
      auto_last_updated_datetime = "2026-01-01 00:00:00"
    )
  )
  return(invisible(NULL))
}

refusal_cases <- expand.grid(
  kind = c("plain", "partitioned"),
  bad = c("pg", "outside"),
  stringsAsFactors = FALSE
)
bad_label <- c(
  pg = "an unreachable PostgreSQL",
  outside = "SQLite outside tempdir()"
)

for (i in seq_len(nrow(refusal_cases))) {
  kind <- refusal_cases$kind[i]
  bad <- refusal_cases$bad[i]

  test_that(
    paste("set_synthetic(TRUE) refuses a", kind, "table on", bad_label[[bad]]),
    {
      local_staging_db()
      spy <- local_connect_spy()
      path <- local_bad_access(bad)
      ss <- local_synth_system()
      add_synth_table(ss, kind, "bad")

      err <- synth_error(ss$set_synthetic(TRUE))

      expect_match(err, "Synthetic mode refuses", fixed = TRUE)
      expect_false(ss$synthetic_mode)
      expect_equal(spy$calls, 0L)
      if (bad == "outside") {
        expect_false(file.exists(path))
      }
    }
  )

  test_that(
    paste("populate_synthetic() refuses a", kind, "table on", bad_label[[bad]]),
    {
      local_staging_db()
      ss <- local_synth_system()
      # The mode is on before the table exists, so only the check of
      # populate_synthetic() stands between the table and drop_all_rows().
      ss$set_synthetic(TRUE)
      path <- local_bad_access(bad)
      table <- add_synth_table(ss, kind, "bad")
      spy <- local_connect_spy()

      err <- synth_error(ss$populate_synthetic(
        tables = synth_name(table),
        n = 10L
      ))

      expect_match(
        err,
        paste0("Synthetic mode refuses table ", synth_name(table)),
        fixed = TRUE
      )
      expect_equal(spy$calls, 0L)
      if (bad == "outside") {
        expect_false(file.exists(path))
      }
    }
  )
}

test_that("set_synthetic(TRUE) accepts SQLite under tempdir()", {
  local_staging_db()
  ss <- local_synth_system()
  add_synth_table(ss, "plain", "anon")
  add_synth_table(ss, "partitioned", "anon")
  expect_false(ss$synthetic_mode)
  ss$set_synthetic(TRUE)
  expect_true(ss$synthetic_mode)
  ss$set_synthetic(FALSE)
  expect_false(ss$synthetic_mode)
})

test_that("populate_synthetic() needs synthetic mode", {
  local_staging_db()
  ss <- local_synth_system()
  add_synth_table(ss, "plain", "anon")
  err <- synth_error(ss$populate_synthetic(tables = "anon_synth_plain"))
  expect_match(err, "needs synthetic mode", fixed = TRUE)
})

test_that("add_table() stores the schema-only default by table name", {
  local_staging_db()
  ss <- local_synth_system()
  add_synth_table(ss, "plain", "anon")
  add_synth_table(ss, "partitioned", "anon")
  expect_named(
    ss$synthetic_tables,
    c("anon_synth_plain", "anon_synth_partitioned")
  )
  plain <- ss$synthetic_tables$anon_synth_plain
  expect_s3_class(plain, "SyntheticTable")
  expect_equal(plain$field_types, c(x = "TEXT", n = "INTEGER"))
  expect_equal(plain$keys, "x")
  part <- ss$synthetic_tables$anon_synth_partitioned
  expect_equal(part$field_types, c(x = "TEXT", n = "INTEGER", part = "TEXT"))
  expect_equal(part$keys, c("x", "part"))

  d <- ss$synthetic("anon_synth_partitioned", n = 5L)
  expect_equal(d$part, c("p1", "p2", "p1", "p2", "p1"))
  expect_error(
    part$validate(data.table::copy(d)[, part := "p3"]),
    "invariant 'partition_known'"
  )
})

test_that("populate_synthetic fills every table and partition of a task", {
  local_staging_db()
  ss <- local_synth_system()
  plain <- add_synth_table(ss, "plain", "anon")
  part <- add_synth_table(ss, "partitioned", "anon")
  ss$add_task(
    name_grouping = "synth",
    name_action = "fill",
    for_each_plan = plnr::expand_list(plan = 1),
    action_fn_name = "identity",
    tables = list(a = plain, b = part)
  )
  ss$set_synthetic(TRUE)

  filled <- ss$populate_synthetic(task = "synth_fill", n = 20L, seed = 3L)

  expect_equal(filled, c(anon_synth_plain = 20L, anon_synth_partitioned = 20L))
  expect_equal(
    synth_rows(plain)[, c("x", "n")],
    data.table::setorderv(
      ss$synthetic("anon_synth_plain", n = 20L, seed = 3L),
      "x"
    )
  )
  expect_equal(
    vapply(part$tables, function(t) t$nrow(), numeric(1)),
    c(p1 = 10, p2 = 10)
  )

  # n = 1 with 2 partitions: the partitioned table gets 2 rows, 1 in each.
  filled <- ss$populate_synthetic(task = "synth_fill", n = 1L)
  expect_equal(filled, c(anon_synth_plain = 1L, anon_synth_partitioned = 2L))
  expect_equal(
    vapply(part$tables, function(t) t$nrow(), numeric(1)),
    c(p1 = 1, p2 = 1)
  )
})

test_that("a custom generator decides which partitions get rows", {
  local_staging_db()
  ss <- local_synth_system()
  part <- add_synth_table(
    ss,
    "partitioned",
    "anon",
    "custom",
    synthetic = csdb::SyntheticTable$new(
      name = "anon_synth_custom",
      field_types = c(x = "TEXT", n = "INTEGER", part = "TEXT"),
      keys = c("x", "part"),
      generator = function(profile, n, seed, world) {
        return(data.table::data.table(
          x = paste0("x_", seq_len(n)),
          n = seq_len(n),
          part = "p1"
        ))
      }
    )
  )
  # Old rows in p2, which the custom data does not name.
  p2 <- part$tables$p2
  suppressMessages(invisible(p2$tbl()))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = p2$dbconfig$db)
  DBI::dbAppendTable(
    con,
    p2$table_name,
    data.frame(
      x = c("old-1", "old-2"),
      n = 0L,
      part = "p2",
      auto_last_updated_datetime = "2026-01-01 00:00:00"
    )
  )
  DBI::dbDisconnect(con)
  expect_equal(p2$nrow(), 2)
  ss$set_synthetic(TRUE)

  ss$populate_synthetic(tables = "anon_synth_custom", n = 5L)

  expect_equal(part$tables$p1$nrow(), 5)
  expect_equal(part$tables$p2$nrow(), 0)
})

test_that("a task with one unsafe table writes nothing", {
  local_staging_db()
  ss <- local_synth_system()
  safe <- add_synth_table(ss, "plain", "anon", "safe")
  seed_synth_rows(safe)
  before <- synth_rows(safe)
  ss$set_synthetic(TRUE)
  local_bad_access("pg")
  unsafe <- add_synth_table(ss, "partitioned", "bad", "unsafe")
  # The safe table comes first, so a per-table check alone would rewrite it.
  ss$add_task(
    name_grouping = "synth",
    name_action = "mixed",
    for_each_plan = plnr::expand_list(plan = 1),
    action_fn_name = "identity",
    tables = list(a = safe, b = unsafe)
  )
  spy <- local_connect_spy()

  err <- synth_error(ss$populate_synthetic(task = "synth_mixed", n = 10L))

  expect_match(
    err,
    "Synthetic mode refuses table bad_synth_unsafe",
    fixed = TRUE
  )
  expect_equal(spy$calls, 0L)
  # Every column, auto_last_updated_datetime too, is as before the call.
  expect_equal(synth_rows(safe), before)
  expect_equal(before$x, c("old-1", "old-2"))
})

test_that("populate_synthetic() checks each table again before drop_all_rows()", {
  local_staging_db()
  ss <- local_synth_system()
  plain_types <- synth_fields[c("x", "n")]
  # The generator runs after the first check, and points the table at a
  # PostgreSQL server. Only the check before drop_all_rows() can see that.
  ss$add_table(
    name_access = "anon",
    name_grouping = "synth",
    name_variant = "moved",
    field_types = plain_types,
    keys = "x",
    synthetic = csdb::SyntheticTable$new(
      name = "anon_synth_moved",
      field_types = plain_types,
      keys = "x",
      generator = function(profile, n, seed, world) {
        moved <- ss$tables$anon_synth_moved
        moved$dbconfig$driver <- "PostgreSQL Unicode"
        return(csdb::simulate_from_profile(profile, n = n, seed = seed))
      }
    )
  )
  moved <- ss$tables$anon_synth_moved
  seed_synth_rows(moved)
  ss$set_synthetic(TRUE)
  spy <- local_connect_spy()

  err <- synth_error(ss$populate_synthetic(
    tables = "anon_synth_moved",
    n = 10L
  ))

  expect_match(
    err,
    "Synthetic mode refuses table anon_synth_moved",
    fixed = TRUE
  )
  expect_equal(moved$dbconfig$driver, "PostgreSQL Unicode")
  moved$dbconfig$driver <- "SQLite"
  expect_equal(synth_rows(moved)$x, c("old-1", "old-2"))
})

test_that("populate_synthetic inserts only validated data", {
  local_staging_db()
  ss <- local_synth_system()
  plain_types <- synth_fields[c("x", "n")]
  # The generator draws n from 0 to 100, which breaks this invariant.
  broken <- add_synth_table(
    ss,
    "plain",
    "anon",
    "broken",
    synthetic = csdb::SyntheticTable$new(
      name = "anon_synth_broken",
      field_types = plain_types,
      keys = "x",
      invariants = list(
        n_negative = function(d) {
          if (all(d$n < 0)) TRUE else "n is not negative."
        }
      )
    )
  )
  # This SyntheticTable has no keys, so it accepts a repeated x. The table has
  # the key x, and cs9 checks the data against the table.
  loose <- add_synth_table(
    ss,
    "plain",
    "anon",
    "loose",
    synthetic = csdb::SyntheticTable$new(
      name = "anon_synth_loose",
      field_types = plain_types,
      generator = function(profile, n, seed, world) {
        return(data.table::data.table(x = rep("same", n), n = seq_len(n)))
      }
    )
  )
  ok <- add_synth_table(ss, "plain", "anon", "ok")
  for (t in list(broken, loose, ok)) {
    seed_synth_rows(t)
  }
  ss$set_synthetic(TRUE)

  err_broken <- synth_error(ss$populate_synthetic(
    tables = c("anon_synth_ok", "anon_synth_broken"),
    n = 10L
  ))
  err_loose <- synth_error(ss$populate_synthetic(
    tables = c("anon_synth_ok", "anon_synth_loose"),
    n = 10L
  ))

  expect_match(err_broken, "invariant 'n_negative'", fixed = TRUE)
  expect_match(err_loose, "failed the keys check", fixed = TRUE)
  # A refused dataset writes nothing, also to the valid table before it.
  for (t in list(broken, loose, ok)) {
    expect_equal(synth_rows(t)$x, c("old-1", "old-2"))
  }

  ss$populate_synthetic(tables = "anon_synth_ok", n = 10L)
  expect_equal(synth_rows(ok)$x, sort(paste0("x_", 1:10)))
})

test_that("source returns the reader when the mode is off", {
  local_staging_db()
  ss <- local_synth_system()
  ss$add_source(
    "visits",
    reader = function() "real visits",
    simulator = function() "synthetic visits"
  )
  ss$add_source(
    "people",
    reader = function() "real people",
    simulator = function() "synthetic people"
  )
  expect_length(ss$sources, 2L)

  for (x in names(ss$sources)) {
    expect_identical(ss$source(x), ss$sources[[x]]$reader)
  }
  ss$set_synthetic(TRUE)
  ss$set_synthetic(FALSE)
  for (x in names(ss$sources)) {
    expect_identical(ss$source(x), ss$sources[[x]]$reader)
  }
  expect_equal(ss$source("visits")(), "real visits")
})

test_that("source returns the simulator when the mode is on", {
  local_staging_db()
  ss <- local_synth_system()
  ss$add_source(
    "visits",
    reader = function() "real visits",
    simulator = function() "synthetic visits"
  )
  ss$add_source(
    "people",
    reader = function() "real people",
    simulator = function() "synthetic people"
  )
  ss$set_synthetic(TRUE)

  for (x in names(ss$sources)) {
    expect_identical(ss$source(x), ss$sources[[x]]$simulator)
  }
  expect_equal(ss$source("people")(), "synthetic people")
  expect_error(ss$source("unknown"), "No source unknown")
})

test_that("set_synthetic_world() passes world to every generator", {
  local_staging_db()
  ss <- local_synth_system()
  add_synth_table(ss, "plain", "anon")
  ss$set_synthetic_world(list(n = c(7L, 8L)))
  expect_true(all(ss$synthetic("anon_synth_plain", n = 30L)$n %in% c(7L, 8L)))
  expect_error(ss$set_synthetic_world(list(1)), "named list")
})

test_that("a task runs on populated synthetic inputs", {
  local_staging_db()
  ss <- local_synth_system()
  ss$add_table(
    name_access = "anon",
    name_grouping = "synth",
    name_variant = "input",
    field_types = c(id = "INTEGER", n = "INTEGER"),
    keys = "id"
  )
  ss$add_partitionedtable(
    name_access = "anon",
    name_grouping = "synth",
    name_variant = "output",
    name_partitions = c("p1", "p2"),
    column_name_partition = "part",
    field_types = c(id = "INTEGER", doubled = "INTEGER", part = "TEXT"),
    keys = c("id", "part")
  )
  fns <- list(
    cs9synth_data = function(argset, tables) {
      return(list(input = dplyr::collect(tables$input$tbl())))
    },
    cs9synth_action = function(data, argset, tables) {
      d <- data$input
      tables$output$drop_all_rows()
      tables$output$insert_data(data.table::data.table(
        id = d$id,
        doubled = 2L * d$n,
        part = rep_len(c("p1", "p2"), nrow(d))
      ))
      return(invisible(NULL))
    }
  )
  for (x in names(fns)) {
    assign(x, fns[[x]], envir = globalenv())
  }
  withr::defer(rm(list = names(fns), envir = globalenv()))
  ss$add_task(
    name_grouping = "synth",
    name_action = "double",
    for_each_plan = plnr::expand_list(plan = 1),
    data_selector_fn_name = "cs9synth_data",
    action_fn_name = "cs9synth_action",
    tables = list(
      input = ss$tables$anon_synth_input,
      output = ss$partitionedtables$anon_synth_output
    )
  )
  ss$set_synthetic(TRUE)

  ss$populate_synthetic(task = "synth_double", n = 50L)
  suppressMessages(utils::capture.output(ss$run_task("synth_double")))

  input <- data.table::as.data.table(dplyr::collect(ss$tables$anon_synth_input$tbl()))
  output <- data.table::rbindlist(lapply(
    ss$partitionedtables$anon_synth_output$tables,
    function(t) data.table::as.data.table(dplyr::collect(t$tbl()))
  ))
  expect_equal(nrow(input), 50L)
  expect_equal(nrow(output), 50L)
  both <- merge(input, output, by = "id")
  expect_equal(nrow(both), 50L)
  expect_equal(both$doubled, 2L * both$n)
})

# run-task.sh ----

# The child MUST run the cs9 under test. Under load_all() that is this source
# tree, and the installed cs9 can be older. A user profile then loads the tree
# into the child. Under R CMD check the installed cs9 is the one under test.
local_child_cs9 <- function(envir = parent.frame()) {
  path <- getNamespaceInfo(asNamespace("cs9"), "path")
  if (!file.exists(file.path(path, "R", "synthetic_surveillance.R"))) {
    return(invisible(NULL))
  }
  profile <- withr::local_tempfile(fileext = ".R", .local_envir = envir)
  writeLines(
    paste0(
      "invisible(suppressMessages(devtools::load_all(",
      deparse(path),
      ", quiet = TRUE)))"
    ),
    profile
  )
  withr::local_envvar(R_PROFILE_USER = profile, .local_envir = envir)
  return(invisible(NULL))
}

# run-task.sh starts its worker with setsid, which Git Bash and Rtools bash on
# Windows do not have. There a stand-in runs the worker without a new session.
local_setsid <- function(envir = parent.frame()) {
  if (nzchar(Sys.which("setsid"))) {
    return(invisible(NULL))
  }
  bin <- withr::local_tempdir(.local_envir = envir)
  writeLines(c("#!/bin/sh", "exec \"$@\""), file.path(bin, "setsid"))
  Sys.chmod(file.path(bin, "setsid"), "0755")
  withr::local_path(bin, action = "prefix", .local_envir = envir)
  return(invisible(NULL))
}

# An implementation package with the task synthrun_copy, which copies
# anon_synthrun_input to anon_synthrun_output. The task writes the number of
# input rows it saw to seen-input-rows.txt in the package directory. A detached
# run has its own tempdir(), so .onLoad() puts the SQLite files there.
local_synthetic_runner_pkg <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  writeLines(
    c(
      "Package: cs9synthpkg",
      "Version: 0.0.1",
      "Title: A synthetic runner fixture",
      "Description: A synthetic runner fixture.",
      "License: MIT",
      "Imports: cs9, data.table, dplyr, plnr"
    ),
    file.path(dir, "DESCRIPTION")
  )
  writeLines('exportPattern(".")', file.path(dir, "NAMESPACE"))
  dir.create(file.path(dir, "R"))
  writeLines(
    c(
      "global <- new.env()",
      ".onLoad <- function(libname, pkgname) {",
      "  Sys.setenv(CS9_DBCONFIG_ACCESS = 'config/anon', CS9_DBCONFIG_DRIVER = 'SQLite',",
      "    CS9_DBCONFIG_DB_CONFIG = file.path(tempdir(), 'config.sqlite'),",
      "    CS9_DBCONFIG_DB_ANON = file.path(tempdir(), 'anon.sqlite'))",
      "  cs9::reload_db_config()",
      "  ss <- cs9::SurveillanceSystem_v9$new(name = 'synthrun')",
      "  for (v in c('input', 'output')) {",
      "    ss$add_table(name_access = 'anon', name_grouping = 'synthrun', name_variant = v,",
      "      field_types = c(id = 'INTEGER', n = 'INTEGER'), keys = 'id')",
      "  }",
      "  ss$add_task(name_grouping = 'synthrun', name_action = 'copy',",
      "    for_each_plan = plnr::expand_list(plan = 1),",
      "    data_selector_fn_name = 'cs9synthpkg::synth_data',",
      "    action_fn_name = 'cs9synthpkg::synth_action',",
      "    tables = list(input = ss$tables$anon_synthrun_input,",
      "      output = ss$tables$anon_synthrun_output))",
      "  global$ss <- ss",
      "}",
      "synth_data <- function(argset, tables) {",
      "  list(input = dplyr::collect(tables$input$tbl()))",
      "}",
      "synth_action <- function(data, argset, tables) {",
      "  writeLines(as.character(nrow(data$input)), 'seen-input-rows.txt')",
      "  tables$output$drop_all_rows()",
      "  tables$output$insert_data(data.table::as.data.table(data$input)[, c('id', 'n')])",
      "}"
    ),
    file.path(dir, "R", "synth.R")
  )
  return(dir)
}


# The lines of a file, or `default` when the path is missing or the file does
# not exist. A runner that failed then gives failed expectations, not errors.
read_or <- function(path, default = character(0)) {
  if (length(path) != 1 || is.na(path) || !file.exists(path)) {
    return(default)
  }
  return(readLines(path, warn = FALSE))
}

# TRUE when the status file appears within `seconds`. FALSE for no path.
wait_for_status <- function(status, seconds = 300) {
  if (length(status) != 1 || is.na(status)) {
    return(FALSE)
  }
  for (i in seq_len(seconds * 2)) {
    if (file.exists(status)) {
      break
    }
    Sys.sleep(0.5)
  }
  return(file.exists(status))
}

# One value of run-task.sh's output, such as LOG, or NA.
runner_field <- function(out, name) {
  hit <- grep(paste0("^", name, "="), out, value = TRUE)
  if (length(hit) != 1) {
    return(NA_character_)
  }
  return(sub(paste0("^", name, "="), "", hit))
}

test_that("run-task.sh writes the synthetic calls only with --synthetic", {
  local_setsid()
  pkg <- withr::local_tempdir()
  writeLines(
    c("Package: cs9synthfake", "Version: 0.0.1"),
    file.path(pkg, "DESCRIPTION")
  )
  runs <- withr::local_tempdir()
  bin <- withr::local_tempdir()
  fake <- file.path(bin, "Rscript")
  writeLines(c("#!/bin/sh", "exit 0"), fake)
  Sys.chmod(fake, "0755")
  script <- system.file("scripts", "run-task.sh", package = "cs9")

  for (synthetic in c(TRUE, FALSE)) {
    args <- c(script, "--dir", pkg, "--runs", runs, "--rscript", fake)
    if (synthetic) {
      args <- c(args, "--synthetic")
    }
    task <- if (synthetic) "t_synthetic" else "t_real"
    out <- suppressWarnings(
      system2("bash", shQuote(c(args, task)), stdout = TRUE, stderr = TRUE)
    )
    exit <- attr(out, "status")
    info <- paste(out, collapse = "\n")
    log <- runner_field(out, "LOG")
    status <- if (is.na(log)) NA_character_ else sub("[.]log$", ".status", log)
    done <- wait_for_status(status, 60)
    driver <- read_or(
      if (is.na(log)) NA_character_ else sub("[.]log$", ".R", log)
    )
    calls <- c(
      "global$ss$set_synthetic(TRUE)",
      paste0("global$ss$populate_synthetic(task = \"", task, "\")")
    )
    run_line <- which(
      driver == paste0("global$ss$run_task(\"", task, "\", run_key = run_key)")
    )

    expect_equal(if (is.null(exit)) 0L else exit, 0L, info = info)
    expect_true(done, info = info)
    expect_equal(calls %in% driver, rep(synthetic, 2), info = info)
    expect_equal("SYNTHETIC: yes" %in% read_or(log), synthetic, info = info)
    expect_length(run_line, 1L)
    if (synthetic) {
      # The synthetic calls come before run_task().
      expect_true(isTRUE(all(match(calls, driver) < run_line[1])))
    }
  }
})

test_that("run_task_detached(synthetic = TRUE) runs the task on synthetic inputs", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  local_staging_db()
  local_child_cs9()
  local_setsid()
  pkg <- local_synthetic_runner_pkg()
  runs <- withr::local_tempdir()

  # An error of run_task_detached() becomes the info of the expectations.
  started <- tryCatch(
    suppressMessages(run_task_detached(
      "synthrun_copy",
      package_dir = pkg,
      run_dir = runs,
      synthetic = TRUE
    )),
    error = function(e) list(error = conditionMessage(e))
  )
  done <- wait_for_status(if (is.null(started$status)) NA else started$status)
  log <- read_or(if (is.null(started$log)) NA else started$log)
  info <- paste(c(started$error, log), collapse = "\n")
  status <- trimws(read_or(
    if (is.null(started$status)) NA else started$status,
    NA
  ))
  seen <- as.numeric(read_or(file.path(pkg, "seen-input-rows.txt"), 0))

  expect_true(done, info = info)
  expect_equal(status, "0", info = info)
  expect_match(
    paste(log, collapse = "\n"),
    "SYNTHETIC: yes",
    fixed = TRUE,
    info = info
  )
  expect_gt(seen, 0)
  expect_equal(seen, 1000)
})
