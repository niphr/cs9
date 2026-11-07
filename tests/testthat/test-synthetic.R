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

# The seed that populate_synthetic() gives a table, from the documented rule:
# seed plus the first 7 hexadecimal digits of the md5 of the table name,
# modulo .Machine$integer.max.
synth_seed_of <- function(seed, table_name) {
  f <- withr::local_tempfile()
  writeBin(charToRaw(table_name), f)
  offset <- strtoi(substr(unname(tools::md5sum(f)), 1, 7), 16L)
  return(as.integer((seed + offset) %% .Machine$integer.max))
}

# Every row of each plain table, every column, ordered by every column.
synth_snapshot <- function(tables) {
  return(lapply(tables, function(t) {
    d <- data.table::as.data.table(dplyr::collect(t$tbl()))
    data.table::setorderv(d, names(d))
    return(d)
  }))
}

# A generator of the columns x and n. Row i gets x = paste0(prefix, i). With
# partitions, row i also gets part = partitions[(i - 1) %% 2 + 1].
synth_prefix_generator <- function(prefix, partitions = NULL) {
  force(prefix)
  force(partitions)
  return(function(profile, n, seed, world) {
    d <- data.table::data.table(x = paste0(prefix, seq_len(n)), n = seq_len(n))
    if (!is.null(partitions)) {
      data.table::set(d, j = "part", value = rep_len(partitions, nrow(d)))
    }
    return(d)
  })
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
      ss$synthetic(
        "anon_synth_plain",
        n = 20L,
        seed = synth_seed_of(3L, "anon_synth_plain")
      ),
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

# fixes from the synthetic-data trial ----

test_that("item 8: one populate fills two partitions holding the same series", {
  local_staging_db()
  ss <- local_synth_system()
  # The keys omit the partition column, as in the tables of norsyss.cs9.
  add_series_table <- function(variant, synthetic = NULL) {
    ss$add_partitionedtable(
      name_access = "anon",
      name_grouping = "synth",
      name_variant = variant,
      name_partitions = c("p1", "p2"),
      column_name_partition = "part",
      field_types = synth_fields,
      keys = "x",
      synthetic = synthetic
    )
    return(ss$partitionedtables[[paste0("anon_synth_", variant)]])
  }
  # The series x_1, x_2 and x_3, each in p1 and in p2.
  same_series <- function(profile, n, seed, world) {
    return(data.table::data.table(
      x = rep(paste0("x_", 1:3), 2),
      n = 1:6,
      part = rep(c("p1", "p2"), each = 3)
    ))
  }
  add_series_table("series_default")
  part <- add_series_table("series_spec", list(generator = same_series))
  default <- ss$synthetic_tables$anon_synth_series_default
  spec <- ss$synthetic_tables$anon_synth_series_spec
  for (t in part$tables) {
    suppressMessages(invisible(t$tbl()))
  }
  ss$set_synthetic(TRUE)

  err_default <- synth_error(default$validate(same_series()))
  err_spec <- synth_error(ss$populate_synthetic(
    tables = "anon_synth_series_spec",
    n = 6L
  ))

  expect_equal(default$keys, c("x", "part"))
  expect_equal(spec$keys, c("x", "part"))
  expect_equal(err_default, "")
  expect_equal(err_spec, "")
  expect_equal(
    vapply(part$tables, function(t) t$nrow(), numeric(1)),
    c(p1 = 3, p2 = 3)
  )
  rows <- synth_rows(part)
  expect_equal(rows$x[rows$part == "p1"], paste0("x_", 1:3))
  expect_equal(rows$x[rows$part == "p2"], paste0("x_", 1:3))
})

test_that("item 9: each table gets its own seed from seed and the table name", {
  local_staging_db()
  ss <- local_synth_system()
  a <- add_synth_table(ss, "plain", "anon", "seed_a")
  b <- add_synth_table(ss, "plain", "anon", "seed_b")
  names_ab <- c("anon_synth_seed_a", "anon_synth_seed_b")
  ss$set_synthetic(TRUE)

  ss$populate_synthetic(tables = names_ab, n = 50L, seed = 1L)
  first <- lapply(list(a, b), function(t) synth_rows(t)[, c("x", "n")])
  ss$populate_synthetic(tables = names_ab, n = 50L, seed = 1L)
  second <- lapply(list(a, b), function(t) synth_rows(t)[, c("x", "n")])

  # The keys are the same, row i gives x_i. The value streams differ.
  expect_equal(first[[1]]$x, first[[2]]$x)
  expect_false(identical(first[[1]]$n, first[[2]]$n))
  expect_equal(second, first)
  for (i in 1:2) {
    expect_equal(
      first[[i]],
      data.table::setorderv(
        ss$synthetic(
          names_ab[i],
          n = 50L,
          seed = synth_seed_of(1L, names_ab[i])
        ),
        "x"
      )
    )
  }
})

test_that("item 10: a spec list or a SyntheticTable works for both table kinds", {
  local_staging_db()
  ss <- local_synth_system()
  n_not_negative <- function(data) {
    return(all(data$n >= 0))
  }
  profile <- csdb::profile_schema(synth_fields[c("x", "n")], "x")
  dir <- withr::local_tempdir()
  add_synth_table(
    ss,
    "plain",
    "anon",
    "spec_plain",
    validator_field_contents = n_not_negative,
    synthetic = list(
      generator = synth_prefix_generator("g_"),
      invariants = list(n_known = function(d) all(!is.na(d$n))),
      profile = profile
    )
  )
  add_synth_table(
    ss,
    "partitioned",
    "anon",
    "spec_part",
    validator_field_contents = n_not_negative,
    synthetic = list(
      generator = synth_prefix_generator("g_", c("p1", "p2")),
      dir = dir
    )
  )
  given <- list(
    anon_synth_st_plain = csdb::SyntheticTable$new(
      name = "anon_synth_st_plain",
      field_types = synth_fields[c("x", "n")],
      keys = "x",
      generator = synth_prefix_generator("g_")
    ),
    anon_synth_st_part = csdb::SyntheticTable$new(
      name = "anon_synth_st_part",
      field_types = synth_fields[c("x", "n", "part")],
      keys = c("x", "part"),
      generator = synth_prefix_generator("g_", c("p1", "p2"))
    )
  )
  add_synth_table(ss, "plain", "anon", "st_plain", synthetic = given[[1]])
  add_synth_table(ss, "partitioned", "anon", "st_part", synthetic = given[[2]])
  spec_of <- function(x) ss$synthetic_tables[[x]]$.__enclos_env__$private$spec
  ss$set_synthetic(TRUE)

  filled <- ss$populate_synthetic(
    tables = c(
      "anon_synth_spec_plain",
      "anon_synth_spec_part",
      "anon_synth_st_plain",
      "anon_synth_st_part"
    ),
    n = 4L
  )

  # The spec-built contract is the contract of the table.
  plain <- spec_of("anon_synth_spec_plain")
  expect_equal(plain$field_types, c(x = "TEXT", n = "INTEGER"))
  expect_equal(plain$keys, "x")
  expect_identical(plain$validator_field_contents, n_not_negative)
  expect_named(plain$invariants, "n_known")
  expect_identical(plain$profile, profile)
  part <- spec_of("anon_synth_spec_part")
  expect_equal(part$field_types, c(x = "TEXT", n = "INTEGER", part = "TEXT"))
  expect_equal(part$keys, c("x", "part"))
  expect_identical(part$validator_field_contents, n_not_negative)
  expect_named(part$invariants, "partition_known")
  expect_equal(part$dir, dir)
  # A SyntheticTable is stored as given.
  for (x in names(given)) {
    expect_identical(ss$synthetic_tables[[x]], given[[x]])
  }
  expect_equal(
    filled,
    c(
      anon_synth_spec_plain = 4L,
      anon_synth_spec_part = 4L,
      anon_synth_st_plain = 4L,
      anon_synth_st_part = 4L
    )
  )
  all_tables <- c(ss$tables, ss$partitionedtables)
  for (x in names(filled)) {
    expect_equal(synth_rows(all_tables[[x]])$x, paste0("g_", 1:4))
  }
})

test_that("item 10: a spec with an unknown element or with dir and profile is refused", {
  local_staging_db()
  ss <- local_synth_system()
  err_unknown <- synth_error(add_synth_table(
    ss,
    "plain",
    "anon",
    "spec_bad",
    synthetic = list(generatr = synth_prefix_generator("g_"))
  ))
  err_both <- synth_error(add_synth_table(
    ss,
    "plain",
    "anon",
    "spec_both",
    synthetic = list(
      dir = withr::local_tempdir(),
      profile = csdb::profile_schema(synth_fields[c("x", "n")], "x")
    )
  ))
  expect_match(err_unknown, "unknown elements: 'generatr'", fixed = TRUE)
  expect_match(err_both, "MUST NOT have both dir and profile", fixed = TRUE)
})

# Three plain tables. join_a and join_b share the key x, and join_y has the
# key y. join_b2 has the same x as join_a.
add_join_tables <- function(ss) {
  tables <- list(
    a = add_synth_table(
      ss,
      "plain",
      "anon",
      "join_a",
      synthetic = list(generator = synth_prefix_generator("a_"))
    ),
    b = add_synth_table(
      ss,
      "plain",
      "anon",
      "join_b",
      synthetic = list(generator = synth_prefix_generator("b_"))
    ),
    b2 = add_synth_table(
      ss,
      "plain",
      "anon",
      "join_b2",
      synthetic = list(generator = synth_prefix_generator("a_"))
    )
  )
  ss$add_table(
    name_access = "anon",
    name_grouping = "synth",
    name_variant = "join_y",
    field_types = c(y = "TEXT", n = "INTEGER"),
    keys = "y"
  )
  tables$y <- ss$tables$anon_synth_join_y
  return(tables)
}

test_that("item 11: populate_synthetic(task = ) refuses tables that share keys and do not join", {
  local_staging_db()
  ss <- local_synth_system()
  t <- add_join_tables(ss)
  seed_synth_rows(t$a)
  seed_synth_rows(t$b)
  ss$add_task(
    name_grouping = "synth",
    name_action = "nojoin",
    for_each_plan = plnr::expand_list(plan = 1),
    action_fn_name = "identity",
    tables = list(a = t$a, b = t$b, y = t$y)
  )
  before <- synth_snapshot(t[c("a", "b", "y")])
  ss$set_synthetic(TRUE)

  err <- synth_error(ss$populate_synthetic(task = "synth_nojoin", n = 5L))

  expect_match(
    err,
    "the tables anon_synth_join_a and anon_synth_join_b have no row in common",
    fixed = TRUE
  )
  expect_match(err, "shared key columns x.", fixed = TRUE)
  # Every column of every table, auto_last_updated_datetime too, is as before.
  expect_equal(synth_snapshot(t[c("a", "b", "y")]), before)
  expect_equal(before$a$x, c("old-1", "old-2"))
})

test_that("item 11: a pair with no shared key column is not checked", {
  local_staging_db()
  ss <- local_synth_system()
  t <- add_join_tables(ss)
  ss$add_task(
    name_grouping = "synth",
    name_action = "join",
    for_each_plan = plnr::expand_list(plan = 1),
    action_fn_name = "identity",
    tables = list(a = t$a, b2 = t$b2, y = t$y)
  )
  ss$set_synthetic(TRUE)

  err <- synth_error(ss$populate_synthetic(task = "synth_join", n = 5L))

  # join_y shares no key column with join_a or join_b2, and holds y_1 to y_5.
  expect_equal(err, "")
  expect_equal(
    vapply(t[c("a", "b2", "y")], function(x) x$nrow(), numeric(1)),
    c(a = 5, b2 = 5, y = 5)
  )
})

test_that("item 11: naming the inputs excludes an output table from the join check", {
  local_staging_db()
  ss <- local_synth_system()
  # Two inputs that join on x, and an output with the key x and other values.
  io <- list(
    in1 = add_synth_table(
      ss,
      "plain",
      "anon",
      "io_in1",
      synthetic = list(generator = synth_prefix_generator("a_"))
    ),
    in2 = add_synth_table(
      ss,
      "plain",
      "anon",
      "io_in2",
      synthetic = list(generator = synth_prefix_generator("a_"))
    ),
    out = add_synth_table(
      ss,
      "plain",
      "anon",
      "io_out",
      synthetic = list(generator = synth_prefix_generator("z_"))
    )
  )
  add_synth_table(ss, "plain", "anon", "io_other")
  for (t in io) {
    seed_synth_rows(t)
  }
  ss$add_task(
    name_grouping = "synth",
    name_action = "io",
    for_each_plan = plnr::expand_list(plan = 1),
    action_fn_name = "identity",
    tables = io
  )
  before <- synth_snapshot(io)
  ss$set_synthetic(TRUE)
  inputs <- c("anon_synth_io_in1", "anon_synth_io_in2")

  err_task <- synth_error(ss$populate_synthetic(task = "synth_io", n = 5L))
  after_task <- synth_snapshot(io)
  err_stranger <- synth_error(ss$populate_synthetic(
    task = "synth_io",
    tables = c(inputs, "anon_synth_io_other"),
    n = 5L
  ))
  err_inputs <- synth_error(ss$populate_synthetic(
    task = "synth_io",
    tables = inputs,
    n = 5L
  ))
  after_inputs <- synth_snapshot(io)

  # The task alone checks the output too, and refuses.
  expect_match(
    err_task,
    "the tables anon_synth_io_in1 and anon_synth_io_out have no row in common",
    fixed = TRUE
  )
  expect_equal(after_task, before)
  expect_match(
    err_stranger,
    "not in the tables of the task: anon_synth_io_other.",
    fixed = TRUE
  )
  # The named inputs are filled, and the output is as before.
  expect_equal(err_inputs, "")
  expect_equal(after_inputs$in1$x, paste0("a_", 1:5))
  expect_equal(after_inputs$in2$x, paste0("a_", 1:5))
  expect_equal(after_inputs$out, before$out)
})

test_that("item 12: add_source() refuses a reader and a simulator with different arguments", {
  local_staging_db()
  ss <- local_synth_system()

  err_missing <- synth_error(ss$add_source(
    "visits",
    reader = function(date_from, date_to) "real",
    simulator = function(date_from) "synthetic"
  ))
  err_order <- synth_error(ss$add_source(
    "visits",
    reader = function(date_from, date_to) "real",
    simulator = function(date_to, date_from) "synthetic"
  ))
  err_same <- synth_error(ss$add_source(
    "visits",
    reader = function(date_from, date_to) "real",
    simulator = function(date_from, date_to) "synthetic"
  ))

  expect_match(
    err_missing,
    "reader has (date_from, date_to), simulator has (date_from).",
    fixed = TRUE
  )
  expect_match(
    err_order,
    "reader has (date_from, date_to), simulator has (date_to, date_from).",
    fixed = TRUE
  )
  expect_equal(err_same, "")
  expect_named(ss$sources, "visits")
  expect_equal(ss$source("visits")("2026-01-01", "2026-01-07"), "real")
})

test_that("populate_synthetic(n = NULL) passes NULL to a custom generator and fills every partition with the default", {
  local_staging_db()
  ss <- local_synth_system()
  # Each custom generator records the n it gets.
  seen <- new.env()
  recording <- function(name, partitions = NULL) {
    inner <- synth_prefix_generator("g_", partitions)
    return(function(profile, n, seed, world) {
      seen[[name]] <- list(n = n)
      return(inner(profile, 4L, seed, world))
    })
  }
  add_synth_table(
    ss,
    "plain",
    "anon",
    "null_plain",
    synthetic = list(generator = recording("plain"))
  )
  add_synth_table(
    ss,
    "partitioned",
    "anon",
    "null_part",
    synthetic = list(generator = recording("part", c("p1", "p2")))
  )
  default <- add_synth_table(ss, "partitioned", "anon", "null_default")
  ss$set_synthetic(TRUE)

  filled <- ss$populate_synthetic(
    tables = c(
      "anon_synth_null_plain",
      "anon_synth_null_part",
      "anon_synth_null_default"
    ),
    n = NULL
  )

  expect_null(seen$plain$n)
  expect_null(seen$part$n)
  # csdb::simulate_from_profile() gives 1000 rows for n = NULL.
  expect_equal(
    filled,
    c(
      anon_synth_null_plain = 4L,
      anon_synth_null_part = 4L,
      anon_synth_null_default = 1000L
    )
  )
  expect_equal(
    vapply(default$tables, function(t) t$nrow(), numeric(1)),
    c(p1 = 500, p2 = 500)
  )
  # With more partitions than that size, the default draws one row for each.
  partitions <- paste0("p", 1:1001)
  d <- synthetic_partition_generator("part", partitions)(
    csdb::profile_schema(c(x = "TEXT", part = "TEXT"), c("x", "part")),
    NULL,
    1L,
    NULL
  )
  expect_equal(nrow(d), 1001L)
  expect_setequal(d$part, partitions)
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
