# What a run with a run_key stages, and what it refuses.
#
# - A table of the config access is never staged. It is written directly,
#   also when a task lists it, while the task's other tables are staged.
# - A keyed run that cannot stage stops before any write. Before round 2 of
#   phase 21 it gave a warning and wrote its tables directly.
# - The automatic writes at the end of each plan are staged like any other.
# - A staging name stays within the 63-byte PostgreSQL limit.
#
# The fixtures are in helper-run-staging.R.

test_that("a cs9 config table in a task's tables is written directly while the others are staged", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  for (p in as.character(1:4)) {
    ctl$hooks[[p]] <- function(tables, argset) {
      tables$cfg$insert_data(config_log_row(paste0("cfg-", argset$plan)))
      if (argset$plan == 2) {
        record_live_nrow(ctl)(tables, argset)
        writeLines(config_log_messages(), file.path(ctl$dir, "cfg-seen-2"))
      }
      return(invisible(NULL))
    }
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, cfg = config$tables$config_log)),
    run_key = "with-config"
  )

  expect_null(err)
  # In plan 2 config_log already held the rows of plans 1 and 2, while the
  # staged table still held the old rows.
  seen_file <- file.path(ctl$dir, "cfg-seen-2")
  cfg_seen <- if (file.exists(seen_file)) readLines(seen_file) else NA
  expect_equal(grep("^cfg-", cfg_seen, value = TRUE), c("cfg-1", "cfg-2"))
  expect_equal(seen_x(ctl, 2), old_rows()$x)
  expect_equal(live_rows(out), expected_rows(1:4))
  expect_equal(grep("^cfg-", config_log_messages(), value = TRUE), paste0("cfg-", 1:4))
  expect_false(any(grepl("config_log_cs9stg", made_tables())))
})

test_that("a keyed run that cannot stage stops and writes nothing", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A second anon table in another SQLite file: the task's tables are then in
  # two databases, and one transaction cannot publish both.
  other_config <- db
  other_config$db <- file.path(dirname(db$db), "other.sqlite")
  other <- DBTableExtended_v9$new(
    dbconfig = other_config,
    table_name = "anon_stage_other",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(other$disconnect())
  seed_old_rows(other)
  ctl <- local_stage_ctl()
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$other$insert_data(stage_rows_of(argset$plan))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, other = other)),
    run_key = "two-databases"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(error_text(err), "more than one database", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
  expect_equal(live_rows(other), old_rows())
})

test_that("two tables with one name stop a keyed run before any write", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A table with the same name in another SQLite file.
  twin_config <- db
  twin_config$db <- file.path(dirname(db$db), "twin.sqlite")
  twin <- DBTableExtended_v9$new(
    dbconfig = twin_config,
    table_name = "anon_stage_plain",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(twin$disconnect())
  seed_old_rows(twin)
  ctl <- local_stage_ctl()
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$twin$insert_data(stage_rows_of(argset$plan))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, twin = twin)),
    run_key = "twins"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(
    error_text(err),
    "two tables share the name anon_stage_plain",
    fixed = TRUE
  )
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
  expect_equal(live_rows(twin), old_rows())
})

test_that("an output table and a config table with one name stop a keyed run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A table of the config access with the name of the output table.
  cfg <- DBTableExtended_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "anon_stage_plain",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(cfg$disconnect())
  seed_old_rows(cfg)
  ctl <- local_stage_ctl()
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$cfg$insert_data(stage_rows_of(argset$plan))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, cfg = cfg)),
    run_key = "config-twin"
  )

  expect_match(
    error_text(err),
    "two tables share the name anon_stage_plain",
    fixed = TRUE
  )
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
  expect_equal(live_rows(cfg), old_rows())
})

test_that("a keyed run with a table that cs9 cannot stage stops and writes nothing", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A plain csdb table in the anon database: not a cs9 table class, and not a
  # table of the config access.
  plain <- csdb::DBTable_v9$new(
    dbconfig = db,
    table_name = "anon_stage_csdb",
    field_types = c(x = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(plain$disconnect())
  suppressMessages(invisible(plain$tbl()))
  con <- DBI::dbConnect(RSQLite::SQLite(), db$db)
  DBI::dbAppendTable(con, "anon_stage_csdb", data.frame(x = "old", n = 0L))
  DBI::dbDisconnect(con)
  ctl <- local_stage_ctl()
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$plain$insert_data(data.table::data.table(x = "new", n = 1L))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, plain = plain)),
    run_key = "plain-csdb"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(error_text(err), "anon_stage_csdb", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
  expect_equal(dplyr::collect(plain$tbl())$x, "old")
})

test_that("a plain csdb wrapper of a staged table stops a keyed run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A second object for the same live table: same name, database and schema,
  # but a plain csdb::DBTable_v9 that cs9 cannot stage. It has the fields of
  # the live table, so csdb keeps the table as it is.
  wrapper <- csdb::DBTable_v9$new(
    dbconfig = db,
    table_name = "anon_stage_plain",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER", auto_last_updated_datetime = "DATETIME"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(wrapper$disconnect())
  ctl <- local_stage_ctl()
  ctl$drop <- FALSE
  ctl$rows <- function(plan) NULL
  ctl$hooks[["1"]] <- function(tables, argset) {
    tables$wrapper$insert_data(data.table::data.table(
      x = "direct",
      part = "p1",
      n = 1L,
      auto_last_updated_datetime = "2026-01-01 00:00:00"
    ))
    return(invisible(NULL))
  }

  # The cs9 table comes first, so a deduplication by live table would keep it
  # and drop the wrapper.
  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, wrapper = wrapper)),
    run_key = "wrapper"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(error_text(err), "anon_stage_plain", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
})

test_that("an object that is not a table stops a keyed run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  # Not a table, but with a disconnect(), which Task$run() calls on every
  # object of the task's tables after the plans. Without the check the run
  # would then go on and publish.
  junk <- list(disconnect = function() invisible(NULL))

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, junk = junk)),
    run_key = "junk"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(error_text(err), "'junk' (an object of class list)", fixed = TRUE)
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
})

test_that("a task table with the name of a staging table stops a keyed run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  # A table with the name of the staging table of `out`. Since 26.10.15 a cs9
  # table class refuses such a name, and since 26.10.17 only cs9's own config
  # tables pass the first pass besides the cs9 classes. The test therefore
  # adds the clash to config$tables for its duration.
  clash <- csdb::DBTable_v9$new(
    dbconfig = config$dbconfigs$config,
    table_name = "anon_stage_plain_cs9stg",
    field_types = c(x = "TEXT", part = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  withr::defer(clash$disconnect())
  config$tables$clash <- clash
  withr::defer(config$tables$clash <- NULL)
  suppressMessages(invisible(clash$tbl()))
  con <- DBI::dbConnect(RSQLite::SQLite(), config$dbconfigs$config$db)
  DBI::dbAppendTable(con, "anon_stage_plain_cs9stg", as.data.frame(old_rows()))
  DBI::dbDisconnect(con)
  ctl <- local_stage_ctl()
  ctl$hooks[["2"]] <- function(tables, argset) {
    tables$clash$insert_data(stage_rows_of(argset$plan))
    return(invisible(NULL))
  }

  err <- run_attempt(
    new_stage_task(out, tables = list(out = out, clash = clash)),
    run_key = "clash"
  )

  expect_match(error_text(err), "cannot stage its writes", fixed = TRUE)
  expect_match(
    error_text(err),
    "anon_stage_plain_cs9stg collides with the staging table of anon_stage_plain",
    fixed = TRUE
  )
  expect_equal(stage_calls(ctl, 4), c(0L, 0L, 0L, 0L))
  expect_equal(live_rows(out), old_rows())
  expect_equal(live_rows(clash), old_rows())
})

test_that("a cs9 table refuses a name that ends in the staging suffix", {
  db <- local_staging_db()
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  refused <- function(fn) {
    return(tryCatch(
      {
        fn()
        ""
      },
      error = function(e) conditionMessage(e)
    ))
  }
  fields <- c(x = "TEXT", n = "INTEGER")
  plain <- refused(function() {
    ss$add_table(
      name_access = "anon",
      name_grouping = "other",
      name_variant = "cs9stg",
      field_types = fields,
      keys = "x"
    )
  })
  partitioned <- refused(function() {
    ss$add_partitionedtable(
      name_access = "anon",
      name_grouping = "other",
      name_variant = "cs9stg",
      name_partitions = c("p1", "p2"),
      column_name_partition = "part",
      field_types = fields,
      keys = "x"
    )
  })
  partition <- refused(function() {
    ss$add_partitionedtable(
      name_access = "anon",
      name_grouping = "other",
      name_variant = "data",
      name_partitions = c("p1", "cs9stg"),
      column_name_partition = "part",
      field_types = fields,
      keys = "x"
    )
  })

  reserved <- "names ending in _cs9stg are reserved for staging"
  expect_match(plain, reserved, fixed = TRUE)
  expect_match(partitioned, reserved, fixed = TRUE)
  expect_match(partition, reserved, fixed = TRUE)
  expect_null(ss$tables$anon_other_cs9stg)
  expect_null(ss$partitionedtables$anon_other_cs9stg)
})

test_that("run_today() is exported, and is the date of the first attempt in a keyed run", {
  expect_true("run_today" %in% getNamespaceExports("cs9"))
  # Outside a run with a run_key it is today's date.
  expect_equal(run_today(), Sys.Date())

  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  ctl <- local_stage_ctl()
  ctl$hooks[["2"]] <- function(tables, argset) {
    cat(format(run_today()), "\n", file = file.path(ctl$dir, "run-today"), append = TRUE)
    return(invisible(NULL))
  }
  task <- new_stage_task(out)
  day <- as.Date("2026-03-01")
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(day)
    },
    .package = "lubridate"
  )
  ctl$fail <- 3L
  err1 <- run_attempt(task, run_key = "run-today")
  day <- as.Date("2026-03-02")
  ctl$fail <- integer()
  # Plan 2 is done after attempt 1, so make it run again: a new key would
  # start a new run, so plan 1 records the date instead.
  ctl$hooks[["1"]] <- ctl$hooks[["2"]]
  err2 <- run_attempt(task, run_key = "run-today")

  expect_s3_class(err1, "error")
  expect_null(err2)
  seen <- readLines(file.path(ctl$dir, "run-today"))
  # Attempt 1 on 2026-03-01 (plan 2), attempt 2 on 2026-03-02 (plan 1).
  expect_equal(trimws(seen), c("2026-03-01", "2026-03-01"))
})

test_that("Task$run() with a run_key builds its plans again on every attempt", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$plans <- function() 1:4
  task <- new_stage_task(out, planned = TRUE)
  run_direct <- function() {
    return(tryCatch(
      {
        suppressWarnings(suppressMessages(task$run(cores = 1, run_key = "k")))
        NULL
      },
      error = function(e) e
    ))
  }

  ctl$fail <- 3L
  err1 <- run_direct()
  expect_match(error_text(err1), "plan 3 failed on purpose", fixed = TRUE)

  # The planner now gives other plans. The call passes no replan.
  ctl$plans <- function() c(1L, 2L, 3L, 5L)
  ctl$fail <- integer()
  err2 <- run_direct()

  expect_match(
    error_text(err2),
    "the plan list changed between attempts of one run",
    fixed = TRUE
  )
  expect_equal(live_rows(out), old_rows())
})

test_that("run_task() passes its run_key to the task run", {
  db <- local_staging_db()
  out <- new_stage_out(db, "plain")
  seed_old_rows(out)
  ctl <- local_stage_ctl()
  ctl$hooks[["2"]] <- record_live_nrow(ctl)
  ss <- SurveillanceSystem_v9$new(name = "cs9test")
  ss$add_task(
    name_grouping = "stage",
    name_action = "viass",
    for_each_plan = plnr::expand_list(plan = 1:4),
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = list(out = out)
  )

  err <- tryCatch(
    {
      suppressWarnings(suppressMessages(ss$run_task("stage_viass", run_key = "k")))
      NULL
    },
    error = function(e) e
  )

  expect_null(err)
  # The run staged: plan 2 read the old rows, and the run with key "k" is
  # marked as published.
  expect_equal(seen_x(ctl, 2), old_rows()$x)
  run_row <- store_rows("cs9_run", "k")
  expect_equal(nrow(run_row), 1L)
  expect_true(all(!is.na(run_row$published_datetime)))
  expect_equal(live_rows(out), expected_rows(1:4))
})

for (mode in c("insert", "upsert")) {
  test_that(
    paste0("a keyed run stages the ", mode, " at the end of each plan"),
    {
      db <- local_staging_db()
      out <- new_stage_out(db, "plain")
      seed_old_rows(out)
      ctl <- local_stage_ctl()
      ctl$return_rows <- TRUE
      ctl$hooks[["2"]] <- record_live_nrow(ctl)

      err <- run_attempt(
        new_stage_task(
          out,
          tables = list(out = out, output = out),
          insert_at_end = mode == "insert",
          upsert_at_end = mode == "upsert"
        ),
        run_key = paste0("plan-end-", mode)
      )

      expect_null(err)
      # cs9 wrote the rows of plan 1 at the end of plan 1. Plan 2 still read
      # the old rows, so that write was staged.
      expect_equal(seen_x(ctl, 2), old_rows()$x)
      expect_equal(live_rows(out), expected_rows(1:4))
    }
  )
}

test_that("a staging name stays within 63 bytes and stays unique", {
  short <- "anon_norsyss_data"
  expect_equal(run_staging_name(short), "anon_norsyss_data_cs9stg")

  prefix <- strrep("a", 52)
  long_1 <- paste0(prefix, "_one_table")
  long_2 <- paste0(prefix, "_two_table")
  name_1 <- run_staging_name(long_1)
  name_2 <- run_staging_name(long_2)

  expect_lte(nchar(name_1, type = "bytes"), 63L)
  expect_lte(nchar(name_2, type = "bytes"), 63L)
  expect_true(endsWith(name_1, "_cs9stg"))
  expect_false(identical(name_1, name_2))
  expect_identical(run_staging_name(long_1), name_1)
})

test_that("a staging name is cut by bytes and keeps whole characters", {
  # "\u00f8" is 1 character and 2 bytes in UTF-8. Each live name is 60 bytes.
  live_1 <- paste0("anon_", strrep("\u00f8", 25), "_one_")
  live_2 <- paste0("anon_", strrep("\u00f8", 25), "_two_")
  expect_equal(nchar(live_1, type = "bytes"), 60L)
  name_1 <- run_staging_name(live_1)
  name_2 <- run_staging_name(live_2)

  expect_lte(nchar(name_1, type = "bytes"), 63L)
  expect_lte(nchar(name_2, type = "bytes"), 63L)
  expect_true(validUTF8(name_1))
  expect_true(validUTF8(name_2))
  expect_true(endsWith(name_1, "_cs9stg"))
  expect_false(identical(name_1, name_2))
  expect_identical(run_staging_name(live_1), name_1)
})
