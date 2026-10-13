# The task runners pass a run_key to run_task(): run_task_detached(),
# run_task_sequentially_as_callr_bg_using_load_all() (through TaskJob) and
# run_task_sequentially_as_rstudio_job_using_load_all(). A keyed run stages
# and publishes; a run without a key writes its tables directly, as before.
#
# The key crosses the process boundary as an argument: a literal in the call to
# run_task() of the child's script, or a command-line argument of the detached
# Rscript. It never travels in an environment variable.
#
# The fixtures are in helper-run-staging.R.

# An implementation package with one task that rebuilds anon_runner_out in two
# plans. Plan 2 writes the rows it reads to seen-in-plan-2.txt in the working
# directory, which is the package directory.
local_runner_pkg <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  writeLines(
    c(
      "Package: cs9runnerpkg",
      "Version: 0.0.1",
      "Title: A task runner fixture",
      "Description: A task runner fixture.",
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
      "  ss <- cs9::SurveillanceSystem_v9$new(name = 'runner')",
      "  ss$add_table(name_access = 'anon', name_grouping = 'runner', name_variant = 'out',",
      "    field_types = c(x = 'TEXT', n = 'INTEGER'), keys = 'x')",
      "  ss$add_task(name_grouping = 'runner', name_action = 'rebuild',",
      "    for_each_plan = plnr::expand_list(plan = 1:2),",
      "    data_selector_fn_name = 'cs9runnerpkg::runner_data',",
      "    action_fn_name = 'cs9runnerpkg::runner_action',",
      "    tables = list(out = ss$tables$anon_runner_out))",
      "  global$ss <- ss",
      "}",
      "runner_data <- function(argset, tables) list()",
      "runner_action <- function(data, argset, tables) {",
      "  if (isTRUE(argset$first_analysis)) tables$out$drop_all_rows()",
      "  tables$out$insert_data(data.table::data.table(",
      "    x = paste0('new-', argset$plan), n = as.integer(argset$plan)))",
      "  if (argset$plan == 2) {",
      "    writeLines(sort(dplyr::collect(tables$out$tbl())$x), 'seen-in-plan-2.txt')",
      "  }",
      "}"
    ),
    file.path(dir, "R", "runner.R")
  )
  return(dir)
}

# The live table of the fixture package, with 2 old rows.
seed_runner_out <- function(db) {
  out <- DBTableExtended_v9$new(
    dbconfig = db,
    table_name = "anon_runner_out",
    field_types = c(x = "TEXT", n = "INTEGER"),
    keys = "x",
    validator_field_types = csdb::validator_field_types_blank,
    validator_field_contents = csdb::validator_field_contents_blank
  )
  suppressMessages(invisible(out$tbl()))
  con <- DBI::dbConnect(RSQLite::SQLite(), db$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbAppendTable(
    con,
    "anon_runner_out",
    data.frame(
      x = c("old-1", "old-2"),
      n = 0L,
      auto_last_updated_datetime = "2026-01-01 00:00:00"
    )
  )
  out$disconnect()
  return(invisible(NULL))
}

runner_live_x <- function(db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  return(sort(DBI::dbGetQuery(con, "SELECT x FROM anon_runner_out")$x))
}

# The callr child loads the fixture package with devtools::load_all(), and
# that package imports the INSTALLED cs9. The test needs both.
skip_without_child_cs9 <- function() {
  testthat::skip_if_not_installed("devtools")
  testthat::skip_if_not_installed("callr")
  # The child MUST load the cs9 under test. An older cs9 in the library has no
  # run_key, so the test skips when the versions differ.
  installed <- callr::r(function() {
    if (!requireNamespace("cs9", quietly = TRUE)) {
      return("")
    }
    return(as.character(utils::packageVersion("cs9")))
  })
  testthat::skip_if_not(
    identical(installed, as.character(utils::packageVersion("cs9"))),
    paste0("the cs9 of a child process is '", installed, "', not this cs9")
  )
  return(invisible(NULL))
}

for (keyed in c(TRUE, FALSE)) {
  label <- if (keyed) {
    "stages with a run_key"
  } else {
    "writes directly without a run_key"
  }
  test_that(paste("the callr runner", label), {
    skip_on_cran()
    skip_without_child_cs9()
    db <- local_staging_db()
    seed_runner_out(db)
    pkg <- local_runner_pkg()
    withr::local_dir(pkg)
    key <- if (keyed) "callr key" else NULL

    job <- suppressMessages(
      run_task_sequentially_as_callr_bg_using_load_all(
        "runner_rebuild",
        run_key = key
      )
    )
    utils::capture.output(job$wait(timeout_s = 300))
    log <- paste(readLines(job$log_path, warn = FALSE), collapse = "\n")

    expect_false(job$is_alive())
    expect_equal(runner_live_x(db), c("new-1", "new-2"), info = log)
    seen <- if (file.exists("seen-in-plan-2.txt")) {
      readLines("seen-in-plan-2.txt")
    } else {
      NA
    }
    if (keyed) {
      # Plan 2 read the old rows, and the run with the key is published.
      expect_equal(seen, c("old-1", "old-2"), info = log)
      run_row <- store_rows("cs9_run", "callr key")
      expect_equal(nrow(run_row), 1L)
      expect_true(all(!is.na(run_row$published_datetime)))
    } else {
      # Plan 2 read the row that plan 1 wrote directly. No staging table.
      expect_equal(seen, c("new-1", "new-2"), info = log)
      expect_equal(made_tables(), character())
    }
  })
}

for (keyed in c(TRUE, FALSE)) {
  label <- if (keyed) "passes its run_key" else "passes NULL without a run_key"
  test_that(paste("the RStudio job runner", label), {
    db <- local_staging_db()
    out <- new_stage_out(db, "plain")
    seed_old_rows(out)
    ctl <- local_stage_ctl()
    ctl$hooks[["2"]] <- record_live_nrow(ctl)
    ss <- SurveillanceSystem_v9$new(name = "cs9test")
    ss$add_task(
      name_grouping = "stage",
      name_action = "rstudio",
      for_each_plan = plnr::expand_list(plan = 1:4),
      action_fn_name = "cs9stage_action",
      data_selector_fn_name = "cs9stage_data",
      tables = list(out = out)
    )
    job <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(
      jobRunScript = function(path, ...) {
        job$path <- path
        return(invisible(NULL))
      },
      .package = "rstudioapi"
    )
    key <- if (keyed) "rstudio key" else NULL

    run_task_sequentially_as_rstudio_job_using_load_all(
      "stage_rstudio",
      ss_prefix = "cs9test_runner$ss",
      run_key = key
    )
    withr::defer(unlink(job$path))
    # The job script without its devtools::load_all(), run in this process.
    exprs <- parse(job$path, keep.source = FALSE)
    is_load_all <- vapply(
      exprs,
      function(e) {
        return(is.call(e) && identical(e[[1]], quote(devtools::load_all)))
      },
      logical(1)
    )
    env <- new.env(parent = globalenv())
    env$cs9test_runner <- new.env(parent = emptyenv())
    env$cs9test_runner$ss <- ss
    err <- tryCatch(
      {
        utils::capture.output(suppressMessages(
          for (e in exprs[!is_load_all]) {
            eval(e, env)
          }
        ))
        NULL
      },
      error = function(e) e
    )

    expect_null(err)
    expect_equal(live_rows(out), expected_rows(1:4))
    if (keyed) {
      expect_equal(seen_x(ctl, 2), old_rows()$x)
      expect_equal(nrow(store_rows("cs9_run", "rstudio key")), 1L)
    } else {
      expect_equal(seen_x(ctl, 2), stage_rows_of(1)$x)
      expect_equal(made_tables(), character())
    }
  })
}

# run-task.sh passes the key to the Rscript that --rscript names. A fake
# Rscript records its arguments, so the test sees what the child receives. The
# real runs below cover run_task_detached() end to end.
for (keyed in c(TRUE, FALSE)) {
  label <- if (keyed) {
    "passes --key to the child Rscript"
  } else {
    "passes no key without --key"
  }
  test_that(paste("run-task.sh", label), {
    skip_on_os("windows")
    skip_if(Sys.which("setsid") == "", "run-task.sh needs setsid")
    pkg <- withr::local_tempdir()
    writeLines(c("Package: cs9detachedpkg", "Version: 0.0.1"), file.path(pkg, "DESCRIPTION"))
    runs <- withr::local_tempdir()
    bin <- withr::local_tempdir()
    fake <- file.path(bin, "Rscript")
    writeLines(
      c("#!/bin/sh", "printf '%s\\n' \"$@\" > \"$(dirname \"$0\")/args.txt\""),
      fake
    )
    Sys.chmod(fake, "0755")
    script <- system.file("scripts", "run-task.sh", package = "cs9")
    args <- c(script, "--dir", pkg, "--runs", runs, "--rscript", fake)
    if (keyed) {
      args <- c(args, "--key", "detached key")
    }

    out <- system2("bash", shQuote(c(args, "t1")), stdout = TRUE, stderr = TRUE)
    field <- function(name) {
      return(sub(paste0("^", name, "="), "", grep(paste0("^", name, "="), out, value = TRUE)))
    }
    for (i in 1:50) {
      if (file.exists(field("STATUS"))) break
      Sys.sleep(0.2)
    }
    driver_path <- sub("[.]log$", ".R", field("LOG"))
    recorded <- readLines(file.path(bin, "args.txt"))
    driver <- readLines(driver_path)

    expect_true(file.exists(field("STATUS")))
    expect_true(any(grepl("run_task(\"t1\", run_key = run_key)", driver, fixed = TRUE)))
    if (keyed) {
      expect_equal(recorded, c(driver_path, "detached key"))
      expect_true(any(readLines(field("LOG")) == "run_key:  detached key"))
    } else {
      expect_equal(recorded, driver_path)
      expect_true(any(readLines(field("LOG")) == "run_key:  none"))
    }
  })
}

# A real detached run: run-task.sh starts a real Rscript child, which loads
# the fixture package and the installed cs9 under test.
for (keyed in c(TRUE, FALSE)) {
  label <- if (keyed) {
    "stages a real detached run with a run_key"
  } else {
    "writes directly in a real detached run without a run_key"
  }
  test_that(paste("run_task_detached()", label), {
    skip_on_cran()
    skip_on_os("windows")
    skip_if(Sys.which("setsid") == "", "run-task.sh needs setsid")
    skip_without_child_cs9()
    db <- local_staging_db()
    seed_runner_out(db)
    pkg <- local_runner_pkg()
    runs <- withr::local_tempdir()
    key <- if (keyed) "detached key" else NULL

    run <- suppressMessages(
      run_task_detached("runner_rebuild", package_dir = pkg, run_dir = runs, run_key = key)
    )
    # The status file appears only when the child ends. At most 300 s.
    for (i in 1:600) {
      if (file.exists(run$status)) break
      Sys.sleep(0.5)
    }
    log <- paste(readLines(run$log, warn = FALSE), collapse = "\n")

    expect_true(file.exists(run$status), info = log)
    expect_equal(trimws(readLines(run$status)), "0", info = log)
    expect_equal(runner_live_x(db), c("new-1", "new-2"), info = log)
    seen_file <- file.path(pkg, "seen-in-plan-2.txt")
    seen <- if (file.exists(seen_file)) readLines(seen_file) else NA
    if (keyed) {
      # Plan 2 read the old rows, and the run with the key is published.
      expect_equal(seen, c("old-1", "old-2"), info = log)
      run_row <- store_rows("cs9_run", "detached key")
      expect_equal(nrow(run_row), 1L)
      expect_true(all(!is.na(run_row$published_datetime)))
    } else {
      # Plan 2 read the row that plan 1 wrote directly. No staging table.
      expect_equal(seen, c("new-1", "new-2"), info = log)
      expect_equal(made_tables(), character())
    }
  })
}

test_that("a runner stops on a run_key that it cannot pass", {
  stopped <- function(fn) {
    return(tryCatch(
      {
        fn()
        ""
      },
      error = function(e) conditionMessage(e)
    ))
  }
  bad <- "a\tb"
  expect_match(
    stopped(function() TaskJob$new("t", run_key = bad)),
    "run_key MUST be",
    fixed = TRUE
  )
  expect_match(
    stopped(function() {
      run_task_detached("t", package_dir = tempdir(), run_key = bad)
    }),
    "run_key MUST be",
    fixed = TRUE
  )
  expect_match(
    stopped(function() {
      run_task_sequentially_as_rstudio_job_using_load_all("t", run_key = bad)
    }),
    "run_key MUST be",
    fixed = TRUE
  )
})
