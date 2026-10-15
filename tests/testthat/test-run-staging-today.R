# A keyed run of a task that stages nothing keeps the date of its first
# attempt. Such a task has no table, or config tables only. Before 26.10.19
# run_begin() stored no state for it. run_today() then read the clock again,
# so a retry on a later day planned and wrote for another date.

# The cs9_run* and *_cs9stg tables in one SQLite file.
bookkeeping_in <- function(db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  return(grep("^cs9_run|_cs9stg$", DBI::dbListTables(con), value = TRUE))
}

# The rows of cs9_run in the config database, ordered by run_key.
config_run_rows <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), config$dbconfigs$config$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "cs9_run")) {
    return(data.frame())
  }
  return(DBI::dbGetQuery(con, "SELECT * FROM cs9_run ORDER BY run_key"))
}

# A task with the fixed plans 1 and 2 that writes no table. On each attempt
# plan 1 records run_today() and argset$today in one line.
new_tablefree_task <- function(ctl, tables) {
  ctl$rows <- function(plan) NULL
  ctl$drop <- FALSE
  ctl$hooks[["1"]] <- function(tables, argset) {
    cat(
      format(run_today()),
      " ",
      format(argset$today),
      "\n",
      sep = "",
      file = file.path(ctl$dir, "seen-today"),
      append = TRUE
    )
    return(invisible(NULL))
  }
  return(task_from_config_v9(
    name_grouping = "stage",
    name_action = "tablefree",
    for_each_plan = plnr::expand_list(plan = 1:2),
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = tables
  ))
}

seen_today <- function(ctl) {
  return(trimws(readLines(file.path(ctl$dir, "seen-today"))))
}

shapes <- list(
  "a task without tables" = function() list(),
  "a task with config tables only" = function() {
    return(list(cfg = config$tables$config_log))
  }
)

for (shape in names(shapes)) {
  for (shared in c(FALSE, TRUE)) {
    layout <- if (shared) {
      "a config database shared with anon"
    } else {
      "its own config database"
    }
    test_that(
      paste0(
        "a keyed retry of ",
        shape,
        " keeps the date of attempt 1, in ",
        layout
      ),
      {
        local_staging_db(shared = shared)
        ctl <- local_stage_ctl()
        task <- new_tablefree_task(ctl, shapes[[shape]]())
        day <- as.Date("2026-03-01")
        testthat::local_mocked_bindings(
          today = function(tzone = "") {
            return(day)
          },
          .package = "lubridate"
        )

        ctl$fail <- 2L
        err1 <- run_attempt(task, run_key = "tablefree")
        day <- as.Date("2026-03-02")
        ctl$fail <- integer()
        err2 <- run_attempt(task, run_key = "tablefree")

        expect_match(error_text(err1), "plan 2 failed on purpose", fixed = TRUE)
        expect_null(err2)
        # Plan 1 of attempt 1 on 2026-03-01, plan 1 of attempt 2 on 2026-03-02.
        expect_equal(
          seen_today(ctl),
          c("2026-03-01 2026-03-01", "2026-03-01 2026-03-01")
        )
        # The run stages nothing and resumes nothing: each attempt runs both plans.
        expect_equal(stage_calls(ctl, 2), c(2L, 2L))
        # The date is in cs9_run of the config database, the only bookkeeping
        # table of the run.
        expect_equal(bookkeeping_in(config$dbconfigs$config$db), "cs9_run")
        if (!shared) {
          expect_equal(bookkeeping_in(config$dbconfigs$anon$db), character())
        }
        rows <- config_run_rows()
        expect_equal(rows$task, task$name)
        expect_equal(rows$run_key, "tablefree")
        expect_equal(rows$run_date, "2026-03-01")
        expect_true(is.na(rows$published_datetime))
        # After the run, run_today() reads the clock again.
        expect_equal(run_today(), as.Date("2026-03-02"))
      }
    )
  }
}

test_that("a new key of a task without tables gets the date of its own first attempt", {
  local_staging_db()
  ctl <- local_stage_ctl()
  task <- new_tablefree_task(ctl, list())
  day <- as.Date("2026-03-01")
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(day)
    },
    .package = "lubridate"
  )

  err1 <- run_attempt(task, run_key = "first key")
  day <- as.Date("2026-03-02")
  err2 <- run_attempt(task, run_key = "second key")

  expect_null(err1)
  expect_null(err2)
  expect_equal(
    seen_today(ctl),
    c("2026-03-01 2026-03-01", "2026-03-02 2026-03-02")
  )
  rows <- config_run_rows()
  expect_equal(rows$run_key, c("first key", "second key"))
  expect_equal(rows$run_date, c("2026-03-01", "2026-03-02"))
})

test_that("a run without a key of a task without tables reads the clock on each attempt", {
  local_staging_db()
  ctl <- local_stage_ctl()
  task <- new_tablefree_task(ctl, list())
  day <- as.Date("2026-03-01")
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(day)
    },
    .package = "lubridate"
  )

  ctl$fail <- 2L
  err1 <- run_attempt(task)
  day <- as.Date("2026-03-02")
  ctl$fail <- integer()
  err2 <- run_attempt(task)

  expect_match(error_text(err1), "plan 2 failed on purpose", fixed = TRUE)
  expect_null(err2)
  expect_equal(
    seen_today(ctl),
    c("2026-03-01 2026-03-01", "2026-03-02 2026-03-02")
  )
  expect_equal(made_tables(), character())
})

# On a driver other than PostgreSQL or SQLite cs9 stores no date, as at
# bba6f38. SQL Server is not available here, so the driver check is mocked.
test_that("a keyed run of a task without tables reads the clock on a config database that is neither PostgreSQL nor SQLite", {
  local_staging_db()
  ctl <- local_stage_ctl()
  task <- new_tablefree_task(ctl, list())
  day <- as.Date("2026-03-01")
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(day)
    },
    .package = "lubridate"
  )
  testthat::local_mocked_bindings(
    run_driver_supported = function(driver) {
      return(FALSE)
    }
  )

  ctl$fail <- 2L
  err1 <- run_attempt(task, run_key = "other driver")
  day <- as.Date("2026-03-02")
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "other driver")

  expect_match(error_text(err1), "plan 2 failed on purpose", fixed = TRUE)
  expect_null(err2)
  expect_equal(
    seen_today(ctl),
    c("2026-03-01 2026-03-01", "2026-03-02 2026-03-02")
  )
  expect_equal(stage_calls(ctl, 2), c(2L, 2L))
  expect_equal(made_tables(), character())
})

# A run inside the action of another run keeps its own date. The inner task
# has no key, so it reads the clock. The outer run keeps the date of its first
# attempt before and after the inner run.
test_that("a run without a key inside a keyed run of a task without tables reads the clock, and the outer run keeps its date", {
  local_staging_db()
  ctl <- local_stage_ctl()
  task <- new_tablefree_task(ctl, list())
  day <- as.Date("2026-03-01")
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(day)
    },
    .package = "lubridate"
  )
  seen <- file.path(ctl$dir, "seen-nested")
  assign(
    "cs9stage_inner_action",
    function(data, argset, tables) {
      cat(
        "inner",
        format(run_today()),
        format(argset$today),
        "\n",
        file = seen,
        append = TRUE
      )
      return(invisible(NULL))
    },
    envir = globalenv()
  )
  withr::defer(rm("cs9stage_inner_action", envir = globalenv()))
  inner <- task_from_config_v9(
    name_grouping = "stage",
    name_action = "inner",
    for_each_plan = plnr::expand_list(plan = 1),
    action_fn_name = "cs9stage_inner_action",
    data_selector_fn_name = "cs9stage_data",
    tables = list()
  )
  ctl$hooks[["1"]] <- function(tables, argset) {
    cat("outer before", format(run_today()), "\n", file = seen, append = TRUE)
    suppressMessages(inner$run(cores = 1, replan = TRUE))
    cat("outer after", format(run_today()), "\n", file = seen, append = TRUE)
    return(invisible(NULL))
  }

  ctl$fail <- 2L
  err1 <- run_attempt(task, run_key = "nested")
  day <- as.Date("2026-03-02")
  ctl$fail <- integer()
  err2 <- run_attempt(task, run_key = "nested")

  expect_match(error_text(err1), "plan 2 failed on purpose", fixed = TRUE)
  expect_null(err2)
  expect_equal(
    trimws(readLines(seen)),
    c(
      "outer before 2026-03-01",
      "inner 2026-03-01 2026-03-01",
      "outer after 2026-03-01",
      "outer before 2026-03-01",
      "inner 2026-03-02 2026-03-02",
      "outer after 2026-03-01"
    )
  )
  expect_equal(run_today(), as.Date("2026-03-02"))
})

# A documented limit. In the production layout the config and the anon access
# share one database and schema, so a table-free run and a staged run share
# cs9_run. A staged run of the same task name with another key fences the row
# of the table-free key, and a retry of that key gets a new date.
test_that("a staged run of another key of the same task removes the stored date of a table-free key", {
  db <- local_staging_db(shared = TRUE)
  out <- new_stage_out(db, "plain")
  ctl <- local_stage_ctl()
  tablefree <- new_tablefree_task(ctl, list())
  staged <- task_from_config_v9(
    name_grouping = "stage",
    name_action = "tablefree",
    for_each_plan = plnr::expand_list(plan = 1:2),
    action_fn_name = "cs9stage_action",
    data_selector_fn_name = "cs9stage_data",
    tables = list(out = out)
  )
  day <- as.Date("2026-03-01")
  testthat::local_mocked_bindings(
    today = function(tzone = "") {
      return(day)
    },
    .package = "lubridate"
  )

  ctl$fail <- 2L
  err1 <- run_attempt(tablefree, run_key = "A")
  day <- as.Date("2026-03-02")
  ctl$fail <- integer()
  err2 <- run_attempt(staged, run_key = "B")
  err3 <- run_attempt(tablefree, run_key = "A")

  expect_match(error_text(err1), "plan 2 failed on purpose", fixed = TRUE)
  expect_null(err2)
  expect_null(err3)
  expect_equal(staged$name, tablefree$name)
  expect_equal(
    seen_today(ctl),
    c(
      "2026-03-01 2026-03-01",
      "2026-03-02 2026-03-02",
      "2026-03-02 2026-03-02"
    )
  )
  rows <- config_run_rows()
  expect_equal(rows$run_key, c("A", "B"))
  expect_equal(rows$run_date, c("2026-03-02", "2026-03-02"))
})
