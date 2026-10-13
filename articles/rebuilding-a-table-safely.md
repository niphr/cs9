# Rebuilding a table safely

A task often rebuilds its output table: plan 1 empties the table, and
each plan then adds its rows. cs9 keeps every change in a staging table
until the last plan succeeds. It then copies the changes into the table
in one step. A reader therefore sees the complete old table or the
complete new table, never a part of one.

cs9 stages a run only when the run has a `run_key`. A scheduler can pass
one to `run_task()`. A run without a `run_key` writes to the table at
once, as before.

This vignette runs a small task against SQLite and draws what a reader
sees. A chunk calls
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) on each claim, so
the vignette fails to build when cs9 stops keeping it.

## The task

The table holds counts per week. Before the run it holds 5 old rows,
with `n = 0`. The task rebuilds it with 3 plans, one per week. Plan 1
empties the table, and each plan adds the row of its week.

``` r
library(data.table)

db_dir <- file.path(tempdir(), "cs9-rebuilding")
unlink(db_dir, recursive = TRUE)
dir.create(db_dir)
Sys.setenv(
  CS9_AUTO = "0",
  CS9_PATH = db_dir,
  CS9_DBCONFIG_ACCESS = "config/anon",
  CS9_DBCONFIG_DRIVER = "SQLite",
  CS9_DBCONFIG_DB_CONFIG = file.path(db_dir, "config.sqlite"),
  CS9_DBCONFIG_DB_ANON = file.path(db_dir, "anon.sqlite")
)
cs9::reload_db_config()

ss <- cs9::SurveillanceSystem_v9$new(name = "rebuild")
ss$add_table(
  name_access = "anon",
  name_grouping = "rebuild",
  name_variant = "counts",
  field_types = c(week = "INTEGER", n = "INTEGER"),
  keys = "week"
)
counts <- ss$tables$anon_rebuild_counts
counts$insert_data(data.table(week = 1:5, n = 0L))
```

After it writes, the action records two numbers. One is the rows that a
reader sees in the table. The other is the rows that wait in the staging
table. `staged_tbl()` reads the rows that the run staged.

``` r
records <- list()
attempt <- "none"
fail_week <- 0L

record <- function(step, published, staged) {
  records[[length(records) + 1L]] <<- data.table(
    attempt = attempt, step = step, published = published, staged = staged
  )
}

rebuild_data <- function(argset, tables) {
  return(list())
}

rebuild_action <- function(data, argset, tables) {
  if (argset$first_analysis) {
    tables$counts$drop_all_rows()
  }
  tables$counts$insert_data(data.table(week = argset$week, n = 10L * argset$week))
  record(
    step = paste("plan", argset$week),
    published = tables$counts$nrow(),
    staged = nrow(dplyr::collect(tables$counts$staged_tbl()))
  )
  if (argset$week == fail_week) {
    stop("week ", argset$week, " failed")
  }
}

ss$add_task(
  name_grouping = "rebuild",
  name_action = "counts",
  for_each_plan = plnr::expand_list(week = 1:3),
  data_selector_fn_name = "rebuild_data",
  action_fn_name = "rebuild_action",
  tables = list(counts = counts)
)
```

## A run that succeeds

``` r
attempt <- "A run that succeeds"
record("before", counts$nrow(), 0L)
ss$run_task("rebuild_counts", run_key = "week 40")
record("after", counts$nrow(), 0L)
```

``` r
run_a <- rbindlist(records)
stopifnot(
  "a reader sees the 5 old rows during the run" = all(run_a[step != "after", published] == 5),
  "the staging table grows by one row per plan" = identical(run_a[grepl("plan", step), staged], 1:3),
  "after the run a reader sees the 3 new rows" = run_a[step == "after", published] == 3,
  "the new rows hold the new counts" = identical(dplyr::collect(counts$tbl())$n, c(10L, 20L, 30L))
)
```

The diagram shows the two numbers at each step. Until the last plan
succeeds, a reader sees the old table. cs9 then publishes the staged
rows, and a reader sees the new table.

    #> Warning: The `label.size` argument of `geom_label()` is deprecated as of ggplot2 3.5.0.
    #> ℹ Please use the `linewidth` argument instead.
    #> This warning is displayed once per session.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    #> generated.

![Line chart of one run with 3 plans. The rows that a reader sees stay
at the 5 old rows before the run and after plans 1, 2 and 3. They drop
to the 3 new rows after the run. The rows in the staging table rise to
1, 2 and 3 after plans 1, 2 and 3. They fall to 0 after the
run.](rebuilding-a-table-safely_files/figure-html/success-diagram-1.png)

## A run that fails, and its retry

Here plan 3 stops with an error. The run publishes nothing, so a reader
still sees the 5 old rows.

The retry runs the task again with the same `run_key`. That attempt
resumes the run. Plan 1 runs again, because it can set values that later
plans read. The last plan, here plan 3, also runs on every attempt, so a
check in it cannot be skipped. Plan 2 finished on the first attempt, so
it is skipped. The rows that plan 3 staged on the first attempt are
removed first, so each row is published once.

``` r
counts$drop_all_rows()
counts$insert_data(data.table(week = 1:5, n = 0L))
records <- list()

attempt <- "Attempt 1: plan 3 fails"
fail_week <- 3L
record("before", counts$nrow(), 0L)
failed <- tryCatch(ss$run_task("rebuild_counts", run_key = "week 41"), error = function(e) e)
# staged_tbl() works only during a run. Between two attempts nothing removes
# the staged rows, so the count after the failure repeats the last count.
record("after", counts$nrow(), tail(rbindlist(records)$staged, 1))

attempt <- "Attempt 2: the same run_key"
fail_week <- 0L
record("before", counts$nrow(), tail(rbindlist(records)$staged, 1))
ss$run_task("rebuild_counts", run_key = "week 41")
record("after", counts$nrow(), 0L)
```

``` r
run_b <- rbindlist(records)
first <- run_b[attempt == "Attempt 1: plan 3 fails"]
second <- run_b[attempt == "Attempt 2: the same run_key"]
stopifnot(
  "attempt 1 stops with the error of plan 3" = grepl("week 3 failed", conditionMessage(failed)),
  "during and after attempt 1 a reader sees the 5 old rows" = all(first$published == 5),
  "attempt 2 keeps the staged row of plan 2" = second[step == "plan 1", staged] == 2,
  "attempt 2 skips plan 2" = !"plan 2" %in% second$step,
  "attempt 2 runs plans 1 and 3" = all(c("plan 1", "plan 3") %in% second$step),
  "after attempt 2 a reader sees the 3 new rows" = second[step == "after", published] == 3,
  "each new row is in the table once" = identical(dplyr::collect(counts$tbl())$week, 1:3)
)
```

![Two line charts, one per attempt of one run. In attempt 1 the rows
that a reader sees stay at the 5 old rows at every step, also after plan
3 fails. The staging table holds 1, 2 and 3 rows after plans 1, 2 and 3,
and keeps 3 rows after the failure. In attempt 2 the staging table
starts with the 3 rows of attempt 1. There is no point for plan 2, which
is skipped. The staging table holds 2 rows after plan 1 and 3 rows after
plan 3. The rows that a reader sees stay at 5 until the end of attempt
2, and then drop to the 3 new
rows.](rebuilding-a-table-safely_files/figure-html/failure-diagram-1.png)

## What you need to know when you write a task

You do not change your task code. A run with a `run_key` stages the
writes of every table in the task’s `tables`. A write outside such a run
goes to the table at once. These rules apply during a run with a
`run_key`:

- Plans depend only on the task’s settings and published tables.
- `tbl()` and [`nrow()`](https://rdrr.io/r/base/nrow.html) read the
  published table. `staged_tbl()` reads the rows that the run staged.
- A run MUST write each row once. Two writes of one row stop the run,
  also when both are upserts.
- A plan MUST NOT delete rows that another plan wrote in the same run.
  Plan 1 MAY delete, because it runs before every other plan.
- Plan 1 and the last plan run on every attempt. Any other plan that
  finished on an earlier attempt is skipped.
- Use
  [`run_today()`](https://niphr.github.io/cs9/reference/run_today.md) in
  a planner instead of
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html). On a retry after
  midnight it still gives the date of the first attempt.
- Write to a partitioned table through the table, not through one of its
  partitions.
- SQL that a task sends on the connection of a table goes to the table
  at once. cs9 does not stage it.

A run with a new `run_key` removes what a failed run of the same task
left in the staging table.

Staging works on PostgreSQL and SQLite, when all tables of the task are
in one database. Otherwise a run with a `run_key` stops with an error
before it writes. A table of the config access is never staged.

On PostgreSQL, two runs of one task with a `run_key` cannot overlap. The
second run stops with an error while the first run is alive.
