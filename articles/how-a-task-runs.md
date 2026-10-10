# How a task runs

This vignette states the nine rules that `Task$run()` imposes on a task.
Each section runs a small task against SQLite. A chunk calls
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) on the rule, so
the vignette fails to build when cs9 stops keeping it.

## Setup

This chunk points the `config` and `anon` access levels at two SQLite
files under [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
[`vignette("installation")`](https://niphr.github.io/cs9/articles/installation.md)
describes the environment variables.

``` r
library(data.table)

db_dir <- file.path(tempdir(), "cs9-how-a-task-runs")
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

ss <- cs9::SurveillanceSystem_v9$new(name = "rules")
```

## A run in one diagram

The diagram shows one run of a task with 4 plans, 2 analyses per plan
and 2 cores. In each plan, `pull` is the call to the data selector. `A1`
and `A2` are the calls to the action for analysis 1 and 2. The sections
below state each rule that the diagram shows.

The vignette records this run when it builds, and draws the diagram from
that record. A hidden chunk stops the build when the record breaks a
rule that the diagram shows. The run forks, so the diagram appears only
on Linux and macOS, in a session that is not interactive.

![Diagram of one run of a task with 4 plans and 2 analyses per plan on 2
cores. It has one row for the main process and one row for each of 2
forked workers. In the main process, run_task() builds the plans, and
plan 1 then pulls its data and runs analyses 1 and 2. The main process
then forks, and each worker runs one middle plan, plan 2 or plan 3,
while the main process waits. After both workers end, plan 4, the last
plan, runs in the main
process.](how-a-task-runs_files/figure-html/overview-diagram-1.png)

## A plan is one data pull

cs9 calls the data selector once per plan. Every analysis of that plan
then gets the same `data`. This task has 3 plans, one per location, and
3 analyses per plan, one per age group. A counter records each call to
the data selector.

``` r
locations <- c("oslo", "bergen", "tromso")
ages <- c("000_014", "015_064", "065p")
counter <- new.env()
counter$pulls <- 0L
counter$seen <- list()

pull_data <- function(argset, tables) {
  counter$pulls <- counter$pulls + 1L
  list(pull = counter$pulls, location_code = argset$location_code)
}

record_analysis <- function(data, argset, tables) {
  counter$seen[[length(counter$seen) + 1L]] <- data.table(
    location_code = argset$location_code,
    age = argset$age,
    pull = data$pull
  )
}

ss$add_task(
  name_grouping = "rules",
  name_action = "pull",
  for_each_plan = plnr::expand_list(location_code = locations),
  for_each_analysis = plnr::expand_list(age = ages),
  data_selector_fn_name = "pull_data",
  action_fn_name = "record_analysis"
)
ss$run_task("rules_pull")

n_plans <- length(locations)
n_analyses <- length(locations) * length(ages)
stopifnot(counter$pulls == n_plans)
c(pulls = counter$pulls, plans = n_plans, analyses = n_analyses)
#>    pulls    plans analyses 
#>        3        3        9
```

## Analyses iterate within a plan

An analysis is one argset and one call to the action function. The
analyses of a plan iterate over its argsets, and all of them read the
data of that one pull. Put the variation that needs no new data in
`for_each_analysis`.

``` r
seen <- rbindlist(counter$seen)
stopifnot(
  nrow(seen) == n_analyses,
  seen[, uniqueN(pull), by = location_code]$V1 == 1L,
  seen[, setequal(age, ages), by = location_code]$V1
)
seen
#>    location_code     age  pull
#>           <char>  <char> <int>
#> 1:          oslo 000_014     1
#> 2:          oslo 015_064     1
#> 3:          oslo    065p     1
#> 4:        bergen 000_014     2
#> 5:        bergen 015_064     2
#> 6:        bergen    065p     2
#> 7:        tromso 000_014     3
#> 8:        tromso 015_064     3
#> 9:        tromso    065p     3
```

## Only the first and the last plan run in a fixed order

`Task$run()` runs the plans in parallel when `cores` is above 1, the
task has at least 4 plans, and the session is not interactive. It then
runs plan 1 first and the last plan last, in the main process. The plans
between them run in forked workers, in any order. `R/r6_Task.R` holds
this branch.

A middle plan MUST NOT decide that the work of the other middle plans is
complete. Put any step that needs all the work done in the last plan.

This task has 6 plans and runs on 2 cores. Each plan records its
process, when it started and when it ended. Each middle plan also counts
the middle plans that finished before it. The last plan counts them too.

The next two chunks fork, so they run only on Linux and macOS, and only
when the session is not interactive. `R CMD check` and pkgdown both
build vignettes that way.

``` r
ss$add_table(
  name_access = "anon",
  name_grouping = "rules",
  name_variant = "fork",
  field_types = c(plan = "INTEGER", pid = "INTEGER"),
  keys = "plan"
)
log_dir <- file.path(db_dir, "plan-log")
dir.create(log_dir)
n_fork_plans <- 6L
middle <- 2:(n_fork_plans - 1L)

fork_data <- function(argset, tables) {
  # TRUE when the table holds an open connection. This reads the private
  # handle, because every public method of csdb::DBConnection_v9 first drops
  # a handle that another process opened. disconnect() closes the handle and
  # keeps the closed object, so an open handle is one that DBI calls valid.
  held_open <- function(table) {
    con <- table$dbconnection$.__enclos_env__$private$pconnection
    !is.null(con) && DBI::dbIsValid(con)
  }
  list(
    start = Sys.time(),
    inherited = c(
      task = held_open(tables$fork),
      vapply(cs9::config$tables, held_open, logical(1))
    )
  )
}

fork_action <- function(data, argset, tables) {
  plan <- argset$index_plan
  open_after_write <- NA
  if (argset$first_analysis) {
    tables$fork$insert_data(data.table(plan = plan, pid = Sys.getpid()))
    open_after_write <- tables$fork$dbconnection$is_connected()
  }
  own <- sprintf("done-%02d", plan)
  others <- setdiff(list.files(log_dir, pattern = "^done-"), own)
  if (plan %in% middle) file.create(file.path(log_dir, own))
  saveRDS(
    data.table(
      plan = plan,
      pid = Sys.getpid(),
      start = data$start,
      end = Sys.time(),
      finished_before = length(others),
      open_at_start = any(data$inherited),
      open_after_write = open_after_write
    ),
    file.path(log_dir, sprintf("plan-%02d.rds", plan))
  )
}

ss$add_task(
  name_grouping = "rules",
  name_action = "fork",
  cores = 2,
  for_each_plan = plnr::expand_list(plan = seq_len(n_fork_plans)),
  data_selector_fn_name = "fork_data",
  action_fn_name = "fork_action",
  tables = list(fork = ss$tables$anon_rules_fork)
)
```

``` r
ss$run_task("rules_fork")
```

The two plans in the main process have its process ID. The middle plans
ran in forked workers. Plan 1 ended before any middle plan started, and
the last plan started after every middle plan ended. A middle plan saw
fewer than 4 finished middle plans, so it could not know that the work
was complete. The last plan saw all 4.

``` r
runs <- rbindlist(lapply(
  sort(list.files(log_dir, pattern = "^plan-", full.names = TRUE)),
  readRDS
))
first <- runs[plan == 1L]
last <- runs[plan == n_fork_plans]
mid <- runs[plan %in% middle]
stopifnot(
  nrow(runs) == n_fork_plans,
  first$pid == Sys.getpid(),
  last$pid == Sys.getpid(),
  all(mid$pid != Sys.getpid()),
  first$end < min(mid$start),
  last$start > max(mid$end),
  all(mid$finished_before < length(middle)),
  last$finished_before == length(middle)
)
runs[, .(plan, worker = fifelse(pid == Sys.getpid(), "main", "forked"), finished_before)]
#>     plan worker finished_before
#>    <int> <char>           <int>
#> 1:     1   main               0
#> 2:     2 forked               2
#> 3:     3 forked               1
#> 4:     4 forked               3
#> 5:     5 forked               2
#> 6:     6   main               4
```

## A forked worker MUST NOT inherit an open connection

A fork copies the open database connections of the main process. The
worker and the main process then share one socket. An inherited
PostgreSQL connection returned wrong results without an error, measured
against the NorSySS server on 2026-08-14.
[`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
reported TRUE on that connection, so nothing detected it.

Two protections exist:

- `Task$run()` closes the connections of every table in the task’s
  `tables` list, and of every table in `cs9::config$tables`, at the end
  of `run_sequential()`. That sweep runs after plan 1 and before the
  fork. `tests/testthat/test-fork-ordering.R` pins the order.
- [`csdb::DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.html)
  records the process that opened a connection, and drops a connection
  that another process opened. csdb added this in 2026.8.15.

A connection that you open yourself with
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) has
neither protection. Open it inside the action, and close it there.

Plan 1 of the task above wrote to its table, so its connection was open
after the write. The sweep then closed it. No middle plan received an
open connection. The data selector of `rules_fork` reads the private
handle, because a public method of the connection drops an inherited
handle before it reports.

``` r
stopifnot(
  isTRUE(first$open_after_write),
  !any(mid$open_at_start)
)
first$open_after_write
#> [1] TRUE
mid[, .(plan, open_at_start)]
#>     plan open_at_start
#>    <int>        <lgcl>
#> 1:     2         FALSE
#> 2:     3         FALSE
#> 3:     4         FALSE
#> 4:     5         FALSE
```

## A task reads `cores` when it runs

`cores` MAY be a function with no arguments. `Task$run()` calls the
function each time the task runs, so the function reads its settings at
that time. A number works as before. `TaskJob` sets `cores` to 1 before
it runs a task, and this replaces the function.

This task reads its cores from the environment variable
`RULES_CORES_MAX`, which changes after `add_task()`. The task has 4
plans, the smallest count that runs in parallel.
[`get_config_tasks_stats()`](https://niphr.github.io/cs9/reference/get_config_tasks_stats.md)
shows the cores of each run. The run forks, so the next two chunks run
only on Linux and macOS, in a session that is not interactive.

``` r
noop_action <- function(data, argset, tables) invisible(NULL)

Sys.setenv(RULES_CORES_MAX = "1")
ss$add_task(
  name_grouping = "rules",
  name_action = "cores",
  cores = function() as.integer(Sys.getenv("RULES_CORES_MAX")),
  for_each_plan = plnr::expand_list(plan = 1:4),
  action_fn_name = "noop_action"
)
Sys.setenv(RULES_CORES_MAX = "2")
ss$run_task("rules_cores")
```

``` r
cores_used <- cs9::get_config_tasks_stats(task = "rules_cores")$cores_n
stopifnot(cores_used == 2)
cores_used
#> [1] 2
```

## A task builds its plans once per run

`run_task()` builds the plans of a task once, before any plan runs. It
builds them in the main process, before `Task$run()` forks a worker. The
plans do not change during the run, and every worker gets the same
plans.

A task with a plan analysis builds its plans again on every `run_task()`
since 26.10.3. A task with fixed plans (`for_each_plan`) does the same
since 26.10.7. `argset$today` and `argset$yesterday` are therefore the
date of the run. Before 26.10.7, a task with fixed plans kept the date
of `add_task()`, which norsyss.cs9 calls when the package loads.

A scheduler such as Airflow starts a new R process for each task. The
package loads and `run_task()` runs once, so the rebuild makes no
material difference there.

In an interactive session, a second `run_task()` of the same task builds
new plans. A plan analysis that reads external state can then give
different plans from the first run. External state includes the
database, environment variables and the date. This is intended: each run
plans for the data that it finds.

`get_task()` and the `shortcut_get_*()` methods build plans only when a
task has none. They show the plans of the last build.

This task has 2 plans and runs twice.
[`testthat::with_mocked_bindings()`](https://testthat.r-lib.org/reference/local_mocked_bindings.html)
gives each run a different date from
[`lubridate::today()`](https://lubridate.tidyverse.org/reference/now.html),
which cs9 calls when it builds the plans.

``` r
plan_runs <- new.env()
plan_runs$rows <- list()

record_today <- function(data, argset, tables) {
  plan_runs$rows[[length(plan_runs$rows) + 1L]] <- data.table(
    run = plan_runs$run,
    plan = argset$plan,
    today = argset$today
  )
}

ss$add_task(
  name_grouping = "rules",
  name_action = "today",
  for_each_plan = plnr::expand_list(plan = 1:2),
  action_fn_name = "record_today"
)
registered <- ss$tasks$rules_today$plans[[1]]$get_argset(1)$today

run_on <- function(run, date) {
  plan_runs$run <- run
  testthat::with_mocked_bindings(
    ss$run_task("rules_today"),
    today = function(tzone = "") date,
    .package = "lubridate"
  )
}
day_1 <- registered + 1
day_2 <- registered + 2
invisible(run_on("first", day_1))
invisible(run_on("second", day_2))

seen_today <- rbindlist(plan_runs$rows)
stopifnot(
  seen_today[run == "first", all(today == day_1)],
  seen_today[run == "second", all(today == day_2)]
)
seen_today[, .(run, plan, today)]
#>       run  plan      today
#>    <char> <int>     <Date>
#> 1:  first     1 2026-10-11
#> 2:  first     2 2026-10-11
#> 3: second     1 2026-10-12
#> 4: second     2 2026-10-12
```

Do not edit `task$plans` by hand and then call `run_task()`.
`run_task()` builds the plans again, and the edit is lost. Call
`task$run()` instead. It runs the current plans and does not build them
again.

This chunk keeps only plan 2 and calls `task$run()`. The edited plan
still has the date of the second run. A later `run_task()` builds both
plans again.

``` r
task <- ss$tasks$rules_today
task$plans <- task$plans[2]
plan_runs$run <- "edited"
invisible(task$run())
plan_runs$run <- "run_task"
invisible(ss$run_task("rules_today"))

seen_today <- rbindlist(plan_runs$rows)
stopifnot(
  identical(seen_today[run == "edited", plan], 2L),
  seen_today[run == "edited", all(today == day_2)],
  setequal(seen_today[run == "run_task", plan], 1:2)
)
seen_today[run %in% c("edited", "run_task")]
#>         run  plan      today
#>      <char> <int>     <Date>
#> 1:   edited     2 2026-10-12
#> 2: run_task     1 2026-10-10
#> 3: run_task     2 2026-10-10
```

## A completeness check covers every declared partition

A partitioned table is one database table per partition.
`names(pt$tables)` lists the declared partitions. A check that a week is
complete MUST cover every declared partition, and never only the
partitions that hold rows.

This table declares 3 partitions. The import wrote rows for `flu` and
`covid` only.

``` r
ss$add_partitionedtable(
  name_access = "anon",
  name_grouping = "rules",
  name_variant = "cases",
  name_partitions = c("flu", "covid", "rsv"),
  column_name_partition = "disease",
  field_types = c(isoyearweek = "TEXT", n = "INTEGER"),
  keys = c("isoyearweek", "disease")
)
pt <- ss$partitionedtables$anon_rules_cases
pt$insert_data(data.table(
  isoyearweek = rep(c("2026-01", "2026-02"), times = 2),
  disease = rep(c("flu", "covid"), each = 2),
  n = 1:4
))
partition_rows <- pt$nrow(collapse = FALSE)
partition_rows
#>                      table_name  nrow partition
#>                          <char> <num>    <char>
#> 1: anon_rules_cases_xxpxx_covid     2     covid
#> 2:   anon_rules_cases_xxpxx_flu     2       flu
```

`nrow(collapse = FALSE)` has one row for each partition whose table
exists. csdb creates the table of a partition at its first use, so `rsv`
has no row here. Use `names(pt$tables)` for the declared partitions.

The `partition` column holds the partition tag. Never parse the tag from
`table_name`. The separator in the name depends on the database driver.

``` r
# The weeks that every partition in `partitions` holds.
complete_weeks <- function(pt, partitions) {
  held <- lapply(partitions, function(p) {
    dplyr::pull(dplyr::distinct(pt$tables[[p]]$tbl(), isoyearweek))
  })
  sort(Reduce(intersect, held))
}

declared <- names(pt$tables)
holding_rows <- partition_rows$partition[partition_rows$nrow > 0]
complete <- complete_weeks(pt, declared)
stopifnot(
  setequal(declared, c("flu", "covid", "rsv")),
  !"rsv" %in% partition_rows$partition,
  length(complete) == 0
)
list(
  declared = declared,
  holding_rows = holding_rows,
  complete = complete,
  complete_if_you_read_rows = complete_weeks(pt, holding_rows)
)
#> $declared
#> [1] "flu"   "covid" "rsv"  
#> 
#> $holding_rows
#> [1] "covid" "flu"  
#> 
#> $complete
#> character(0)
#> 
#> $complete_if_you_read_rows
#> [1] "2026-01" "2026-02"
```

## `plnr::expand_list()` makes one argset per element

[`plnr::expand_list()`](https://www.rwhite.no/plnr/reference/expand_list.html)
returns one argset for each combination of its arguments. A vector of 3
weeks therefore gives 3 argsets. To give each argset all 3 weeks, pass
them as one text value and split it in the action. A vector inside
[`list()`](https://rdrr.io/r/base/list.html) also gives one argset.

``` r
weeks <- c("2026-01", "2026-02", "2026-03")
one_per_week <- plnr::expand_list(isoyearweek = weeks)
one_text <- plnr::expand_list(run_weeks = paste(weeks, collapse = ","))
one_list <- plnr::expand_list(run_weeks = list(weeks))
stopifnot(
  length(one_per_week) == 3,
  length(one_text) == 1,
  identical(strsplit(one_text[[1]]$run_weeks, ",")[[1]], weeks),
  length(one_list) == 1,
  identical(one_list[[1]]$run_weeks, weeks)
)
c(one_per_week = length(one_per_week), one_text = length(one_text), one_list = length(one_list))
#> one_per_week     one_text     one_list 
#>            3            1            1
```

`universal_argset` is not expanded. Every argset of the task gets each
of its values unchanged.

## Size a plan by its data pull

A plan holds one pull in memory, so its data pull sets the size of a
plan. The unit of work that you commit is a different choice. Do not
make a plan per committed unit when one pull serves several units. Group
the commits in the last plan.

Both tasks below read the same 6 rows, for 2 weeks and 3 locations. The
source returns one week for all locations. `per_week_location` makes a
plan per week and location, so it pulls each week 3 times. `per_week`
makes a plan per week and an analysis per location, so it pulls each
week once. Its analyses write to a staging table. The last analysis
moves the staged rows into the live table in one transaction.

``` r
for (variant in c("staging", "live")) {
  ss$add_table(
    name_access = "anon",
    name_grouping = "rules_size",
    name_variant = variant,
    field_types = c(isoyearweek = "TEXT", location_code = "TEXT", n = "INTEGER"),
    keys = c("isoyearweek", "location_code")
  )
}
size_weeks <- c("2026-01", "2026-02")
size <- new.env()
size$pulls <- c(per_week_location = 0L, per_week = 0L)
size$commits <- 0L

week_data <- function(argset, tables) {
  size$pulls[argset$design] <- size$pulls[argset$design] + 1L
  list(week = data.table(
    isoyearweek = argset$isoyearweek,
    location_code = locations,
    n = seq_along(locations)
  ))
}

week_action <- function(data, argset, tables) {
  rows <- data$week[location_code == argset$location_code]
  if (argset$design == "per_week") tables$staging$insert_data(rows)
  if (argset$design == "per_week" && argset$last_analysis) {
    staged <- dplyr::collect(tables$staging$tbl())
    # connect() creates the live table if this is its first use.
    tables$live$connect()
    con <- tables$live$dbconnection$autoconnection
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM ", tables$live$table_name_fully_specified_text))
      DBI::dbAppendTable(con, tables$live$table_name_short_for_mssql_fully_specified_for_postgres, staged)
    })
    tables$staging$drop_all_rows()
    size$commits <- size$commits + 1L
  }
}

size_tables <- list(
  staging = ss$tables$anon_rules_size_staging,
  live = ss$tables$anon_rules_size_live
)
ss$add_task(
  name_grouping = "rules_size",
  name_action = "per_week_location",
  for_each_plan = plnr::expand_list(isoyearweek = size_weeks, location_code = locations),
  universal_argset = list(design = "per_week_location"),
  data_selector_fn_name = "week_data",
  action_fn_name = "week_action",
  tables = size_tables
)
ss$add_task(
  name_grouping = "rules_size",
  name_action = "per_week",
  for_each_plan = plnr::expand_list(isoyearweek = size_weeks),
  for_each_analysis = plnr::expand_list(location_code = locations),
  universal_argset = list(design = "per_week"),
  data_selector_fn_name = "week_data",
  action_fn_name = "week_action",
  tables = size_tables
)
ss$run_task("rules_size_per_week_location")
ss$run_task("rules_size_per_week")

live_n <- nrow(dplyr::collect(ss$tables$anon_rules_size_live$tbl()))
staged_n <- nrow(dplyr::collect(ss$tables$anon_rules_size_staging$tbl()))
stopifnot(
  size$pulls[["per_week_location"]] == length(size_weeks) * length(locations),
  size$pulls[["per_week"]] == length(size_weeks),
  size$commits == 1L,
  live_n == length(size_weeks) * length(locations),
  staged_n == 0L
)
c(size$pulls, commits = size$commits, live_rows = live_n)
#> per_week_location          per_week           commits         live_rows 
#>                 6                 2                 1                 6
```

[`vignette("task-shapes")`](https://niphr.github.io/cs9/articles/task-shapes.md)
shows [staging as a
checkpoint](https://niphr.github.io/cs9/articles/task-shapes.html#shape-staging-checkpoint),
which commits each complete unit in the last plan.
