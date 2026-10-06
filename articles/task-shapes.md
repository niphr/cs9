# Task shapes

This vignette shows two task shapes that publish the result of a run
only when the whole run succeeds. Both shapes run against SQLite when
the vignette builds. A chunk calls
[`stop()`](https://rdrr.io/r/base/stop.html) if a shape does not keep
its promise, so the build fails.

## Setup

CS9 reads its database configuration from environment variables. This
chunk points the `config` and `anon` access levels at two SQLite files
under [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

``` r
library(data.table)

db_dir <- file.path(tempdir(), "cs9-task-shapes")
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

ss <- cs9::SurveillanceSystem_v9$new(name = "shapes")

# The rows of a table as a data.frame, sorted by the columns in `by`.
read_rows <- function(table, by) {
  d <- as.data.frame(dplyr::collect(table$tbl()))
  d <- d[do.call(order, d[by]), , drop = FALSE]
  rownames(d) <- NULL
  d
}
```

## Shape 1: staged import with check and swap

The import task has one plan per week. Each argset copies its week from
the source into a staging table. The argset with `first_analysis = TRUE`
empties the staging table first. The argset with `last_analysis = TRUE`
checks the staged rows. It then swaps them into the live table in one
transaction.

``` r
weeks <- c("2026-01", "2026-02", "2026-03", "2026-04")
import_fields <- c(isoyearweek = "TEXT", location_code = "TEXT", n = "INTEGER")
for (variant in c("staging", "live")) {
  ss$add_table(
    name_access = "anon",
    name_grouping = "import",
    name_variant = variant,
    field_types = import_fields,
    keys = c("isoyearweek", "location_code")
  )
}

source_counts <- data.table(
  isoyearweek = rep(weeks, each = 2),
  location_code = c("county_03", "county_11"),
  n = 1:8
)
fail_week <- NA_character_

import_data <- function(argset, tables) {
  list(counts = source_counts[isoyearweek == argset$isoyearweek])
}

import_action <- function(data, argset, tables) {
  if (argset$first_analysis) tables$staging$drop_all_rows()
  if (identical(argset$isoyearweek, fail_week)) {
    stop("the source failed for ", fail_week)
  }
  if (nrow(data$counts) > 0) tables$staging$insert_data(data$counts)
  if (argset$last_analysis) swap_into_live(tables, argset$weeks)
}

# Deletes every week of the run from the live table, not only the weeks that
# reached staging, and appends the staged rows. One transaction holds both.
swap_into_live <- function(tables, weeks) {
  staged <- dplyr::collect(tables$staging$tbl())
  stopifnot(all(staged$isoyearweek %in% weeks), !anyNA(staged$n))
  live <- tables$live
  con <- live$dbconnection$autoconnection
  run_weeks <- paste(DBI::dbQuoteString(con, weeks), collapse = ", ")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0(
      "DELETE FROM ", live$table_name_fully_specified_text,
      " WHERE isoyearweek IN (", run_weeks, ")"
    ))
    DBI::dbAppendTable(
      con,
      live$table_name_short_for_mssql_fully_specified_for_postgres,
      staged
    )
  })
}

ss$add_task(
  name_grouping = "import",
  name_action = "weekly",
  for_each_plan = lapply(weeks, function(w) list(isoyearweek = w)),
  universal_argset = list(weeks = weeks),
  data_selector_fn_name = "import_data",
  action_fn_name = "import_action",
  tables = list(
    staging = ss$tables$anon_import_staging,
    live = ss$tables$anon_import_live
  )
)
```

The live table holds an earlier run of all 4 weeks. The source then
withdraws week 2026-03, so no row for that week reaches staging. After
the run, the live table MUST hold exactly the staged rows, and no row
for 2026-03.

``` r
ss$tables$anon_import_live$insert_data(
  data.table(isoyearweek = weeks, location_code = "county_03", n = 99L)
)
source_counts <- source_counts[isoyearweek != "2026-03"]

ss$run_task("import_weekly")
live <- read_rows(ss$tables$anon_import_live, c("isoyearweek", "location_code"))
staged <- read_rows(ss$tables$anon_import_staging, c("isoyearweek", "location_code"))
stopifnot(identical(live, staged), !"2026-03" %in% live$isoyearweek)
live
#>   isoyearweek location_code n auto_last_updated_datetime
#> 1     2026-01     county_03 1        2026-10-06 06:10:59
#> 2     2026-01     county_11 2        2026-10-06 06:10:59
#> 3     2026-02     county_03 3        2026-10-06 06:10:59
#> 4     2026-02     county_11 4        2026-10-06 06:10:59
#> 5     2026-04     county_03 7        2026-10-06 06:10:59
#> 6     2026-04     county_11 8        2026-10-06 06:10:59
```

The second run reads new counts, and week 2026-02 fails. The task MUST
stop with an error, and the live table MUST keep the rows of the first
run.

``` r
source_counts[, n := n + 100L]
fail_week <- "2026-02"
run <- tryCatch(ss$run_task("import_weekly"), error = function(e) e)
stopifnot(
  inherits(run, "error"),
  identical(
    read_rows(ss$tables$anon_import_live, c("isoyearweek", "location_code")),
    live
  )
)
conditionMessage(run)
#> [1] "the source failed for 2026-02"
cat("SHAPE 1 CHECKS PASSED\n")
#> SHAPE 1 CHECKS PASSED
```

## Shape 2: fit then fill

The fit task fits a threshold for each stratum in its own argset. It
writes `NA` for a stratum with fewer than 4 weeks of history. The fill
task has one plan. It fills each `NA` row from the national row, records
the `source` of each threshold, and publishes the whole result with
`csdb::DBTable_v9$replace_all_rows()`.

``` r
ss$add_table(
  name_access = "anon",
  name_grouping = "threshold",
  name_variant = "fitted",
  field_types = c(location_code = "TEXT", threshold = "DOUBLE"),
  keys = "location_code"
)
ss$add_table(
  name_access = "anon",
  name_grouping = "threshold",
  name_variant = "published",
  field_types = c(location_code = "TEXT", threshold = "DOUBLE", source = "TEXT"),
  keys = "location_code"
)

history <- data.table(
  location_code = rep(
    c("norge", "county_03", "county_11", "county_46"),
    times = c(8, 8, 8, 2)
  ),
  n = c(
    rep(c(40L, 44L, 38L, 46L), 2),
    rep(c(10L, 12L, 9L, 14L), 2),
    rep(c(20L, 25L, 18L, 22L), 2),
    c(3L, 4L)
  )
)
break_publish <- FALSE

fit_data <- function(argset, tables) {
  list(history = history[location_code == argset$location_code])
}

fit_action <- function(data, argset, tables) {
  n <- data$history$n
  threshold <- if (length(n) >= 4) mean(n) + 2 * sd(n) else NA_real_
  tables$fitted$upsert_data(
    data.table(location_code = argset$location_code, threshold = threshold)
  )
}

fill_data <- function(argset, tables) {
  list(fitted = as.data.table(dplyr::collect(tables$fitted$tbl())))
}

fill_action <- function(data, argset, tables) {
  d <- copy(data$fitted)
  national <- d[location_code == "norge", threshold]
  d[, source := fifelse(is.na(threshold), "national", "fitted")]
  d[is.na(threshold), threshold := national]
  # A duplicate key makes the publish fail on purpose.
  if (break_publish) d <- rbind(d, d[1])
  tables$published$replace_all_rows(d)
}

ss$add_task(
  name_grouping = "threshold",
  name_action = "fit",
  for_each_plan = lapply(unique(history$location_code), function(x) {
    list(location_code = x)
  }),
  data_selector_fn_name = "fit_data",
  action_fn_name = "fit_action",
  tables = list(fitted = ss$tables$anon_threshold_fitted)
)
#> <Task>
#>   Public:
#>     action_after_fn: NULL
#>     action_before_fn: NULL
#>     clone: function (deep = FALSE) 
#>     cores: 1
#>     implementation_version: unspecified
#>     initialize: function (name_grouping = NULL, name_action = NULL, name_variant = NULL, 
#>     insert_at_end_of_each_plan: FALSE
#>     insert_first_last_analysis: function () 
#>     name: threshold_fit
#>     name_action: fit
#>     name_grouping: threshold
#>     name_variant: NULL
#>     num_analyses: function () 
#>     num_plans: function () 
#>     permission: NULL
#>     plans: list
#>     private: environment
#>     run: function (cores = self$cores) 
#>     self: Task, R6
#>     ss: shapes
#>     tables: list
#>     update_plans: function (replan = FALSE) 
#>     update_plans_fn: NULL
#>     upsert_at_end_of_each_plan: FALSE
#>   Private:
#>     plans_built: FALSE
#>     run_parallel: function (plans_index, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, 
#>     run_parallel_plans: function (plans_index, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, 
#>     run_sequential: function (plans_index, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan,
ss$add_task(
  name_grouping = "threshold",
  name_action = "fill",
  for_each_plan = list(list()),
  data_selector_fn_name = "fill_data",
  action_fn_name = "fill_action",
  tables = list(
    fitted = ss$tables$anon_threshold_fitted,
    published = ss$tables$anon_threshold_published
  )
)
#> <Task>
#>   Public:
#>     action_after_fn: NULL
#>     action_before_fn: NULL
#>     clone: function (deep = FALSE) 
#>     cores: 1
#>     implementation_version: unspecified
#>     initialize: function (name_grouping = NULL, name_action = NULL, name_variant = NULL, 
#>     insert_at_end_of_each_plan: FALSE
#>     insert_first_last_analysis: function () 
#>     name: threshold_fill
#>     name_action: fill
#>     name_grouping: threshold
#>     name_variant: NULL
#>     num_analyses: function () 
#>     num_plans: function () 
#>     permission: NULL
#>     plans: list
#>     private: environment
#>     run: function (cores = self$cores) 
#>     self: Task, R6
#>     ss: shapes
#>     tables: list
#>     update_plans: function (replan = FALSE) 
#>     update_plans_fn: NULL
#>     upsert_at_end_of_each_plan: FALSE
#>   Private:
#>     plans_built: FALSE
#>     run_parallel: function (plans_index, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, 
#>     run_parallel_plans: function (plans_index, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan, 
#>     run_sequential: function (plans_index, tables, upsert_at_end_of_each_plan, insert_at_end_of_each_plan,
```

After both tasks run, every published row MUST have a threshold and a
source. County 46 MUST carry the national threshold.

``` r
ss$run_task("threshold_fit")
ss$run_task("threshold_fill")
published <- read_rows(ss$tables$anon_threshold_published, "location_code")
stopifnot(
  !anyNA(published$threshold),
  !anyNA(published$source),
  published$source[published$location_code == "county_46"] == "national"
)
published
#>   location_code threshold   source auto_last_updated_datetime
#> 1     county_03  15.35575   fitted        2026-10-06 06:11:01
#> 2     county_11  26.77914   fitted        2026-10-06 06:11:01
#> 3     county_46  48.76123 national        2026-10-06 06:11:01
#> 4         norge  48.76123   fitted        2026-10-06 06:11:01
```

The history then doubles, and the fit runs again. The publish of the
fill task fails. The published table MUST keep its previous contents.

``` r
history[, n := 2L * n]
ss$run_task("threshold_fit")
break_publish <- TRUE
run <- tryCatch(ss$run_task("threshold_fill"), error = function(e) e)
stopifnot(
  inherits(run, "error"),
  identical(read_rows(ss$tables$anon_threshold_published, "location_code"), published)
)
conditionMessage(run)
#> [1] "UNIQUE constraint failed: anon_threshold_published.location_code"
cat("SHAPE 2 CHECKS PASSED\n")
#> SHAPE 2 CHECKS PASSED
```

## Three rules

**Publish in one transaction.** A task once emptied its MEM table in
`first_analysis` and refilled it argset by argset. Between those two
points, and after any failed argset, readers saw an empty or partial
table. Shape 2 writes the published table in one call to
`replace_all_rows()`.

**Replace the whole scope of the run.** A swap once deleted from the
live table only the weeks that each staging partition held. A week with
no staged rows therefore kept its stale rows. Shape 1 deletes every week
of the run, so a withdrawn week leaves the live table.

**Record where each value came from.** A national threshold copied into
a county row looks the same as a threshold fitted for that county. Shape
2 writes `source` next to each threshold.

Both shapes need cs9 to run the argset with `last_analysis = TRUE` after
every other argset of the task.
`tests/testthat/test-first-last-analysis.R` pins that order.
