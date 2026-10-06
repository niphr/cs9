# More task shapes

This vignette holds three of the five task shapes.
[`vignette("task-shapes")`](https://niphr.github.io/cs9/articles/task-shapes.md)
lists all five and holds the other two. Each shape runs against SQLite
when the vignette builds, and a chunk calls
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) on what the shape
promises.

## Setup

This chunk points the `config` and `anon` access levels at two SQLite
files under [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

``` r
library(data.table)

db_dir <- file.path(tempdir(), "cs9-more-task-shapes")
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

ss <- cs9::SurveillanceSystem_v9$new(name = "more_shapes")

# The rows of a table as a data.frame, sorted by the columns in `by`, without
# the timestamp column that cs9 adds to every table.
read_rows <- function(table, by) {
  d <- as.data.frame(dplyr::collect(table$tbl()))
  d$auto_last_updated_datetime <- NULL
  d <- d[do.call(order, d[by]), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# Values quoted for an SQL IN list.
sql_in <- function(x) paste(DBI::dbQuoteString(DBI::ANSI(), x), collapse = ", ")
```

## Per-unit rebuild

The task has one plan per unit, for example a location or a partition.
The first analysis empties the output table. Each analysis then inserts
the rows of its own unit.

- **Use it when** the rows of each unit depend only on the data pull of
  that unit, and nothing reads the output during the run.
- **Do not use it when** a reader MUST see a complete table at every
  moment. Use [fit then
  fill](https://niphr.github.io/cs9/articles/task-shapes.html#shape-fit-then-fill),
  which publishes in one transaction.
- **Failure modes.** A failed run leaves the table partial until the
  next run succeeds. Only the first plan MAY empty the table, because
  the middle plans MAY run in parallel, in any order
  ([rule](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-first-last)).
  With a partitioned output, one plan per partition writes only its own
  partition.

``` r
ss$add_table(
  name_access = "anon",
  name_grouping = "rebuild",
  name_variant = "rates",
  field_types = c(location_code = "TEXT", rate_per_1000 = "DOUBLE"),
  keys = "location_code"
)
counts <- data.table(
  location_code = c("county_03", "county_11", "county_46"),
  cases = c(30L, 12L, 7L),
  pop = c(700L, 600L, 650L)
)
rebuild <- new.env()
rebuild$fail <- NA_character_

rebuild_data <- function(argset, tables) {
  list(counts = counts[location_code == argset$location_code])
}

rebuild_action <- function(data, argset, tables) {
  if (argset$first_analysis) tables$rates$drop_all_rows()
  if (identical(argset$location_code, rebuild$fail)) {
    stop("the computation failed for ", rebuild$fail)
  }
  tables$rates$insert_data(
    data$counts[, .(location_code, rate_per_1000 = 1000 * cases / pop)]
  )
}

ss$add_task(
  name_grouping = "rebuild",
  name_action = "rates",
  for_each_plan = plnr::expand_list(location_code = counts$location_code),
  data_selector_fn_name = "rebuild_data",
  action_fn_name = "rebuild_action",
  tables = list(rates = ss$tables$anon_rebuild_rates)
)
```

The first run writes all 3 locations. The second run fails at the second
location. The table then holds only the first location, and a reader
sees that partial table until a run succeeds.

``` r
ss$run_task("rebuild_rates")
full <- read_rows(ss$tables$anon_rebuild_rates, "location_code")

rebuild$fail <- "county_11"
run <- tryCatch(ss$run_task("rebuild_rates"), error = function(e) e)
partial <- read_rows(ss$tables$anon_rebuild_rates, "location_code")
stopifnot(
  identical(full$location_code, counts$location_code),
  inherits(run, "error"),
  identical(partial$location_code, "county_03")
)
partial
#>   location_code rate_per_1000
#> 1     county_03      42.85714
```

## Staging as a checkpoint

The import has one plan per week. A run imports the refresh weeks, which
MUST always be fresh, and every week of each selected unit. Here the
unit is the isoyear. The staging table keeps the weeks of a failed run,
so the next run does not pull them again.

1.  The planner selects each unit that the live table does not hold
    completely. It plans the refresh weeks, and every week of a selected
    unit that staging does not hold.
2.  The first plan removes only the refresh weeks from staging.
3.  Each plan pulls its week and writes it to staging.
4.  The last plan swaps the refresh weeks into the live table. It then
    swaps each unit whose weeks are all in staging. One transaction per
    swap deletes the weeks from the live table, copies them from staging
    and removes them from staging.

- **Use it when** an import runs for hours, and a failure part way MUST
  NOT cost the weeks that the run already pulled.
- **Do not use it when** one run is short. A [staged
  import](https://niphr.github.io/cs9/articles/task-shapes.html#shape-staged-import)
  is simpler.
- **Failure modes.** The first plan MUST NOT empty staging, or the next
  run loses its checkpoint. Only the last plan MAY swap, because no
  middle plan knows which other weeks are staged
  ([rule](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-first-last)).
  With partitioned tables, a week counts as staged only when every
  declared partition holds it
  ([rule](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-partitions)).
  A plan holds one week in memory, and the swap copies the rows inside
  the database.

``` r
ckpt_fields <- c(isoyear = "INTEGER", isoyearweek = "TEXT", n = "INTEGER")
for (variant in c("staging", "live")) {
  ss$add_table(
    name_access = "anon",
    name_grouping = "ckpt",
    name_variant = variant,
    field_types = ckpt_fields,
    keys = "isoyearweek"
  )
}
refresh_weeks <- c("2026-01", "2026-02")
unit_weeks <- list(
  "2024" = c("2024-01", "2024-02", "2024-03"),
  "2025" = c("2025-01", "2025-02", "2025-03")
)
ckpt <- new.env()
ckpt$fail_week <- NA_character_
ckpt$pulled <- character(0)
ckpt$swapped <- character(0)

weeks_in <- function(table) {
  dplyr::pull(dplyr::distinct(table$tbl(), isoyearweek))
}

ckpt_plan_analysis <- function(argset, tables) {
  live <- weeks_in(tables$live)
  staged <- weeks_in(tables$staging)
  units <- names(unit_weeks)[
    vapply(unit_weeks, function(w) !all(w %in% live), logical(1))
  ]
  to_pull <- setdiff(unlist(unit_weeks[units], use.names = FALSE), staged)
  list(
    for_each_plan = plnr::expand_list(isoyearweek = c(refresh_weeks, to_pull)),
    # One text value each, so every analysis gets the whole list.
    for_each_analysis = plnr::expand_list(
      refresh = paste(refresh_weeks, collapse = ","),
      units = paste(units, collapse = ",")
    )
  )
}

ckpt_data <- function(argset, tables) {
  ckpt$pulled <- c(ckpt$pulled, argset$isoyearweek)
  list(rows = data.table(
    isoyear = as.integer(substr(argset$isoyearweek, 1, 4)),
    isoyearweek = argset$isoyearweek,
    n = 10L
  ))
}

ckpt_action <- function(data, argset, tables) {
  refresh <- strsplit(argset$refresh, ",")[[1]]
  if (argset$first_analysis) {
    tables$staging$drop_rows_where(paste0("isoyearweek IN (", sql_in(refresh), ")"))
  }
  if (identical(argset$isoyearweek, ckpt$fail_week)) {
    stop("the source failed for ", ckpt$fail_week)
  }
  tables$staging$insert_data(data$rows)
  if (argset$last_analysis) {
    ckpt_swap(tables, refresh, strsplit(argset$units, ",")[[1]])
  }
}

# One transaction for the refresh weeks, and one for each complete unit.
ckpt_swap <- function(tables, refresh, units) {
  staged <- weeks_in(tables$staging)
  batches <- c(list(refresh = refresh), unit_weeks[units])
  tables$live$connect()
  con <- tables$live$dbconnection$autoconnection
  live_name <- tables$live$table_name_fully_specified_text
  staging_name <- tables$staging$table_name_fully_specified_text
  for (b in names(batches)) {
    # A unit with a week missing from staging waits for the next run.
    if (!all(batches[[b]] %in% staged)) next
    where <- paste0(" WHERE isoyearweek IN (", sql_in(batches[[b]]), ")")
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM ", live_name, where))
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", live_name, " SELECT * FROM ", staging_name, where
      ))
      DBI::dbExecute(con, paste0("DELETE FROM ", staging_name, where))
    })
    ckpt$swapped <- c(ckpt$swapped, b)
  }
}

ss$add_task(
  name_grouping = "ckpt",
  name_action = "import",
  plan_analysis_fn_name = "ckpt_plan_analysis",
  data_selector_fn_name = "ckpt_data",
  action_fn_name = "ckpt_action",
  tables = list(
    staging = ss$tables$anon_ckpt_staging,
    live = ss$tables$anon_ckpt_live
  )
)
```

The live table holds the refresh weeks of an earlier run. The first run
fails at week 2025-02, after it staged every week of 2024. The live
table MUST be unchanged.

``` r
ss$tables$anon_ckpt_live$insert_data(
  data.table(isoyear = 2026L, isoyearweek = refresh_weeks, n = 99L)
)
live_before <- read_rows(ss$tables$anon_ckpt_live, "isoyearweek")

ckpt$fail_week <- "2025-02"
run <- tryCatch(ss$run_task("ckpt_import"), error = function(e) e)
stopifnot(
  inherits(run, "error"),
  identical(read_rows(ss$tables$anon_ckpt_live, "isoyearweek"), live_before),
  all(unit_weeks[["2024"]] %in% weeks_in(ss$tables$anon_ckpt_staging))
)
sort(weeks_in(ss$tables$anon_ckpt_staging))
#> [1] "2024-01" "2024-02" "2024-03" "2025-01" "2026-01" "2026-02"
```

The second run pulls the refresh weeks and the 2 missing weeks of 2025,
and no week of 2024. Its last plan swaps the refresh weeks and both
units. Staging is then empty.

``` r
ckpt$fail_week <- NA_character_
ckpt$pulled <- character(0)
ckpt$swapped <- character(0)
ss$run_task("ckpt_import")
live_after <- read_rows(ss$tables$anon_ckpt_live, "isoyearweek")
all_weeks <- c(unlist(unit_weeks, use.names = FALSE), refresh_weeks)
stopifnot(
  setequal(ckpt$pulled, c(refresh_weeks, "2025-02", "2025-03")),
  identical(ckpt$swapped, c("refresh", "2024", "2025")),
  setequal(live_after$isoyearweek, all_weeks),
  all(live_after$n == 10L),
  length(weeks_in(ss$tables$anon_ckpt_staging)) == 0
)
list(pulled = ckpt$pulled, swapped = ckpt$swapped)
#> $pulled
#> [1] "2026-01" "2026-02" "2025-02" "2025-03"
#> 
#> $swapped
#> [1] "refresh" "2024"    "2025"
```

## Export from a finished table

The task reads tables that an earlier task finished, and writes files.
It has one plan per output file, for example per location or per week.
The first plan creates the folder. The last plan writes what needs every
file, here an index, and checks that every expected file exists.

- **Use it when** the output is files, and the scheduler runs the task
  after the tasks that write its input tables.
- **Do not use it when** an input table can change during the export,
  for example a [per-unit rebuild](#shape-per-unit-rebuild) that runs at
  the same time.
- **Failure modes.** Do not catch an error in the action only to print
  it. `Task$run()` then records the task as succeeded, with a file
  missing. Plans that run in parallel MUST write to different file
  names. A step that needs every file MUST run in the last plan
  ([rule](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-first-last)).

``` r
ss$add_table(
  name_access = "anon",
  name_grouping = "export",
  name_variant = "source",
  field_types = c(location_code = "TEXT", isoyearweek = "TEXT", n = "INTEGER"),
  keys = c("location_code", "isoyearweek")
)
ss$tables$anon_export_source$insert_data(data.table(
  location_code = rep(c("county_03", "county_11", "county_46"), each = 2),
  isoyearweek = c("2026-01", "2026-02"),
  n = 1:6
))
export_locations <- c("county_03", "county_11", "county_46")
export <- new.env()
export$broken <- NA_character_

export_data <- function(argset, tables) {
  list(rows = dplyr::collect(dplyr::filter(
    tables$source$tbl(),
    location_code == !!argset$location_code
  )))
}

export_action <- function(data, argset, tables) {
  if (argset$first_analysis) dir.create(argset$folder, recursive = TRUE)
  if (identical(argset$location_code, export$broken)) {
    stop("the figure failed for ", export$broken)
  }
  data.table::fwrite(
    data$rows,
    file.path(argset$folder, paste0(argset$location_code, ".csv"))
  )
  if (argset$last_analysis) {
    expected <- paste0(strsplit(argset$locations, ",")[[1]], ".csv")
    missing <- expected[!file.exists(file.path(argset$folder, expected))]
    if (length(missing) > 0) stop("missing exports: ", paste(missing, collapse = ", "))
    writeLines(expected, file.path(argset$folder, "index.txt"))
  }
}

ss$add_task(
  name_grouping = "export",
  name_action = "csv",
  for_each_plan = plnr::expand_list(location_code = export_locations),
  universal_argset = list(
    folder = file.path(db_dir, "export"),
    locations = paste(export_locations, collapse = ",")
  ),
  data_selector_fn_name = "export_data",
  action_fn_name = "export_action",
  tables = list(source = ss$tables$anon_export_source)
)
```

The first run writes 3 files and the index. In the second run the export
for county 11 fails. The task MUST stop with an error.

``` r
folder <- file.path(db_dir, "export")
ss$run_task("export_csv")
stopifnot(identical(readLines(file.path(folder, "index.txt")), paste0(export_locations, ".csv")))

unlink(folder, recursive = TRUE)
export$broken <- "county_11"
run <- tryCatch(ss$run_task("export_csv"), error = function(e) e)
stopifnot(
  inherits(run, "error"),
  !file.exists(file.path(folder, "index.txt"))
)
conditionMessage(run)
#> [1] "the figure failed for county_11"
```
