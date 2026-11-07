# Synthetic data in a surveillance system

Synthetic mode lets one task run on synthetic inputs, with no upstream
task and no real data. Every table of a `SurveillanceSystem_v9` has a
[`csdb::SyntheticTable`](https://niphr.github.io/csdb/reference/SyntheticTable.html),
which holds the contract of the table and its fixed datasets. With
synthetic mode on, cs9 fills the input tables of a task from them, and
each switchable source returns its simulator.

This vignette builds a small surveillance system on SQLite and runs one
task on synthetic data. Two other vignettes cover the levels below and
above it:

- csdb,
  [`vignette("synthetic-data", package = "csdb")`](https://niphr.github.io/csdb/articles/synthetic-data.html):
  profiles, simulation and the `SyntheticTable` of one table.
- norsyss.cs9, `vignettes/synthetic-development.Rmd` in the private
  repository `niphr/norsyss.cs9`: a late task of a real pipeline, run on
  synthetic inputs alone.

## A small system

The system has two input tables and one output table.
`anon_vignette_cases` holds weekly cases, partitioned by region.
`anon_vignette_population` holds the population of each region. The
databases are SQLite files under
[`tempdir()`](https://rdrr.io/r/base/tempfile.html).

``` r
library(data.table)

db_dir <- file.path(tempdir(), "cs9-synthetic")
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
ss <- cs9::SurveillanceSystem_v9$new(name = "synthetic")
```

## Attach a SyntheticTable to a table

`add_table()` and `add_partitionedtable()` take the argument
`synthetic`. It is one of these:

- `NULL`, the schema-only default.
- a spec, `list(generator = , dir = , invariants = , profile = )`. Each
  element is optional.
- a
  [`csdb::SyntheticTable`](https://niphr.github.io/csdb/reference/SyntheticTable.html),
  which cs9 stores as given.

cs9 builds the `SyntheticTable` of a spec from the contract of the
table: its field types, its keys and its `validator_field_contents`. So
the spec states no column twice. A generator is a
`function(profile, n, seed, world)` that returns a data.table. This one
gives every region every week of the world, and ignores `n`:

``` r
cases_generator <- function(profile, n, seed, world) {
  set.seed(seed)
  d <- CJ(isoyearweek = world$isoyearweek, region = world$region)
  d[, cases := rpois(.N, 20)]
  return(d[, .(isoyearweek, cases, region)])
}
ss$add_partitionedtable(
  name_access = "anon",
  name_grouping = "vignette",
  name_variant = "cases",
  name_partitions = c("north", "south"),
  column_name_partition = "region",
  field_types = c(isoyearweek = "TEXT", cases = "INTEGER", region = "TEXT"),
  keys = "isoyearweek",
  synthetic = list(
    generator = cases_generator,
    invariants = list(
      cases_nonnegative = function(d) {
        if (all(d$cases >= 0)) TRUE else "A count of cases is negative."
      }
    )
  )
)
ss$synthetic_tables$anon_vignette_cases$keys
#> [1] "isoyearweek" "region"
```

The keys of a partitioned table get the partition column, because two
partitions can hold the same week. cs9 also adds the invariant
`partition_known`, which refuses a region outside `name_partitions`.

The population table gets a generator with a defect. It writes the
regions in title case, and ignores the world:

``` r
add_population <- function(generator) {
  ss$add_table(
    name_access = "anon",
    name_grouping = "vignette",
    name_variant = "population",
    field_types = c(region = "TEXT", pop = "INTEGER"),
    keys = "region",
    synthetic = list(generator = generator)
  )
}
add_population(function(profile, n, seed, world) {
  return(data.table(region = c("North", "South"), pop = 50000L))
})
```

## set_synthetic()

Synthetic mode writes to the databases of the system.
`set_synthetic(TRUE)` therefore refuses unless every database is a
SQLite file under [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
It checks each entry of `config$dbconfigs` and the database of every
table. Here a production access is added to the configuration:

``` r
cfg <- cs9::config
cfg$dbconfigs$prod <- list(
  access = "prod",
  driver = "PostgreSQL Unicode",
  db = "surveillance"
)
ss$set_synthetic(TRUE)
#> Error:
#> ! Synthetic mode refuses config$dbconfigs: the driver is 'PostgreSQL Unicode', not SQLite. Synthetic mode MUST write only to a SQLite file under tempdir().
```

The check reads the configuration only. It opens no connection. Without
the production access, the call passes:

``` r
cfg$dbconfigs$prod <- NULL
ss$set_synthetic(TRUE)
ss$synthetic_mode
#> [1] TRUE
```

## set_synthetic_world()

The world is a named list of the allowed values of each column. Every
generator gets it as `world`. One world serves every table. Generators
that read it draw the same regions and weeks.

``` r
ss$set_synthetic_world(list(
  isoyearweek = c("2026-01", "2026-02", "2026-03", "2026-04"),
  region = c("north", "south")
))
print(ss$synthetic("anon_vignette_cases", n = NULL))
#> Key: <isoyearweek, region>
#>    isoyearweek cases region
#>         <char> <int> <char>
#> 1:     2026-01    17  north
#> 2:     2026-01    25  south
#> 3:     2026-02    25  north
#> 4:     2026-02    21  south
#> 5:     2026-03    13  north
#> 6:     2026-03    22  south
#> 7:     2026-04    23  north
#> 8:     2026-04    22  south
```

## populate_synthetic() and the join check

The task reads the cases and the population, and writes the weekly rate
per 100,000 to `anon_vignette_rates`.

``` r
ss$add_table(
  name_access = "anon",
  name_grouping = "vignette",
  name_variant = "rates",
  field_types = c(isoyearweek = "TEXT", region = "TEXT", rate = "DOUBLE"),
  keys = c("isoyearweek", "region")
)
print(ss$synthetic("anon_vignette_rates", n = 3))
#>    isoyearweek region      rate
#>         <char> <char>     <num>
#> 1:     2026-03  north 0.9446753
#> 2:     2026-03  south 0.6607978
#> 3:     2026-04  north 0.6291140
```

The output table has the schema-only default. Its keys take their values
from the world, and `rate` is a uniform value from 0 to 1.

cs9 finds the functions of a task by name with
[`get()`](https://rdrr.io/r/base/get.html), so they live in the global
environment:

``` r
rates_data <- function(argset, tables) {
  # A partitioned table holds one table for each partition.
  cases <- lapply(tables$cases$tables, function(x) dplyr::collect(x$tbl()))
  return(list(
    cases = rbindlist(cases),
    population = dplyr::collect(tables$population$tbl())
  ))
}
rates_action <- function(data, argset, tables) {
  d <- merge(data$cases, data$population, by = "region")
  d[, rate := 100000 * cases / pop]
  tables$rates$drop_all_rows()
  tables$rates$insert_data(d[, .(isoyearweek, region, rate)])
}
assign("rates_data", rates_data, envir = globalenv())
assign("rates_action", rates_action, envir = globalenv())
add_rates_task <- function() {
  ss$add_task(
    name_grouping = "vignette",
    name_action = "rates",
    for_each_plan = plnr::expand_list(plan = 1),
    data_selector_fn_name = "rates_data",
    action_fn_name = "rates_action",
    tables = list(
      cases = ss$partitionedtables$anon_vignette_cases,
      population = ss$tables$anon_vignette_population,
      rates = ss$tables$anon_vignette_rates
    ),
    synthetic_joins = list(
      c("anon_vignette_cases", "anon_vignette_population")
    )
  )
}
add_rates_task()
```

`populate_synthetic(task = )` fills every table in the `tables` of the
task. cs9 cannot tell an input table from an output table, so name the
inputs in `tables`.

The join check is off by default. `add_task(synthetic_joins = )`
declares join groups, and `populate_synthetic(task = )` checks only
these groups. Two tables of a group that share key columns MUST have a
row in common on them. The task declares the cases and the population as
one group. They share `region`, and the population holds no region of
the world:

``` r
ss$populate_synthetic(
  task = "vignette_rates",
  tables = c("anon_vignette_cases", "anon_vignette_population"),
  n = NULL
)
#> Error:
#> ! populate_synthetic() refuses the task: the synthetic data of the tables anon_vignette_cases and anon_vignette_population have no row in common on their shared key columns region. Nothing was written.
```

The call checks every table and builds all the data before its first
write, so a refusal writes nothing. The fix is a generator that takes
the regions from the world. The task is added again, so that it holds
the new table:

``` r
add_population(function(profile, n, seed, world) {
  return(data.table(region = world$region, pop = 50000L))
})
add_rates_task()
written <- ss$populate_synthetic(
  task = "vignette_rates",
  tables = c("anon_vignette_cases", "anon_vignette_population"),
  n = NULL
)
written
#>      anon_vignette_cases anon_vignette_population 
#>                        8                        2
```

`written` holds the number of rows written to each table. Each table
gets its own seed, from `seed` and the md5 of the table name. Two tables
then do not share a stream of random values. `dataset = "<name>"` fills
each table whose catalogue holds that dataset with it, and every other
table with random data.

The task now runs on synthetic inputs alone:

``` r
invisible(utils::capture.output(ss$run_task("vignette_rates")))
dplyr::collect(ss$tables$anon_vignette_rates$tbl())[, c("isoyearweek", "region", "rate")]
#> # A tibble: 8 × 3
#>   isoyearweek region  rate
#>   <chr>       <chr>  <dbl>
#> 1 2026-01     north     32
#> 2 2026-02     north     30
#> 3 2026-03     north     44
#> 4 2026-04     north     26
#> 5 2026-01     south     56
#> 6 2026-02     south     48
#> 7 2026-03     south     50
#> 8 2026-04     south     42
```

## add_source() and source()

In synthetic mode, a task MUST NOT read an external source, such as a
register or an SQL Server. `add_source()` registers a reader and a
simulator under one name. `source(name)` returns the simulator with
synthetic mode on, and the reader otherwise. The two MUST have the same
formal arguments.

``` r
ss$add_source(
  "register",
  reader = function(isoyearweek) stop("The register is not reachable here."),
  simulator = function(isoyearweek) data.table(isoyearweek = isoyearweek, cases = 3L)
)
ss$source("register")("2026-04")
#>    isoyearweek cases
#>         <char> <int>
#> 1:     2026-04     3
```

The call site reads `ss$source("register")(isoyearweek)` in both modes.
A simulator with other arguments is refused:

``` r
ss$add_source(
  "register_2",
  reader = function(isoyearweek) NULL,
  simulator = function(week) NULL
)
#> Error:
#> ! reader and simulator of source register_2 MUST have the same formal arguments. reader has (isoyearweek), simulator has (week).
```

With synthetic mode off, the same call reaches the reader:

``` r
ss$set_synthetic(FALSE)
ss$source("register")("2026-04")
#> Error:
#> ! The register is not reachable here.
```

## run-task.sh –synthetic

A detached run of a task of an implementation package can start in
synthetic mode. `run_task_detached(synthetic = TRUE)` passes
`--synthetic` to `run-task.sh`:

``` r
cs9::run_task_detached("my_task", package_dir = ".", synthetic = TRUE)
```

From a shell, the same run is:

``` bash
RUN=$(Rscript -e 'cat(system.file("scripts", "run-task.sh", package = "cs9"))')
"$RUN" -d . --synthetic my_task
```

The child process calls `set_synthetic(TRUE)` and
`populate_synthetic(task = "my_task", dataset = "default")` before
`run_task()`. The log header then holds `SYNTHETIC: yes`. That call has
no `tables`, so it fills every table of the task, outputs included. The
join check covers only the join groups that the task declares.

A detached run has its own
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). The implementation
package MUST therefore point its `CS9_DBCONFIG_DB_*` variables under
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) in its `.onLoad()`,
then call
[`reload_db_config()`](https://niphr.github.io/cs9/reference/reload_db_config.md).
Otherwise `set_synthetic(TRUE)` refuses in the child and the run stops.
