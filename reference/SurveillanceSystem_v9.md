# A Surveillance System Object

Core R6 class for creating and managing disease surveillance systems.
This class orchestrates database tables, tasks, and analyses for
real-time epidemiological monitoring and public health surveillance.

## Details

SurveillanceSystem_v9 provides infrastructure for:

- Database table management with automated logging.

- Task scheduling and parallel execution.

- Data validation and schema enforcement.

- Configuration and performance monitoring.

The surveillance system follows a structured approach:

1.  Define database tables with `add_table()`.

2.  Configure surveillance tasks with `add_task()`.

3.  Execute tasks with `run_task()` or external schedulers.

## Synthetic mode

Every table that `add_table()` or `add_partitionedtable()` adds gets a
[`csdb::SyntheticTable`](https://niphr.github.io/csdb/reference/SyntheticTable.html)
in `synthetic_tables`. The argument `synthetic` takes one of these:

- `NULL`: the schema-only default. It is the spec
  [`list()`](https://rdrr.io/r/base/list.html).

- a spec: a named list with the elements `generator`, `dir`,
  `invariants` and `profile`. Each element is optional.

- a
  [`csdb::SyntheticTable`](https://niphr.github.io/csdb/reference/SyntheticTable.html),
  which cs9 stores as given.

cs9 builds the `SyntheticTable` of a spec from the contract of the
table. The contract is the field types of the table without
`auto_last_updated_datetime`, the keys, and `validator_field_contents`.
With `dir`, cs9 calls
[`csdb::synthetic_table_from_dir()`](https://niphr.github.io/csdb/reference/synthetic_table_from_dir.html),
which reads `profile.yaml` from `dir`. A spec MUST NOT have both `dir`
and `profile`.

The keys of the contract of a partitioned table also hold the partition
column. Each partition is its own database table, so two partitions can
hold the same series. The spec of a partitioned table also gets the
invariant `partition_known`, which refuses a partition value outside
`name_partitions`. Without `generator`, row `i` gets the partition
`name_partitions[(i - 1) %% length(name_partitions) + 1]`.

`set_synthetic(TRUE)` stops unless every database is a SQLite file under
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). It checks each
entry of `config$dbconfigs` and the database of each table in `tables`
and `partitionedtables`. The check compares normalised paths with
[`fs::path_has_parent()`](https://fs.r-lib.org/reference/path_math.html).

`populate_synthetic()` stops unless `synthetic_mode` is `TRUE`. It stops
when it gets neither `task`, a task name or a `Task`, nor `tables`. It
fills these tables:

- `task` alone: every table in the `tables` of the task.

- `tables` alone: the tables named in `tables`.

- `task` and `tables`: only the tables named in `tables`. Each of them
  MUST be in the `tables` of the task. Otherwise the call stops, and the
  error names the tables that are not.

cs9 cannot tell the input tables of a task from its output tables. When
the `tables` of a task hold output tables, name the input tables in
`tables`.

It then works in this order:

1.  It checks the database of every table, and of every partition. One
    database that is not SQLite under
    [`tempdir()`](https://rdrr.io/r/base/tempfile.html) stops the whole
    call.

2.  It gets the data of each table. With `dataset`, a table whose
    catalogue holds a dataset of that name gets that dataset. Every
    other table gets `$random(n, seed)`, and a message names those
    tables. With a number `n`, a partitioned table gets
    `max(n, length(name_partitions))` rows. Every table gets `n = NULL`
    unchanged, which means the generator's own size. With `n = NULL`,
    the default generator of a partitioned table draws the size of
    [`csdb::simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.html),
    and at least `length(name_partitions)` rows. The default generator
    therefore fills every partition. A custom generator decides which
    partitions get rows.

3.  It validates the data against the contract of the table that
    receives it. A `SyntheticTable` from the caller can be looser than
    the table.

4.  With `task`, it checks the join groups of the task. The next
    paragraphs describe this join check.

5.  For each table, it checks the database of the table again, then
    calls `drop_all_rows()`, then `insert_data()`. `drop_all_rows()`
    empties every partition, so a partition that the data does not name
    is left empty.

A failure in steps 1 to 4 therefore writes nothing. The check in step 5
refuses a database that changed after step 1.

`add_task(synthetic_joins = )` declares the join groups of a task, and
cs9 stores them in the field `synthetic_joins` by task name. A join
group is a character vector of at least 2 table names of the `tables` of
the task. `add_task()` stops when a group names another table. Two
tables of a group that share key columns MUST have a non-empty inner
join on the shared key columns, the partition column included. Otherwise
`populate_synthetic()` stops, and the error names the two tables and the
shared columns. A pair with no shared key column is not checked.

The join check is off by default. A task with no join groups gets no
join check, and `tables` alone gets none. With `task` and `tables`, each
group keeps only the tables that the call fills. A group with fewer than
2 of these tables is skipped.

Each table gets its own seed, so two tables do not share a stream of
random values. The seed of a table is `seed` plus the first 7
hexadecimal digits of the md5 of the table name, modulo
`.Machine$integer.max`. `synthetic(table_name, seed = s)` uses `s` as
given.

A detached run has its own
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). With
`run-task.sh --synthetic`, the implementation package MUST point its
`CS9_DBCONFIG_DB_*` variables under
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) in its `.onLoad()`.
It MUST then call
[`reload_db_config()`](https://niphr.github.io/cs9/reference/reload_db_config.md).

`source(name)` returns the `simulator` of `add_source()` when
`synthetic_mode` is `TRUE`, and the `reader` otherwise. `add_source()`
stops unless the `reader` and the `simulator` have the same names of
formal arguments, in the same order. The error names both lists.

## Public fields

- `tables`:

  List of database tables managed by the surveillance system.

- `partitionedtables`:

  List of partitioned database tables.

- `tasks`:

  List of surveillance tasks configured for execution.

- `name`:

  Character string identifying the surveillance system instance.

- `implementation_version`:

  Character string tracking the analytics code version.

- `synthetic_tables`:

  Named list of
  [`csdb::SyntheticTable`](https://niphr.github.io/csdb/reference/SyntheticTable.html),
  by table name.

- `synthetic_mode`:

  `TRUE` when synthetic mode is on. Change it with `set_synthetic()`.

- `synthetic_world`:

  `NULL`, or the named list that every generator gets as `world`.

- `synthetic_joins`:

  Named list of the join groups of `add_task(synthetic_joins = )`, by
  task name.

- `sources`:

  Named list of sources from `add_source()`. Each holds a `reader` and a
  `simulator`.

## Methods

### Public methods

- [`SurveillanceSystem_v9$new()`](#method-SurveillanceSystem_v9-initialize)

- [`SurveillanceSystem_v9$add_table()`](#method-SurveillanceSystem_v9-add_table)

- [`SurveillanceSystem_v9$add_partitionedtable()`](#method-SurveillanceSystem_v9-add_partitionedtable)

- [`SurveillanceSystem_v9$add_task()`](#method-SurveillanceSystem_v9-add_task)

- [`SurveillanceSystem_v9$get_task()`](#method-SurveillanceSystem_v9-get_task)

- [`SurveillanceSystem_v9$run_task()`](#method-SurveillanceSystem_v9-run_task)

- [`SurveillanceSystem_v9$shortcut_get_tables()`](#method-SurveillanceSystem_v9-shortcut_get_tables)

- [`SurveillanceSystem_v9$shortcut_get_argset()`](#method-SurveillanceSystem_v9-shortcut_get_argset)

- [`SurveillanceSystem_v9$shortcut_get_data()`](#method-SurveillanceSystem_v9-shortcut_get_data)

- [`SurveillanceSystem_v9$shortcut_get_plans_argsets_as_dt()`](#method-SurveillanceSystem_v9-shortcut_get_plans_argsets_as_dt)

- [`SurveillanceSystem_v9$shortcut_get_num_analyses()`](#method-SurveillanceSystem_v9-shortcut_get_num_analyses)

- [`SurveillanceSystem_v9$set_synthetic()`](#method-SurveillanceSystem_v9-set_synthetic)

- [`SurveillanceSystem_v9$set_synthetic_world()`](#method-SurveillanceSystem_v9-set_synthetic_world)

- [`SurveillanceSystem_v9$synthetic()`](#method-SurveillanceSystem_v9-synthetic)

- [`SurveillanceSystem_v9$populate_synthetic()`](#method-SurveillanceSystem_v9-populate_synthetic)

- [`SurveillanceSystem_v9$add_source()`](#method-SurveillanceSystem_v9-add_source)

- [`SurveillanceSystem_v9$source()`](#method-SurveillanceSystem_v9-source)

- [`SurveillanceSystem_v9$clone()`](#method-SurveillanceSystem_v9-clone)

------------------------------------------------------------------------

### `SurveillanceSystem_v9$new()`

#### Usage

    SurveillanceSystem_v9$new(
      name = "unspecified",
      implementation_version = "unspecified"
    )

#### Arguments

- `name`:

  A string that you may use to track performance metrics (runtime and
  RAM usage).

- `implementation_version`:

  A string that you may use to track performance metrics (runtime and
  RAM usage).

------------------------------------------------------------------------

### `SurveillanceSystem_v9$add_table()`

Add a table.

#### Usage

    SurveillanceSystem_v9$add_table(
      name_access,
      name_grouping = NULL,
      name_variant = NULL,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank,
      synthetic = NULL
    )

#### Arguments

- `name_access`:

  First part of table name, corresponding to the database where it will
  be stored.

- `name_grouping`:

  Second part of table name, corresponding to some sort of grouping.

- `name_variant`:

  Final part of table name, corresponding to a distinguishing variant.

- `field_types`:

  Named character vector, where the names are the column names, and the
  values are the column types. Valid types are BOOLEAN, CHARACTER,
  INTEGER, DOUBLE, DATE, DATETIME.

- `keys`:

  Character vector, containing the column names that uniquely identify a
  row of data.

- `indexes`:

  Named list, containing indexes.

- `validator_field_types`:

  Function corresponding to a validator for the field types.

- `validator_field_contents`:

  Function corresponding to a validator for the field contents.

- `synthetic`:

  `NULL` for the schema-only default, a spec
  `list(generator = , dir = , invariants = , profile = )`, or a
  [`csdb::SyntheticTable`](https://niphr.github.io/csdb/reference/SyntheticTable.html).
  See the section "Synthetic mode".

#### Returns

No return value. This method is called for its side effect of adding a
table to the surveillance system.

#### Examples

    global$ss$add_table(
      name_access = c("anon"),
      name_grouping = "example_weather",
      name_variant = "data",
      field_types =  c(
        "granularity_time" = "TEXT",
        "granularity_geo" = "TEXT",
        "country_iso3" = "TEXT",
        "location_code" = "TEXT",
        "border" = "INTEGER",
        "age" = "TEXT",
        "sex" = "TEXT",

        "isoyear" = "INTEGER",
        "isoweek" = "INTEGER",
        "isoyearweek" = "TEXT",
        "season" = "TEXT",
        "seasonweek" = "DOUBLE",

        "calyear" = "INTEGER",
        "calmonth" = "INTEGER",
        "calyearmonth" = "TEXT",

        "date" = "DATE",

        "temp_max" = "DOUBLE",
        "temp_min" = "DOUBLE",
        "precip" = "DOUBLE"
      ),
      keys = c(
        "granularity_time",
        "location_code",
        "date",
        "age",
        "sex"
      ),
      validator_field_types = csdb::validator_field_types_csfmt_rts_data_v1,
      validator_field_contents = csdb::validator_field_contents_csfmt_rts_data_v1
    )

------------------------------------------------------------------------

### `SurveillanceSystem_v9$add_partitionedtable()`

Add a partitioned table to the surveillance system.

#### Usage

    SurveillanceSystem_v9$add_partitionedtable(
      name_access,
      name_grouping = NULL,
      name_variant = NULL,
      name_partitions = "default",
      column_name_partition = "partition",
      value_generator_partition = NULL,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank,
      synthetic = NULL
    )

#### Arguments

- `name_access`:

  First part of table name, corresponding to the database where it will
  be stored.

- `name_grouping`:

  Second part of table name, corresponding to some sort of grouping.

- `name_variant`:

  Final part of table name, corresponding to a distinguishing variant.

- `name_partitions`:

  Character string specifying partition naming scheme.

- `column_name_partition`:

  Column name used for partitioning.

- `value_generator_partition`:

  Function to generate partition values.

- `field_types`:

  Named character vector of column names and types.

- `keys`:

  Character vector of column names that uniquely identify rows.

- `indexes`:

  Named list containing index definitions.

- `validator_field_types`:

  Function to validate field types.

- `validator_field_contents`:

  Function to validate field contents.

- `synthetic`:

  `NULL` for the schema-only default, a spec
  `list(generator = , dir = , invariants = , profile = )`, or a
  [`csdb::SyntheticTable`](https://niphr.github.io/csdb/reference/SyntheticTable.html).
  See the section "Synthetic mode".

#### Returns

No return value. This method is called for its side effect of adding a
partitioned table to the surveillance system.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$add_task()`

Add a surveillance task to the system. `run_task()` builds the plans
again on every call, so `argset$today` and `argset$yesterday` hold the
date of the run.

#### Usage

    SurveillanceSystem_v9$add_task(
      name_grouping = NULL,
      name_action = NULL,
      name_variant = NULL,
      cores = 1,
      permission = NULL,
      plan_analysis_fn_name = NULL,
      for_each_plan = NULL,
      for_each_analysis = NULL,
      universal_argset = NULL,
      upsert_at_end_of_each_plan = FALSE,
      insert_at_end_of_each_plan = FALSE,
      action_fn_name,
      data_selector_fn_name = NULL,
      tables = NULL,
      synthetic_joins = NULL
    )

#### Arguments

- `name_grouping`:

  Name of the task (grouping).

- `name_action`:

  Name of the task (action).

- `name_variant`:

  Name of the task (variant).

- `cores`:

  The number of CPU cores, or a function with no arguments that returns
  it. The task calls the function each time it runs, so the function MAY
  read an environment variable that changes after `add_task()`. A runner
  that sets `cores` to 1, such as `TaskJob`, replaces the function.

- `permission`:

  A permission R6 instance.

- `plan_analysis_fn_name`:

  The name of a function that returns a named list
  `list(for_each_plan = list(), for_each_analysis = NULL)`.

- `for_each_plan`:

  A list, where each unit corresponds to one data extraction. You SHOULD
  use
  [`plnr::expand_list`](https://www.rwhite.no/plnr/reference/expand_list.html).

- `for_each_analysis`:

  A list, where each unit corresponds to one analysis within a plan
  (data extraction). You SHOULD use
  [`plnr::expand_list`](https://www.rwhite.no/plnr/reference/expand_list.html).

- `universal_argset`:

  A list, where these argsets are applied to all analyses universally.

- `upsert_at_end_of_each_plan`:

  Whether to upsert your results automatically at the end of each plan.

- `insert_at_end_of_each_plan`:

  Whether to insert your results automatically at the end of each plan.

- `action_fn_name`:

  The name of the function called for each analysis with arguments
  `data`, `argset`, `schema`.

- `data_selector_fn_name`:

  The name of a function called to obtain the data for each analysis.
  The function MUST have the arguments `argset`, `schema` and MUST
  return a named list.

- `tables`:

  A named list that maps `cs9::config$schemas` for use in
  `action_fn_name` and `data_selector_fn_name`.

- `synthetic_joins`:

  `NULL`, or a list of join groups for `populate_synthetic()`. A join
  group is a character vector of at least 2 table names of `tables`. See
  the section "Synthetic mode".

#### Returns

No return value. This method is called for its side effect of adding a
task to the surveillance system.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$get_task()`

Get a surveillance task by name. The task builds its plans only when it
has none, so it shows the plans of the last build. Call `task$run()` on
the result to run plans that you edit by hand, because `run_task()`
builds the plans again.

#### Usage

    SurveillanceSystem_v9$get_task(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name.

#### Returns

A Task R6 object representing the surveillance task.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$run_task()`

Execute a surveillance task by name. The task builds its plans once on
every call, before any plan runs and before a worker forks. The plans do
not change during the run, and `argset$today` is the date of the run. A
run started with a `run_key`, which a scheduler can pass, stages its
writes and changes the task's tables only after every plan succeeds. An
interactive run writes directly, as before. See
[`vignette("rebuilding-a-table-safely")`](https://niphr.github.io/cs9/articles/rebuilding-a-table-safely.md).

#### Usage

    SurveillanceSystem_v9$run_task(task_name, run_key = NULL)

#### Arguments

- `task_name`:

  Character string specifying the task name to run.

- `run_key`:

  A string that names the run, or NULL. With NULL the run writes the
  task's tables directly. A second call with the same `run_key` resumes
  a failed run. It skips each plan that the failed run finished, except
  plan 1.

#### Returns

No return value. This method is called for its side effect of executing
the task.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$shortcut_get_tables()`

Get database tables associated with a task.

#### Usage

    SurveillanceSystem_v9$shortcut_get_tables(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name.

#### Returns

A named list of database table objects used by the task.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$shortcut_get_argset()`

Get argument set for a specific plan and analysis.

#### Usage

    SurveillanceSystem_v9$shortcut_get_argset(
      task_name,
      index_plan = 1,
      index_analysis = 1
    )

#### Arguments

- `task_name`:

  Character string specifying the task name.

- `index_plan`:

  Integer specifying which plan to access.

- `index_analysis`:

  Integer specifying which analysis to access.

#### Returns

A named list containing the argument set for the specified plan and
analysis.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$shortcut_get_data()`

Get data for a specific plan.

#### Usage

    SurveillanceSystem_v9$shortcut_get_data(task_name, index_plan = 1)

#### Arguments

- `task_name`:

  Character string specifying the task name.

- `index_plan`:

  Integer specifying which plan to access.

#### Returns

A named list containing the data extracted for the specified plan.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$shortcut_get_plans_argsets_as_dt()`

Get plans and argsets as a data.table.

#### Usage

    SurveillanceSystem_v9$shortcut_get_plans_argsets_as_dt(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name.

#### Returns

A data.table containing plan and analysis information with columns
including index_plan and index_analysis.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$shortcut_get_num_analyses()`

Get the total number of analyses for a task.

#### Usage

    SurveillanceSystem_v9$shortcut_get_num_analyses(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name.

#### Returns

Integer value representing the total number of analyses across all plans
for the task.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$set_synthetic()`

Turn synthetic mode on or off. See the section "Synthetic mode".

#### Usage

    SurveillanceSystem_v9$set_synthetic(on = TRUE)

#### Arguments

- `on`:

  `TRUE` or `FALSE`. `TRUE` stops unless every database is a SQLite file
  under [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

------------------------------------------------------------------------

### `SurveillanceSystem_v9$set_synthetic_world()`

Set the `world` that every generator gets.

#### Usage

    SurveillanceSystem_v9$set_synthetic_world(world)

#### Arguments

- `world`:

  `NULL`, or a named list.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$synthetic()`

Get validated synthetic data for one table.

#### Usage

    SurveillanceSystem_v9$synthetic(
      table_name,
      dataset = NULL,
      n = 1000L,
      seed = 1L
    )

#### Arguments

- `table_name`:

  The name of the table.

- `dataset`:

  `NULL` for `$random(n, seed)`, or the name of a fixed dataset for
  `$dataset(dataset)`.

- `n, seed`:

  The number of rows and the seed of random data.

#### Returns

A validated data.table.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$populate_synthetic()`

Replace the rows of tables with synthetic data. See the section
"Synthetic mode".

#### Usage

    SurveillanceSystem_v9$populate_synthetic(
      task = NULL,
      tables = NULL,
      dataset = NULL,
      n = 1000L,
      seed = 1L
    )

#### Arguments

- `task`:

  `NULL`, a task name or a `Task`. The method fills every table in its
  `tables`.

- `tables`:

  `NULL`, or a character vector of table names to fill.

- `dataset, n, seed`:

  A table whose catalogue holds the dataset `dataset` gets that dataset.
  Every other table gets `$random(n, seed)`, with its own seed from
  `seed` and the table name.

#### Returns

Invisibly, the number of rows inserted, named by table.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$add_source()`

Add a source that synthetic mode can switch.

#### Usage

    SurveillanceSystem_v9$add_source(name, reader, simulator)

#### Arguments

- `name`:

  The name of the source.

- `reader, simulator`:

  The functions that [`source()`](https://rdrr.io/r/base/source.html)
  returns with synthetic mode off and on. They MUST have the same names
  of formal arguments.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$source()`

Get a source: the simulator with synthetic mode on, else the reader.

#### Usage

    SurveillanceSystem_v9$source(name)

#### Arguments

- `name`:

  The name of the source.

#### Returns

A function.

------------------------------------------------------------------------

### `SurveillanceSystem_v9$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SurveillanceSystem_v9$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create surveillance system
ss <- SurveillanceSystem_v9$new(
  name = "covid_surveillance",
  implementation_version = "1.0"
)

# Add database table
ss$add_table(
  name_access = "anon",
  name_grouping = "covid",
  name_variant = "cases",
  field_types = c("date" = "DATE", "cases" = "INTEGER"),
  keys = c("date")
)

# Add surveillance task
ss$add_task(
  name_grouping = "covid",
  name_action = "import",
  name_variant = "daily_data",
  action_fn_name = "import_covid_data",
  data_selector_fn_name = "select_covid_sources"
)

# Run task
ss$run_task("covid_import_daily_data")
} # }


## ------------------------------------------------
## Method `SurveillanceSystem_v9$add_table()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
global$ss$add_table(
  name_access = c("anon"),
  name_grouping = "example_weather",
  name_variant = "data",
  field_types =  c(
    "granularity_time" = "TEXT",
    "granularity_geo" = "TEXT",
    "country_iso3" = "TEXT",
    "location_code" = "TEXT",
    "border" = "INTEGER",
    "age" = "TEXT",
    "sex" = "TEXT",

    "isoyear" = "INTEGER",
    "isoweek" = "INTEGER",
    "isoyearweek" = "TEXT",
    "season" = "TEXT",
    "seasonweek" = "DOUBLE",

    "calyear" = "INTEGER",
    "calmonth" = "INTEGER",
    "calyearmonth" = "TEXT",

    "date" = "DATE",

    "temp_max" = "DOUBLE",
    "temp_min" = "DOUBLE",
    "precip" = "DOUBLE"
  ),
  keys = c(
    "granularity_time",
    "location_code",
    "date",
    "age",
    "sex"
  ),
  validator_field_types = csdb::validator_field_types_csfmt_rts_data_v1,
  validator_field_contents = csdb::validator_field_contents_csfmt_rts_data_v1
)
} # }
```
