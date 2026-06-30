# A Surveillance System Object

Core R6 class for creating and managing disease surveillance systems.
This class orchestrates database tables, tasks, and analyses for
real-time epidemiological monitoring and public health surveillance.

## Details

SurveillanceSystem_v9 provides infrastructure for:

- Database table management with automated logging

- Task scheduling and parallel execution

- Data validation and schema enforcement

- Configuration and performance monitoring

The surveillance system follows a structured approach:

1.  Define database tables with `add_table()`

2.  Configure surveillance tasks with `add_task()`

3.  Execute tasks with `run_task()` or external schedulers

## Public fields

- `tables`:

  List of database tables managed by the surveillance system

- `partitionedtables`:

  List of partitioned database tables

- `tasks`:

  List of surveillance tasks configured for execution

- `name`:

  Character string identifying the surveillance system instance

- `implementation_version`:

  Character string tracking the analytics code version

## Methods

### Public methods

- [`SurveillanceSystem_v9$new()`](#method-SurveillanceSystem_v9-new)

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

- [`SurveillanceSystem_v9$clone()`](#method-SurveillanceSystem_v9-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    SurveillanceSystem_v9$new(
      name = "unspecified",
      implementation_version = "unspecified"
    )

#### Arguments

- `name`:

  A string that the user may choose to use to track performance metrics
  (runtime and RAM usage)

- `implementation_version`:

  A string that the user may choose to use to track performance metrics
  (runtime and RAM usage)

------------------------------------------------------------------------

### Method `add_table()`

Add a table

#### Usage

    SurveillanceSystem_v9$add_table(
      name_access,
      name_grouping = NULL,
      name_variant = NULL,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
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
  INTEGER, DOUBLE, DATE, DATETIME

- `keys`:

  Character vector, containing the column names that uniquely identify a
  row of data.

- `indexes`:

  Named list, containing indexes.

- `validator_field_types`:

  Function corresponding to a validator for the field types.

- `validator_field_contents`:

  Function corresponding to a validator for the field contents.

#### Returns

No return value. This method is called for its side effect of adding a
table to the surveillance system.

#### Examples

    \dontrun{
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
    }

------------------------------------------------------------------------

### Method `add_partitionedtable()`

Add a partitioned table to the surveillance system

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
      validator_field_contents = csdb::validator_field_contents_blank
    )

#### Arguments

- `name_access`:

  First part of table name, corresponding to the database where it will
  be stored

- `name_grouping`:

  Second part of table name, corresponding to some sort of grouping

- `name_variant`:

  Final part of table name, corresponding to a distinguishing variant

- `name_partitions`:

  Character string specifying partition naming scheme

- `column_name_partition`:

  Column name used for partitioning

- `value_generator_partition`:

  Function to generate partition values

- `field_types`:

  Named character vector of column names and types

- `keys`:

  Character vector of column names that uniquely identify rows

- `indexes`:

  Named list containing index definitions

- `validator_field_types`:

  Function to validate field types

- `validator_field_contents`:

  Function to validate field contents

#### Returns

No return value. This method is called for its side effect of adding a
partitioned table to the surveillance system.

------------------------------------------------------------------------

### Method `add_task()`

Add a surveillance task to the system

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
      tables = NULL
    )

#### Arguments

- `name_grouping`:

  Name of the task (grouping)

- `name_action`:

  Name of the task (action)

- `name_variant`:

  Name of the task (variant)

- `cores`:

  Number of CPU cores

- `permission`:

  A permission R6 instance

- `plan_analysis_fn_name`:

  The name of a function that returns a named list
  `list(for_each_plan = list(), for_each_analysis = NULL)`.

- `for_each_plan`:

  A list, where each unit corresponds to one data extraction. Generally
  recommended to use
  [`plnr::expand_list`](https://www.rwhite.no/plnr/reference/expand_list.html).

- `for_each_analysis`:

  A list, where each unit corresponds to one analysis within a plan
  (data extraction). Generally recommended to use
  [`plnr::expand_list`](https://www.rwhite.no/plnr/reference/expand_list.html).

- `universal_argset`:

  A list, where these argsets are applied to all analyses univerally

- `upsert_at_end_of_each_plan`:

  Do you want to upsert your results automatically at the end of each
  plan?

- `insert_at_end_of_each_plan`:

  Do you want to insert your results automatically at the end of each
  plan?

- `action_fn_name`:

  The name of the function that will be called for each analysis with
  arguments `data`, `argset`, `schema`

- `data_selector_fn_name`:

  The name of a function that will be called to obtain the data for each
  analysis. The function must have the arguments `argset`, `schema` and
  must return a named list.

- `tables`:

  A named list that maps `cs9::config$schemas` for use in
  `action_fn_name` and `data_selector_fn_name`

#### Returns

No return value. This method is called for its side effect of adding a
task to the surveillance system.

------------------------------------------------------------------------

### Method `get_task()`

Get a surveillance task by name

#### Usage

    SurveillanceSystem_v9$get_task(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name

#### Returns

A Task R6 object representing the surveillance task

------------------------------------------------------------------------

### Method `run_task()`

Execute a surveillance task by name

#### Usage

    SurveillanceSystem_v9$run_task(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name to run

#### Returns

No return value. This method is called for its side effect of executing
the task.

------------------------------------------------------------------------

### Method `shortcut_get_tables()`

Get database tables associated with a task

#### Usage

    SurveillanceSystem_v9$shortcut_get_tables(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name

#### Returns

A named list of database table objects used by the task

------------------------------------------------------------------------

### Method `shortcut_get_argset()`

Get argument set for a specific plan and analysis

#### Usage

    SurveillanceSystem_v9$shortcut_get_argset(
      task_name,
      index_plan = 1,
      index_analysis = 1
    )

#### Arguments

- `task_name`:

  Character string specifying the task name

- `index_plan`:

  Integer specifying which plan to access

- `index_analysis`:

  Integer specifying which analysis to access

#### Returns

A named list containing the argument set for the specified plan and
analysis

------------------------------------------------------------------------

### Method `shortcut_get_data()`

Get data for a specific plan

#### Usage

    SurveillanceSystem_v9$shortcut_get_data(task_name, index_plan = 1)

#### Arguments

- `task_name`:

  Character string specifying the task name

- `index_plan`:

  Integer specifying which plan to access

#### Returns

A named list containing the data extracted for the specified plan

------------------------------------------------------------------------

### Method `shortcut_get_plans_argsets_as_dt()`

Get plans and argsets as a data.table

#### Usage

    SurveillanceSystem_v9$shortcut_get_plans_argsets_as_dt(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name

#### Returns

A data.table containing plan and analysis information with columns
including index_plan and index_analysis

------------------------------------------------------------------------

### Method `shortcut_get_num_analyses()`

Get the total number of analyses for a task

#### Usage

    SurveillanceSystem_v9$shortcut_get_num_analyses(task_name)

#### Arguments

- `task_name`:

  Character string specifying the task name

#### Returns

Integer value representing the total number of analyses across all plans
for the task

------------------------------------------------------------------------

### Method `clone()`

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
## Method `SurveillanceSystem_v9$add_table`
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
