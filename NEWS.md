# Version 25.8.21

## Documentation
* Organized pkgdown reference documentation with logical function groupings
* Enhanced reference structure with clear categories for different types of functions
* Improved pkgdown configuration for better navigation of package documentation

# Version 25.7.31

## New Features
- Enhanced environment variable validation with detailed diagnostic function `check_environment_setup()`
- Improved graceful degradation for CRAN compatibility - package loads with limited functionality when database infrastructure is not available
- Context-aware environment variable validation with clear user guidance
- Robust error handling in package loading process

## Improvements  
- Updated system environment configuration handling
- Comprehensive startup messages guide users through configuration issues
- Enhanced database connection error handling
- Improved package loading sequence with better error isolation

## Documentation
- Comprehensive installation vignette explaining infrastructure requirements
- Enhanced function documentation with CRAN-compatible examples
- Clear guidance on functionality available in different deployment scenarios
- Added comprehensive package-level documentation (`?cs9`)
- Enhanced `check_environment_setup()` documentation with detailed examples
- Updated vignettes with CRAN vs. full setup guidance

## CRAN Preparation
- Removed fhiplot dependency (replaced with standard R functions)
- Fixed non-portable file names in vignettes directory
- Cleaned up build artifacts and hidden files
- Updated LICENSE file copyright year to 2025
- Created vignette precompiler system for maintainable documentation
- Verified graceful degradation in minimal environments

## Bug Fixes
- Fixed package loading issues in environments without database configuration
- Improved error messages for missing environment variables
- Enhanced database table setup error handling

# Version 2025.3.6

- Automatically logging when tasks start running.

# Version 2025.2.24

### New Features
- Added `get_config_log()` function to retrieve configuration log entries from the `config_log` table.  
  - Supports optional filtering by surveillance system (`ss`), task name (`task`), and date range (`start_date`, `end_date`).
  - Returns a `data.table` with the filtered entries.

### Improvements
- Updated `update_config_log()` to also route custom messages (`...`) to the `message()` function for clearer console output.

# Version 2025.2.21

- **Added `update_config_log` function**. Logs configuration updates including surveillance system (`ss`), task name (`task`), and a custom `message`.

# Version 2024.6.17

- Allows for `CS9_DBCONFIG_ROLE_CREATE_TABLE` in environmental variables.

# Version 2024.6.17

- When running in parallel, a seed is set according to the index of the first analysis in each plan.

# Version 2024.6.6

- Partition table names are now 'xxxpartitionxxx' not 'PARTITION'

# Version 2024.3.7

- Including confirm_insert_via_nrow in DBtables. Checks nrow() before insert and after insert. If nrow() has not increased sufficiently, then attempt an upsert.

# Version 2023.8.1

- cs9::path now uses _interactive instead of interactive.

# Version 2023.5.3

* In R 4.3.0 `as.character(lubridate::now())` adds microseconds, which breaks the SQL upload. This is now replaced by `cstime::now_c()`.

# Version 2023.4.14

* Changed "success" to "succeeded" in `update_config_tasks_stats`

# Version 2023.4.13

* `DBPartitionedTableExtended_v9$info()` bug fixed with argument `collapse=TRUE`.
* Inclusion of `confirm_indexes` in `DBPartitionedTableExtended_v9`.

# Version 2023.4.12

* `DBPartitionedTableExtended_v9$nrow()` now has a new argument `collapse=FALSE` that provides partion-specific results
* `DBPartitionedTableExtended_v9$info()` now includes sizes in MB

# Version 2023.4.3

* Bug fix in `DBPartitionedTableExtended_v9$nrow()`

# Version 2023.4.2

* Extension of `DBPartitionedTableExtended_v9` so that it is easier to use multiple partitioning variables.

# Version 2023.4.1

* Inclusion of `partitions_randomized` in `DBPartitionedTableExtended_v9` so that when running in parallel, the database tables don't get locked.
* Inclusion of `remove_table` in `DBPartitionedTableExtended_v9`
* Fixed an error in RAM calculation in parallel for `get_config_tasks_stats`

# Version 2023.3.31

* `DBTableExtended_v9` now automatically includes a column for all tables, called `auto_last_updated_datetime`, which is automatically calculated each time that row is changed.
* Creation of `DBPartitionedTableExtended_v9`, which allows for one dataset to be partitioned amongst multiple SQL tables automatically.

# Version 2023.3.8

* `SurveillanceSystem_v9` constructor now takes in an argument called `implementation_version`, which can be used to identify what version of analytics code is currently being run.
* `update_config_last_updated` has now been replaced by `update_config_tables_last_updated` (which contains when the tables were last updated) and `config_tasks_stats` (which contains all the runtimes of the tasks).
* `SurveillanceSystem_v9` now uses an internal R6 class `DBTableExtended_v9` (which extends `csdb::DBTable_v9`) instead of using `csdb::DBTable_v9` directly. `DBTableExtended_v9` calls `update_config_last_updated` after altering a database table.

# Version 2023.3.7

* sc8 is deprecated in favor of cs9.

# Version 8.0.2

* Allows for multiple databases to be used for different access levels.
* `copy_into_new_table_where` now also copies indexes.
* V8 schemas now have a nice print function.
* V8 redirects now have a nice print function.
* `copy_into_new_table_where` uses tablock.
* `upsert_at_end_of_each_plan` and `insert_at_end_of_each_plan` can now take named lists as the return value from the `action_fn`.
* Custom progressr handler.

# Version 8.0.1

* When using `sc8::add_task_from_config_v8` the schema list is checked to make sure they are actually schemas. This will solve the issue where people incorrectly add non-existent schemas to the task.
* `insert_data`, `upsert_data`, `drop_all_rows_and_then_insert_data` are now the recommended ways of inserting data
* `addin_load_production`
* schemas now use `load_folder_fn`, which should dynamically check if a user has permission to write to a folder, solving permissions errors
* Including `tm_get_schema_names`
* Both `granularity_time` AND `granularity_geo` are now included in db censors
* Requires R >= 4.1.0
* `sc8::config$plan_attempt_index` now exists. When running plans in parallel, if a plan fails it is retried five times. This lets a user track which attempt they are on. This is mostly useful so that emails and smses are only sent when  `sc8::config$plan_attempt_index==1`
* (Disabled) TABLOCK is disabled right now due to issues where data would not be uploaded.
* (Disabled) Data is sorted before sending it to bcp to speed up in/upserts.

# Version 8.0.0

* Release of schema redirects that allow for restricted and anonymous datasets to be seamlessly used by people with different access rights
* Consistent naming of `task_from_config_v8` and `add_schema_v8`

# Version 7.1.4

* `db_insert_data`, `db_upsert_data`, `db_drop_all_rows_and_then_upsert_data` are now the recommended ways of inserting data

# Version 7.1.3

* `update_config_datetime` and `get_config_datetime` now automatically record database table updates as well

# Version 7.1.2

* Updating default db schemas to be more explicit with the useage of isotime.

# Version 7.1.1

* `qsenc_save` and `qsenc_read` to save/read to/from encrypted files.

# Version 7.1.0

* `task_from_config_v3` sets a new direction for creation of tasks and management of tasks
* `describe_tasks` and `describe_schemas` help with automatic documentation

# Version 7.0.8

* `task_inline_v1` allows for easy inline task creation
* Corresponding RStudio addin for inline tasks that copy from one db table to another

# Version 7.0.7

* `copy_into_new_table_where` allows for the creation of a new table from an old table
* Including `task_from_config_v2` 
* First RStudio addin

# Version 7.0.6

* `write_data_infile` now checks for Infinite/NaN values and sets them to NA

# Version 7.0.5

* `Task` now includes `action_before_fn` and `action_after_fn`

# Version 7.0.4

* `validator_field_contents_sykdomspulsen` now allows `baregion` as a valid `granularity_geo`

# Version 7.0.3

* `tm_get_plans_argsets_as_dt` provides an overview of the plans and argsets within a task

# Version 7.0.2

* `keep_rows_where` now also retains the PK constraints
