# Changelog

## Version 26.8.6

### Licensing

- The copyright holder is now **Folkehelseinstituttet**. It read “Core
  Surveillance”, which names the package family rather than a legal
  entity.
- `DESCRIPTION` `Authors@R` now declares that holder with
  `role = "cph"`. It declared no copyright holder at all, and neither
  did any other package in the fleet. Nothing in `R CMD check` reports
  that.
- The copyright year is now 2026. It read 2025.
- `CLAUDE.md` now carries a Licensing section, so the year gets checked
  rather than silently ageing.

### Documentation

- Repository prose rewritten to ASD-STE100 (Simplified Technical
  English). Every sentence in the roxygen blocks, the five vignettes,
  `README.md`, `index.md` and `NEWS.md` is now at most 25 words. 35
  sentences were over that limit before, counted over the whole
  repository. They divide into 2 in roxygen, 15 in the vignettes, 16 in
  `NEWS.md` and 2 in `index.md`. The vignette count of 15 covers both
  the built `.Rmd` files and their `.Rmd.orig` sources. No claim,
  number, condition or attribution changed. The NorSySS figures, the
  106-diseases and 378-locations counts, and the White & Valcarcel
  Salamanca attribution are unaltered.
- Long sentences that buried a sequence are split into one idea each.
  Three places stated three conditions in a single sentence and now
  state one per sentence. They are the `SET ROLE ""` trap in
  [`vignette("backends")`](https://niphr.github.io/cs9/articles/backends.md),
  the same trap in
  [`vignette("installation")`](https://niphr.github.io/cs9/articles/installation.md),
  and the `csdb` version-floor entry in `NEWS.md`.
- RFC-2119 keywords are capitalised where the vignettes and roxygen
  state an obligation. `CS9_PATH` MUST NOT be empty.
  `CS9_DBCONFIG_ACCESS` MUST include `config`. The function named by
  `data_selector_fn_name` MUST return a named list.
- Roxygen field, parameter and return descriptions now end in a full
  stop. The `TaskJob` method summary is a list. It was an indented
  block, which Rd collapsed into one paragraph.
- Vignette prose edits were applied identically to each precompiled
  `.Rmd` and its `.Rmd.orig` source. `vignettes/_PRECOMPILER.R`
  therefore still reproduces the `.Rmd` from the `.orig`.

## Version 26.8.5

### New Features

- `CS9_DBCONFIG_DRIVER=SQLite` is accepted, matched case-insensitively.
  SQLite is a file rather than a server. `CS9_DBCONFIG_SERVER` and
  `CS9_DBCONFIG_PORT` therefore moved out of the always-required tier of
  [`check_environment_setup()`](https://niphr.github.io/cs9/reference/check_environment_setup.md).
  Only the server-based drivers now require them. A SQLite environment
  needs `CS9_AUTO`, `CS9_PATH`, `CS9_DBCONFIG_ACCESS`,
  `CS9_DBCONFIG_DRIVER` and one `CS9_DBCONFIG_DB_<ACCESS>` file path per
  access. It needs no `CS9_DBCONFIG_USER`, no `CS9_DBCONFIG_PASSWORD`
  and no `CS9_DBCONFIG_SCHEMA_*`.
- [`check_environment_setup()`](https://niphr.github.io/cs9/reference/check_environment_setup.md)
  now rejects a `CS9_DBCONFIG_ACCESS` list that omits `config`. The four
  configuration tables are built from that access unconditionally, so
  its absence used to fail later and obscurely.
- [`reload_db_config()`](https://niphr.github.io/cs9/reference/reload_db_config.md)
  — new export, no arguments. Re-reads every `CS9_DBCONFIG_*` variable
  and rebuilds the configuration tables. A package that sets its own
  values in `.onLoad()` needs this. `cs9` is a dependency, loads first,
  and reads the environment before that package runs. The reload is
  state-safe. It disconnects every table it replaces, so a repeated
  reload does not leak connections. It also empties `config$tables`
  before it rebuilds them. A reload against an invalid environment
  therefore leaves an empty table list, not tables describing the
  previous configuration.

### Bug Fixes

- `DESCRIPTION` requires `csdb (>= 2026.8.5)` and carries
  `Remotes: niphr/csdb`. The bare `csdb` it had before let a resolver
  satisfy the dependency with any version. CRAN and RSPM serve version
  2026.5.13, which predates csdb’s SQLite backend. Installed against
  that, `CS9_DBCONFIG_DRIVER=SQLite` matched no branch in csdb and fell
  through to the generic ODBC arm. `$connect()` then failed with
  `Can't open lib 'SQLite' : file not found`, which names neither csdb
  nor a version. csdb 2026.8.5 is on GitHub and not on CRAN, so the
  floor alone would leave the dependency unsatisfiable. The `Remotes`
  field is what makes it obtainable. `cs9` is not submitted to CRAN, so
  the field costs nothing.
- Two blocks in `tests/testthat/test-sqlite-config.R` now call
  `skip_if_not_installed("csdb", "2026.8.5")`. This is not redundant
  with the version floor. Nothing enforces an `Imports` version after
  installation. `cs9` reaches `csdb` through `csdb::` alone, so
  `NAMESPACE` holds no import directive. R therefore runs no version
  check at load time. Measured on 2026-08-05 against csdb 2026.5.13,
  `R CMD INSTALL` exits 0 and
  [`library(cs9)`](https://niphr.github.io/cs9/) succeeds. The guard is
  keyed on the version and on nothing else, so it cannot hide a failure
  that is not a version mismatch.
- `setup_database_tables()` now builds all four tables into a local list
  and assigns `config$tables` once, at the end. Assigning each table
  directly meant a failure in the third constructor left a
  partially-populated `config$tables` behind, indistinguishable from a
  complete one.
- Under SQLite, a dbconfig’s `id` is the database file path rather than
  `[db].[schema]`. A partitioned table’s per-partition name uses the
  `xxpxx` separator that PostgreSQL uses. `PARTITION` is a keyword in
  SQLite’s window-function grammar.

### Documentation

- The installation vignette starts on SQLite. A reader now installs
  `cs9`, writes six settings into `.Renviron`, validates them and then
  opens the database, all before the vignette mentions a server. The
  last step is deliberate.
  [`check_environment_setup()`](https://niphr.github.io/cs9/reference/check_environment_setup.md)
  only checks the variables. It takes a `$connect()` on a configuration
  table to create the SQLite file and the `config_log` table in it. The
  PostgreSQL-in-Docker guide follows below.
- The installation vignette no longer says CS9 requires PostgreSQL for
  full functionality, which stopped being true when the SQLite backend
  landed. It says PostgreSQL is the production backend and points at the
  SQLite section for the alternative.
- [`vignette("backends")`](https://niphr.github.io/cs9/articles/backends.md)
  — new. It puts the `CS9_DBCONFIG_*` environments for PostgreSQL and
  SQLite side by side. It has a table of what each backend does with
  every variable. It gives the tiers
  [`check_environment_setup()`](https://niphr.github.io/cs9/reference/check_environment_setup.md)
  validates in, and the `.onLoad()` pattern that sets the variables from
  another package and then calls
  [`reload_db_config()`](https://niphr.github.io/cs9/reference/reload_db_config.md).
  It carries no R-level detail:
  [`vignette("backends", package = "csdb")`](https://niphr.github.io/csdb/articles/backends.html)
  is the companion for that.
- Both vignettes now warn about two traps that cost nothing to avoid and
  are hard to diagnose. An empty `CS9_PATH` counts as a missing
  variable, so `CS9_PATH=` fails validation. And an unset
  `CS9_DBCONFIG_ROLE_CREATE_TABLE` reaches `csdb` as `""` rather than
  `NULL`. `""` is not the `"x"` no-role sentinel, so the PostgreSQL
  `create_table` can emit `SET ROLE ""`. Both PostgreSQL blocks now set
  the variable explicitly.
- The PostgreSQL `.Renviron` block in the installation vignette carried
  both of those traps, and had done so since before the SQLite work. It
  wrote `CS9_PATH=` with no value. The variable table below it called
  `CS9_PATH` “usually empty”. `CS9_DBCONFIG_ROLE_CREATE_TABLE` was
  absent. A reader who copied the block got
  `Missing required environment variables: CS9_PATH`. All three are
  fixed.
- `README.md` names both backends and links to the two vignettes.
- `_pkgdown.yml` indexes all five vignettes. `cs9` and `backends` were
  missing from the `articles:` list; `cs9` had been missing
  independently of this work, although the navbar links to it.

### Development

- `DBI` and `RSQLite` added to `Suggests`, for the new
  `tests/testthat/test-sqlite-config.R`.

## Version 26.8.4

### Documentation

- Updated introduction vignette with single-instance design principle,
  NorSySS case study, and comparative analysis table
- Updated task creation vignette with single-instance framing
- Added Apache Airflow integration guidance to installation vignette
- Updated CLAUDE.md with surveillance-domain examples from academic
  paper (White & Valcarcel Salamanca, NIPH)
- Introduction vignette now states when CS9 is **not** the right choice.
  It names the hard requirements (PostgreSQL, containers, systems
  administration capability) and the workflow reorganisation adoption
  costs. It names `plnr` as the simpler option for a pilot or a
  resource-constrained setting.
- Introduction vignette now states that CS9 analyses each stratum
  independently **by default**. It also states that borrowing strength
  across strata is a decision about the statistical method, not a
  property of the framework. Adjusting exceedance probabilities for
  multiple comparisons is the same kind of decision. With 106 diseases
  across 378 locations the number of simultaneous tests is large.
  Nothing in CS9 controls the family-wise error rate or the false
  discovery rate for you.
- Introduction vignette now lists what the framework handles:
  per-analysis structured logging, schema validation and time-period
  partitioning, framework-level parallelism, and validation workflows

### Development

- Documentation is generated by roxygen2 8.0.0. `DESCRIPTION` now
  declares `Config/roxygen2/version` in place of `RoxygenNote`, and
  every `.Rd` file was regenerated by that version. `NAMESPACE` is
  unchanged.

## Version 26.5.13

### New Features

- `TaskJob` R6 class and
  [`run_task_sequentially_as_callr_bg_using_load_all()`](https://niphr.github.io/cs9/reference/run_task_sequentially_as_callr_bg_using_load_all.md)
  wrapper. A drop-in alternative to
  [`run_task_sequentially_as_rstudio_job_using_load_all()`](https://niphr.github.io/cs9/reference/run_task_sequentially_as_rstudio_job_using_load_all.md)
  that works in editors without RStudio’s job API (notably Positron,
  which does not implement `runScriptJob`). Spawns the task in a fresh
  [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html)
  process, so the current environment is not polluted. Captures output
  via a pipe. Streams the output back to the calling R console (prefixed
  with the task name) via
  [`later::later()`](https://later.r-lib.org/reference/later.html)
  polling. Includes `$start()`, `$wait()`, `$is_alive()`, `$status()`,
  `$tail()`, `$kill()`.

## Version 25.8.21

### Documentation

- Organized pkgdown reference documentation with logical function
  groupings
- Enhanced reference structure with clear categories for different types
  of functions
- Improved pkgdown configuration for better navigation of package
  documentation

## Version 25.7.31

### New Features

- Enhanced environment variable validation with detailed diagnostic
  function
  [`check_environment_setup()`](https://niphr.github.io/cs9/reference/check_environment_setup.md)
- Improved graceful degradation for CRAN compatibility - package loads
  with limited functionality when database infrastructure is not
  available
- Context-aware environment variable validation with clear user guidance
- Robust error handling in package loading process

### Improvements

- Updated system environment configuration handling
- Comprehensive startup messages guide users through configuration
  issues
- Enhanced database connection error handling
- Improved package loading sequence with better error isolation

### Documentation

- Comprehensive installation vignette explaining infrastructure
  requirements
- Enhanced function documentation with CRAN-compatible examples
- Clear guidance on functionality available in different deployment
  scenarios
- Added comprehensive package-level documentation (`?cs9`)
- Enhanced
  [`check_environment_setup()`](https://niphr.github.io/cs9/reference/check_environment_setup.md)
  documentation with detailed examples
- Updated vignettes with CRAN vs. full setup guidance

### CRAN Preparation

- Removed fhiplot dependency (replaced with standard R functions)
- Fixed non-portable file names in vignettes directory
- Cleaned up build artifacts and hidden files
- Updated LICENSE file copyright year to 2025
- Created vignette precompiler system for maintainable documentation
- Verified graceful degradation in minimal environments

### Bug Fixes

- Fixed package loading issues in environments without database
  configuration
- Improved error messages for missing environment variables
- Enhanced database table setup error handling

## Version 2025.3.6

- Automatically logging when tasks start running.

## Version 2025.2.24

#### New Features

- Added
  [`get_config_log()`](https://niphr.github.io/cs9/reference/get_config_log.md)
  function to retrieve configuration log entries from the `config_log`
  table.
  - Supports optional filtering by surveillance system (`ss`), task name
    (`task`), and date range (`start_date`, `end_date`).
  - Returns a `data.table` with the filtered entries.

#### Improvements

- Updated
  [`update_config_log()`](https://niphr.github.io/cs9/reference/update_config_log.md)
  to also route custom messages (`...`) to the
  [`message()`](https://rdrr.io/r/base/message.html) function for
  clearer console output.

## Version 2025.2.21

- **Added `update_config_log` function**. Logs configuration updates
  including surveillance system (`ss`), task name (`task`), and a custom
  `message`.

## Version 2024.6.17

- Allows for `CS9_DBCONFIG_ROLE_CREATE_TABLE` in environmental
  variables.

## Version 2024.6.17

- When running in parallel, a seed is set according to the index of the
  first analysis in each plan.

## Version 2024.6.6

- Partition table names are now ‘xxxpartitionxxx’ not ‘PARTITION’

## Version 2024.3.7

- Including confirm_insert_via_nrow in DBtables. Checks nrow() before
  insert and after insert. If nrow() has not increased sufficiently,
  then attempt an upsert.

## Version 2023.8.1

- cs9::path now uses \_interactive instead of interactive.

## Version 2023.5.3

- In R 4.3.0 `as.character(lubridate::now())` adds microseconds, which
  breaks the SQL upload. This is now replaced by
  [`cstime::now_c()`](https://rdrr.io/pkg/cstime/man/now_c.html).

## Version 2023.4.14

- Changed “success” to “succeeded” in `update_config_tasks_stats`

## Version 2023.4.13

- `DBPartitionedTableExtended_v9$info()` bug fixed with argument
  `collapse=TRUE`.
- Inclusion of `confirm_indexes` in `DBPartitionedTableExtended_v9`.

## Version 2023.4.12

- `DBPartitionedTableExtended_v9$nrow()` now has a new argument
  `collapse=FALSE` that provides partion-specific results
- `DBPartitionedTableExtended_v9$info()` now includes sizes in MB

## Version 2023.4.3

- Bug fix in `DBPartitionedTableExtended_v9$nrow()`

## Version 2023.4.2

- Extension of `DBPartitionedTableExtended_v9` so that it is easier to
  use multiple partitioning variables.

## Version 2023.4.1

- Inclusion of `partitions_randomized` in
  `DBPartitionedTableExtended_v9` so that when running in parallel, the
  database tables don’t get locked.
- Inclusion of `remove_table` in `DBPartitionedTableExtended_v9`
- Fixed an error in RAM calculation in parallel for
  `get_config_tasks_stats`

## Version 2023.3.31

- `DBTableExtended_v9` now automatically includes a column for all
  tables, called `auto_last_updated_datetime`, which is automatically
  calculated each time that row is changed.
- Creation of `DBPartitionedTableExtended_v9`, which allows for one
  dataset to be partitioned amongst multiple SQL tables automatically.

## Version 2023.3.8

- `SurveillanceSystem_v9` constructor now takes in an argument called
  `implementation_version`, which can be used to identify what version
  of analytics code is currently being run.
- `update_config_last_updated` has now been replaced by
  `update_config_tables_last_updated` (which contains when the tables
  were last updated) and `config_tasks_stats` (which contains all the
  runtimes of the tasks).
- `SurveillanceSystem_v9` now uses an internal R6 class
  `DBTableExtended_v9` (which extends
  [`csdb::DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.html))
  instead of using
  [`csdb::DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.html)
  directly. `DBTableExtended_v9` calls `update_config_last_updated`
  after altering a database table.

## Version 2023.3.7

- sc8 is deprecated in favor of cs9.

## Version 8.0.2

- Allows for multiple databases to be used for different access levels.
- `copy_into_new_table_where` now also copies indexes.
- V8 schemas now have a nice print function.
- V8 redirects now have a nice print function.
- `copy_into_new_table_where` uses tablock.
- `upsert_at_end_of_each_plan` and `insert_at_end_of_each_plan` can now
  take named lists as the return value from the `action_fn`.
- Custom progressr handler.

## Version 8.0.1

- When using `sc8::add_task_from_config_v8` the schema list is checked
  to make sure they are actually schemas. This will solve the issue
  where people incorrectly add non-existent schemas to the task.
- `insert_data`, `upsert_data`, `drop_all_rows_and_then_insert_data` are
  now the recommended ways of inserting data
- `addin_load_production`
- schemas now use `load_folder_fn`, which should dynamically check if a
  user has permission to write to a folder, solving permissions errors
- Including `tm_get_schema_names`
- Both `granularity_time` AND `granularity_geo` are now included in db
  censors
- Requires R \>= 4.1.0
- `sc8::config$plan_attempt_index` now exists. When running plans in
  parallel, if a plan fails it is retried five times. This lets a user
  track which attempt they are on. This is mostly useful so that emails
  and smses are only sent when `sc8::config$plan_attempt_index==1`
- (Disabled) TABLOCK is disabled right now due to issues where data
  would not be uploaded.
- (Disabled) Data is sorted before sending it to bcp to speed up
  in/upserts.

## Version 8.0.0

- Release of schema redirects that allow for restricted and anonymous
  datasets to be seamlessly used by people with different access rights
- Consistent naming of `task_from_config_v8` and `add_schema_v8`

## Version 7.1.4

- `db_insert_data`, `db_upsert_data`,
  `db_drop_all_rows_and_then_upsert_data` are now the recommended ways
  of inserting data

## Version 7.1.3

- `update_config_datetime` and `get_config_datetime` now automatically
  record database table updates as well

## Version 7.1.2

- Updating default db schemas to be more explicit with the useage of
  isotime.

## Version 7.1.1

- `qsenc_save` and `qsenc_read` to save/read to/from encrypted files.

## Version 7.1.0

- `task_from_config_v3` sets a new direction for creation of tasks and
  management of tasks
- `describe_tasks` and `describe_schemas` help with automatic
  documentation

## Version 7.0.8

- `task_inline_v1` allows for easy inline task creation
- Corresponding RStudio addin for inline tasks that copy from one db
  table to another

## Version 7.0.7

- `copy_into_new_table_where` allows for the creation of a new table
  from an old table
- Including `task_from_config_v2`
- First RStudio addin

## Version 7.0.6

- `write_data_infile` now checks for Infinite/NaN values and sets them
  to NA

## Version 7.0.5

- `Task` now includes `action_before_fn` and `action_after_fn`

## Version 7.0.4

- `validator_field_contents_sykdomspulsen` now allows `baregion` as a
  valid `granularity_geo`

## Version 7.0.3

- `tm_get_plans_argsets_as_dt` provides an overview of the plans and
  argsets within a task

## Version 7.0.2

- `keep_rows_where` now also retains the PK constraints
