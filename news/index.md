# Changelog

## Version 26.8.15

### Bug Fixes

- `DESCRIPTION` requires `csdb (>= 2026.8.15)`. That is the release
  where `DBConnection_v9` refuses a connection another process opened.
  `Task$run_parallel_plans()` forks with
  [`pbmcapply::pbmclapply`](https://rdrr.io/pkg/pbmcapply/man/pbmclapply.html)
  and passes the table objects into the workers, and every partition of
  a partitioned table now shares one connection. A forked child that
  used the parent’s connection received wrong results and no error.
- `csdb 2026.8.14` carries the shared connection without that guard, so
  this floor names `2026.8.15` rather than the release it would
  otherwise pair with.

### Development

- No `cs9` source file changed in this release. The version moves so the
  raised floor reaches a distinct tree, because `26.8.14` is already
  published.
- `Task$run()` closes every connection in `run_sequential()` before it
  forks, so production never reached the corrupt state. Nothing asserts
  that ordering. `csdb 2026.8.15` removes the dependency on it.

## Version 26.8.14

### Bug Fixes

- `DBPartitionedTableExtended_v9` now opens one database connection for
  the whole table, not one per partition. It builds a single
  [`csdb::DBConnection_v9`](https://niphr.github.io/csdb/reference/DBConnection_v9.html)
  before the partition loop and passes it to every `DBTableExtended_v9`.
  A table with 106 partitions needed 106 simultaneous connections before
  this change. That exceeded the NorSySS PostgreSQL limit of 100
  connections on 2026-08-13. The import died with
  `FATAL: remaining connection slots are reserved for roles with the SUPERUSER attribute`.
  The failure did not depend on the run size, because the partition
  count exceeds the budget before the first row is inserted.
- `DBPartitionedTableExtended_v9$disconnect()` now closes the shared
  connection once, at the parent. It looped over the partitions and
  closed each child before. Each child borrows the connection, so a
  child’s `disconnect()` is a no-op in `csdb` 2026.8.14.
- `DBTableExtended_v9$initialize()` gains `dbconnection` as its eighth
  and last argument. It forwards that argument to
  [`csdb::DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.html)
  in the eighth position. The argument defaults to `NULL`, which keeps
  the previous behaviour: the object builds and owns its own connection.
  `SurveillanceSystem_v9$add_table()` names every argument it passes and
  passes no `dbconnection`, so it is unaffected.
- `DESCRIPTION` requires `csdb (>= 2026.8.14)`. That is the version
  where `DBTable_v9$initialize()` accepts `dbconnection`. Without this
  version floor, the incompatibility would fail at run time rather than
  at install time.
- `DBPartitionedTableExtended_v9$drop_all_rows_and_then_upsert_data()`
  now sends each row of `newdata` to the partition that its partition
  column names. It read `self[[self$column_name_partition]]` before.
  `self` is the R6 object and holds no field with that name, so `[[`
  returned `NULL`. `NULL == "a"` returns `logical(0)`, and a data.table
  indexed by `logical(0)` holds zero rows. The method therefore ran
  `drop_all_rows()` on every partition and then upserted nothing. The
  whole table lost every row, with no error and no warning.
  `drop_all_rows_and_then_insert_data()` reads `newdata` and is
  unchanged.
- `DBPartitionedTableExtended_v9` now rejects a `newdata` that it cannot
  route, before any method destroys a row. This is the third defect of
  this shape in the class in one week, and the routing fix above does
  not cover it. `private$check_for_correct_partitions_in_data()` read
  `newdata[[self$column_name_partition]]`, and `[[` returns `NULL` for a
  column that `newdata` does not carry. `unique(NULL)` is `NULL`,
  `!NULL %in% x` is `logical(0)`, and `sum(logical(0))` is 0, so
  validation passed. Both `drop_all_rows_and_then_*` methods then
  computed `NULL == i`, which is `logical(0)`. Each took zero rows for
  every partition, dropped all of them and wrote nothing back. The whole
  table lost every row, with no error and no warning. The check now
  rejects four things, in this order.
  - A `newdata` that is not a `data.frame`. It names the class it
    received.
  - A `newdata` with no partition column. It names the column it
    expected.
  - An `NA` in the partition column.
  - A value that no partition covers. It names those values.
- A zero-row `newdata` that carries the partition column is still
  accepted. Clearing every partition is a legitimate operation, and
  `tests/testthat/test-partition-safety.R` pins it. Only a `NULL`, or a
  table without the column, is rejected.
- `DBPartitionedTableExtended_v9` now writes a generated partition
  column with a valid `:=` call, and it writes into a copy. Four methods
  held `newdata[, .(self$column_name_partition) := part]`. That form
  raises `LHS of := must be a symbol, or an atomic vector`. Every table
  with a non-`NULL` `value_generator_partition` therefore failed on its
  first write. No test built one, which is why the defect survived. The
  form is now `newdata[, (self$column_name_partition) := part]`. `:=`
  also writes by reference. The old form therefore added a column to the
  caller’s own data.table, as a side effect of a write.
  [`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html)
  now runs first. The copy is inside the `value_generator_partition`
  branch only, and not on the common path. A NorSySS production import
  is memory-bound against about 20 GiB of usable pod memory, so a copy
  of every `newdata` would double the peak.
- `DBPartitionedTableExtended_v9` now handles a numeric partition value,
  which two separate defects broke. First, `sample(x, length(x))` reads
  a length-1 numeric `x` as the range `1:x`. A single partition numbered
  9 therefore became a draw from 1 to 9. Both call sites, in
  `insert_data()` and in the `partitions_randomized` active binding, now
  index by a permutation: `x[sample.int(length(x))]`. That form has no
  such special case and is a no-op below length 2. Second, `[[` on a
  list takes a numeric index as a position, so `self$tables[[5L]]`
  returned the fifth child rather than the child named “5”. In the
  constructor it also grew the list to five elements, three of them
  `NULL`. All 16 `self$tables[[...]]` sites in
  `R/r6_DBPartitionedTableExtended_v9.R` now index by
  [`as.character()`](https://rdrr.io/r/base/character.html). A character
  partition value was never affected by either defect.
- `DBPartitionedTableExtended_v9` now declares `indexes` as a public
  field, and `initialize()` assigns it. The class took `indexes` as a
  constructor argument and gave it to every child, but it held no field
  of that name. `self$indexes` was therefore `NULL`, and the
  `drop_indexes = names(self$indexes)` default in `upsert_data()` and in
  `drop_all_rows_and_then_upsert_data()` evaluated to `NULL`. Each child
  received an explicit `NULL`, which overrode the child’s own
  `names(child$indexes)` default. Both defaults are now a plain `NULL`,
  so the runtime behaviour is identical and is now explicit. Nothing
  drops an index by default. Whether an index rebuild beats row-by-row
  index maintenance depends on four things: rows changed per child,
  index count and width, table size, and concurrency. A `DROP INDEX`
  also briefly blocks reads and writes. That needs measurement against
  real batch sizes. A caller who wants the drop now passes
  `drop_indexes = names(pt$indexes)`.
- `DBPartitionedTableExtended_v9$upsert_data()` computed
  `partitions_in_use` twice from the same expression. One of the two
  lines is removed. Nothing observable changes. The method still writes
  to the partitions in the order `newdata` names them. It is not
  randomised, because no transaction spans the partition loop, so each
  child releases its locks before the next runs.
- `DBPartitionedTableExtended_v9` now computes one route vector, and all
  four write methods route with it.
  `private$check_for_correct_partitions_in_data()` returns
  [`as.character()`](https://rdrr.io/r/base/character.html) of the
  partition column. Each write method captures that vector and indexes
  with `route == as.character(i)`, beside
  `self$tables[[as.character(i)]]`. One coercion now names the child and
  selects its rows. Validation used
  [`setdiff()`](https://rdrr.io/r/base/sets.html) on the raw values
  before, while the write methods used `==`. Those were two independent
  answers to one question, and they agreed. An adversarial review said a
  factor column made them diverge and wiped every partition. Measured
  against the real class on 2026-08-14, it does not.
  [`setdiff()`](https://rdrr.io/r/base/sets.html) converts a factor to
  character, and `==` against a factor compares its labels, so both
  sides already read the same thing. The class is refactored so that the
  question has one answer, not because the two answers differed.
- `DBPartitionedTableExtended_v9` now rejects a partition column that is
  not atomic. A list column passed the old check, because
  [`setdiff()`](https://rdrr.io/r/base/sets.html) also converts a list
  to character. `setdiff(list("a"), c("a", "b", "c"))` is therefore
  empty. `drop_all_rows_and_then_upsert_data()` then dropped every
  partition and died inside csdb on `is.infinite(get(i))`, which has no
  list method. A three-partition table holding six rows lost all six,
  and nothing failed until after the last drop. That is the fourth
  silent-data-loss defect of this shape in the class in one week.
  [`is.atomic()`](https://rdrr.io/r/base/is.recursive.html) now rejects
  the column before the first drop. A factor is atomic, so a factor
  column still routes.
- `DBPartitionedTableExtended_v9$initialize()` now rejects a partition
  set that the class cannot use. It ran no check on
  `table_name_partitions` at all before. The check runs before the class
  opens its connection, so a rejected construction leaves no database
  connection open. It rejects four things.
  - A zero-length set. The table then carried no children, and every
    write was a silent no-op.
  - An `NA`.
  - An empty string after
    [`as.character()`](https://rdrr.io/r/base/character.html).
    `self$tables[[""]] <- child` matches no name, so it APPENDS. A set
    of `c("a", "")` built three children for two partitions.
  - A duplicate after
    [`as.character()`](https://rdrr.io/r/base/character.html).
    `self$tables[[name]]` takes the first match, so a duplicated
    partition made one child unreachable while `length(self$partitions)`
    still counted it.
- The guard now compares the character form of a partition value, not
  the raw value. `names(self$tables)` and all 16 `self$tables[[...]]`
  sites already used
  [`as.character()`](https://rdrr.io/r/base/character.html), so the
  character form was already the identity of a partition. One accepted
  input changes as a result. A `newdata` value of `1.0000000000000002`
  against a partition of `1` was rejected before, because
  [`setdiff()`](https://rdrr.io/r/base/sets.html) compares two doubles
  exactly. It now routes to the child named “1”. Both values print as
  `1`, and the partition column is stored as `TEXT`. A caller MUST NOT
  give one table two partition values that
  [`as.character()`](https://rdrr.io/r/base/character.html) renders
  identically. `initialize()` now rejects that set.

### Known limitation

- A child that a caller keeps after
  `DBPartitionedTableExtended_v9$disconnect()` can reopen the shared
  connection. `csdb::DBConnection_v9$autoconnection` calls `connect()`
  on every access, so any later use of that child opens a new
  connection. The child cannot close that connection again, because it
  does not own it. Only the parent closes it. Callers MUST NOT use a
  child after they disconnect the parent. A caller that does MUST
  disconnect the parent again. `r6_Task.R` calls `disconnect()` on the
  objects in the task’s table list. A partitioned table is registered
  there as the parent, so the production teardown path is unaffected.

### Licensing

- `DESCRIPTION` no longer carries explicit `Author` and `Maintainer`
  fields. R derives both from `Authors@R`, which is the single source.
  The explicit fields named “Core Surveillance” as the copyright holder,
  while `Authors@R` names Folkehelseinstituttet. The 2026-08-06 sweep
  corrected `Authors@R` and left the free text behind, so the two
  disagreed on the legal entity.

### Development

- `tests/testthat/test-shared-connection.R` is new. It builds a
  three-partition table against a temporary SQLite file, and it asserts
  five things.
  - The three children hold one connection object, counted by
    [`data.table::address()`](https://rdrr.io/pkg/data.table/man/address.html).
  - A write reaches the partition that its `part` column names.
  - `disconnect()` on the parent closes the shared connection exactly
    once. The test counts the closes.
  - A `disconnect()` loop over every child leaves the partitioned table
    usable.
  - A child kept past the parent’s `disconnect()` reopens the shared
    connection, and only the parent closes it again.
- `tests/testthat/test-partition-routing.R` is new. It builds a
  three-partition table against a temporary SQLite file, and it asserts
  three things. The file holds 35 assertions, and 29 of them fail when
  the routing defect is present.
  - `drop_all_rows_and_then_upsert_data()` sends every row to the
    partition that its `part` column names. The three partitions receive
    2, 1 and 3 rows, so a row that reaches the wrong partition changes a
    count.
  - `drop_all_rows_and_then_upsert_data()` and
    `drop_all_rows_and_then_insert_data()` write the same rows to the
    same partitions.
  - `drop_all_rows_and_then_upsert_data()` empties a partition that
    `newdata` does not name.
- `tests/testthat/test-partition-safety.R` is new. It holds 21 tests and
  178 assertions against a temporary SQLite file. The duplicated
  `partitions_in_use` line has no test, because its removal changes
  nothing that a test can observe. The whole cs9 suite now reports 298
  passing assertions, up from 256.
  - The first test is the one that matters. It writes two sentinel rows
    into each of three partitions, calls
    `drop_all_rows_and_then_upsert_data()` with a `newdata` that lacks
    the partition column, and then re-reads every partition. Before the
    fix it reported no error and zero rows in all three partitions. An
    error alone would not prove that the rows survived.
  - A zero-row `newdata` that carries the partition column still clears
    every partition.
  - A table with a `value_generator_partition` routes every row, and all
    four write methods leave the caller’s data.table unchanged.
  - A single numeric partition numbered 9 gives
    `partitions_randomized == 9`. Under seed 5 the old
    [`sample()`](https://rdrr.io/r/base/sample.html) returned 2.
  - A table with partitions `c(5L, 7L)` holds exactly two children,
    named “5” and “7”, and a row routes to the child its value names.
  - A `newdata` that names exactly one numeric partition reaches that
    partition. This covers the second
    [`sample()`](https://rdrr.io/r/base/sample.html) call site, which is
    inside `insert_data()`.
  - `pt$indexes` holds the list the constructor received.
  - A factor partition column against numeric partitions routes every
    row, and every row survives. This is the case the adversarial review
    named. The test pins the measured behaviour, so a later change
    cannot regress it in silence.
  - A list partition column is rejected, and all six sentinel rows
    survive.
  - The constructor rejects each of the four unusable partition sets.
    Each error message names the problem.
  - `check_for_correct_partitions_in_data()` returns a character vector
    whose length is `nrow(newdata)`. The test reaches private through
    `pt$.__enclos_env__$private`, and it is the only test that does.

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
