# AGENTS.md

## Before you change a task

Each rule links to the section of `vignette("how-a-task-runs")` whose chunk demonstrates it. `vignette("task-shapes")` lists the task shapes that follow these rules.

- A plan is one data pull: cs9 calls the data selector once per plan. [Demonstration](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-one-pull).
- Analyses iterate within a plan, and every analysis of a plan reads the data of that one pull. [Demonstration](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-analyses).
- Only the first and the last plan run in a fixed order. The middle plans run in parallel, in any order, under three conditions: `cores` above 1, at least 4 plans, and a non-interactive session. Otherwise every plan runs in sequence, in plan order. A middle plan MUST NOT assume any order relative to other middle plans, because a production run MAY run them in parallel. It MUST NOT decide that the work of other middle plans is complete, so put that step in the last plan. [Demonstration](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-first-last).
- A forked worker MUST NOT inherit an open database connection, because an inherited PostgreSQL connection returned wrong results without an error. Keep every table that a task uses in its `tables` list, so the sweep before the fork closes it. [Demonstration](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-forked-workers).
- A completeness check covers every declared partition, `names(pt$tables)`, never only the partitions that hold rows. The `partition` column of `pt$nrow(collapse = FALSE)` lists only the partitions whose table exists. Never parse a partition tag from a table name. [Demonstration](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-partitions).
- `plnr::expand_list()` makes one argset per element of a vector. To give one argset several values, pass them as one text value. [Demonstration](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-expand-list).
- Size a plan by its data pull, not by the unit of work that you commit. Group the commits in the last plan. [Demonstration](https://niphr.github.io/cs9/articles/how-a-task-runs.html#rule-plan-size).

## Overview

CS9 (Core Surveillance 9) is an R framework for database-driven disease surveillance. It follows the **single-instance design principle**. The epidemiologist writes the analysis for one scenario, for example influenza in one location. CS9 runs that code across every disease, location and demographic group.

For simpler needs, with no database and one computer, the [plnr](https://CRAN.R-project.org/package=plnr) package gives the same single-instance design without the CS9 infrastructure.

Reference: White RA, Valcarcel Salamanca B. "CS9: An analysis framework for real-time disease surveillance." Norwegian Institute of Public Health.

## Development commands

```r
devtools::load_all(".")   # load the package for development
devtools::document()      # regenerate man/ and NAMESPACE
devtools::test()          # run tests/testthat; no test needs a database server
```

```bash
R CMD build .
R CMD check --as-cran --no-manual cs9_*.tar.gz   # the flags that CI uses
```

`vignettes/_PRECOMPILER.R` knits `cs9.Rmd`, `creating-a-task.Rmd` and `file-layout.Rmd` from their `.Rmd.orig` files. Run it from the package root, and commit both files of each pair. The other vignettes run their chunks against SQLite when they build.

## Architecture

CS9 has three tiers. **Plans** define the iteration scope, for example the diseases, locations and age groups. **Data selectors** pull the data: cs9 calls the data selector once per plan, not once per analysis. A data selector MAY run several queries, or none. **Action functions** hold the epidemiological logic for one instance.

**SurveillanceSystem_v9** (`R/r6_SurveillanceSystem.R`) holds the tables, the partitioned tables and the tasks, and provides the `shortcut_*()` methods.

**Task** (`R/r6_Task.R`) holds the plans of one task and runs them, in sequence or in parallel.

**DBTableExtended_v9** (`R/r6_DBTableExtended_v9.R`) is a `csdb::DBTable_v9` that records each write in `config_tables_last_updated` and adds the column `auto_last_updated_datetime` to every table.

**DBPartitionedTableExtended_v9** (`R/r6_DBPartitionedTableExtended_v9.R`) is one table per partition. All partitions share one connection.

### Argsets

An argset is the named list that a data selector or an action function receives. cs9 joins three levels into it:

- `universal_argset`, the same for every argset of the task
- the element of `for_each_plan` for its plan
- the element of `for_each_analysis` for its analysis

cs9 then adds `index`, `today` and `yesterday` (`R/r6_SurveillanceSystem.R:477-543`).

Only the argset of an analysis gets `index_plan`, `index_analysis`, `first_analysis`, `last_analysis`, `within_plan_first_analysis` and `within_plan_last_analysis` (`R/r6_Task.R:70-99`). The argset of a data selector does not have them.

### Plans built at run time

A task with `plan_analysis_fn_name` builds its plans when it runs. cs9 calls that function as `fn(universal_argset, tables)` (`R/r6_SurveillanceSystem.R:464`). It MUST return `list(for_each_plan = , for_each_analysis = )`. `SurveillanceSystem_v9$run_task()` builds the plans again on every call (`R/r6_SurveillanceSystem.R:306-312`). `get_task()` and the `shortcut_*()` methods build them once and then reuse them.

### Schemas

csdb maps six field types to each database: `TEXT`, `INTEGER`, `DOUBLE`, `DATE`, `DATETIME` and `BOOLEAN`. `validator_field_types` checks the field types when csdb builds the table object. `validator_field_contents` checks the rows of each insert and upsert. The `name_access` of a table, for example `anon` or `restr`, selects its database configuration.

## Development workflow

Use `plnr::is_run_directly()` blocks to run the body of a task function interactively:

```r
if (plnr::is_run_directly()) {
  index_plan <- 1
  argset <- ss$shortcut_get_argset("task_name", index_plan = index_plan)
}
```

```r
ss$run_task("task_name")
ss$shortcut_get_plans_argsets_as_dt("task_name")
data <- ss$shortcut_get_data("task_name", index_plan = 1)
argset <- ss$shortcut_get_argset("task_name", index_plan = 1, index_analysis = 1)
```

## Task implementation

- Do not catch an error in an action only to print it, or to return `NULL`. `Task$run()` then records the task as succeeded (`R/r6_Task.R:128` and `:275`).
- A middle plan that fails in a parallel run runs again, up to 5 times with 5 seconds between tries (`R/r6_Task.R:344-404`). The data selector and the action then run again, so a middle plan MUST be safe to run twice.
- `mandatory_db_filter()` filters on the standard columns, for example `granularity_geo`, `location_code`, `age` and `sex`. Each argument defaults to `NULL`, which applies no filter.
- `R/addins.R` and `inst/rstudio/addins.dcf` provide RStudio addins that write the boilerplate of a task.
- `update_config_log()` and `get_config_log()` write and read the task log. `get_config_tasks_stats()` reads the run time and status of each run.

## File structure

```
R/
├── r6_SurveillanceSystem.R          # SurveillanceSystem_v9, add_task(), the shortcuts
├── r6_Task.R                        # Task: run(), the sequential and parallel branches
├── r6_DBTableExtended_v9.R          # DBTableExtended_v9
├── r6_DBPartitionedTableExtended_v9.R
├── config_*.R                       # the four configuration tables
├── addins.R                         # RStudio addins
├── util_*.R                         # utilities
└── zzz_imports.R                    # imports that R CMD check cannot see

vignettes/
├── *.Rmd.orig                       # knitted into *.Rmd by _PRECOMPILER.R
├── how-a-task-runs.Rmd              # the execution rules, run on SQLite
├── task-shapes.Rmd                  # task shapes, run on SQLite
└── more-task-shapes.Rmd             # task shapes, run on SQLite

tests/testthat/                      # no test needs a database server
```

`vignette("file-layout")` describes the layout of a package that uses CS9.

## `R CMD check` cannot see a `::` call inside an R6 method

`R6::R6Class()` takes its methods as `public = list(...)`. The dependency scan of `R CMD check` walks top-level function definitions, and it does not walk that list. A `pkg::fn()` call inside an R6 method is therefore invisible to it, in both directions:

- An **undeclared** package raises no NOTE.
- A **declared** package used only inside R6 methods lands in `Namespaces in Imports field not imported from`.

`R/zzz_imports.R` names each package that only R6 methods call, once, in a plain function that nothing calls. It also names `progress`, which `progressr::handler_progress()` calls on behalf of `.onLoad()`. Add a new R6-only import there. Do not add a package there to silence the NOTE: remove a package that nothing uses from `DESCRIPTION` instead.

Compare every `::` call in `R/` against `DESCRIPTION` by hand:

```bash
grep -rhoE "\b[a-zA-Z][a-zA-Z0-9._]*::" R/ --include='*.R' | sed 's/:://' | sort -u > /tmp/used.txt
sed -n '/^Depends:/,/^License:/p' DESCRIPTION | grep -oE "^ +[a-zA-Z][a-zA-Z0-9._]*" \
  | tr -d ' ' | sort -u > /tmp/decl.txt
comm -23 /tmp/used.txt /tmp/decl.txt
```

The output also reports a match inside a comment or a string. Four matches are not calls:

| Match | What it is |
|---|---|
| `PACKAGE` | the template text `"PACKAGE::TASK_NAME_action"` in `R/addins.R` |
| `future.apply` | a comment in `R/r6_Task.R` |
| `pkg` | a comment in `R/zzz_imports.R` |
| `cs9` | the package itself |

Ask what runs the text, not only who wrote it. `devtools::load_all('.')` in `R/r6_SurveillanceSystem.R` and `R/r6_TaskJob.R` is text that cs9 writes into a script and then runs in a child R process. That feature needs `devtools` at run time, so `devtools` is in `Suggests`.

`utils` ships with R and is always installed. Declare it anyway, because cs9 calls `utils::packageDescription()` and `utils::tail()`.

## Licensing

This package is `MIT + file LICENSE`. Two files carry the licence and they MUST agree with each other:

- `LICENSE` holds exactly two lines, `YEAR:` and `COPYRIGHT HOLDER:`. CRAN requires that shape for `MIT + file LICENSE`. Do not put the licence text there.
- `DESCRIPTION` `Authors@R` MUST name the same holder, with `role = "cph"`.

The copyright holder for this package is **Folkehelseinstituttet**.

**Check the year at the start of each calendar year, and whenever you edit `DESCRIPTION`.** Nothing in `R CMD check` tests the copyright year.

Check both in one step:

```r
readLines("LICENSE")
a <- unclass(eval(parse(text = read.dcf("DESCRIPTION")[1, "Authors@R"])))
Filter(function(p) "cph" %in% p$role, a)
```
