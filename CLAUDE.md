# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

CS9 (Core Surveillance 9) is a domain-specific analysis framework in R that provides the analytical infrastructure layer for disease surveillance systems. Its core innovation is the **single-instance design principle**: epidemiologists write analytical code for a single scenario (e.g. influenza trends in one location), while CS9 automatically scales that code across all diseases, locations, and demographic groups — handling parallelization, database operations, and production reliability transparently.

This resolves a longstanding tension in surveillance: epidemiologists naturally think in terms of specific scenarios, but traditional implementations bury analytical logic within nested loop structures that manage scaling. CS9 separates the "what to analyze" (action functions) from the "where to apply it" (plans), so epidemiological reasoning stays readable and testable.

CS9 has powered the Norwegian Syndromic Surveillance System (NorSySS) for 10 years (2014-2024), processing 18 million consultations annually across 106 diseases and 378 locations with 99.5% availability. During the COVID-19 pandemic, the team added new diagnostic codes and began producing daily municipal-level outputs within weeks — without disrupting existing surveillance.

For simpler needs (no database, single computer), the [plnr](https://CRAN.R-project.org/package=plnr) package provides the same single-instance design principle without CS9's infrastructure layer.

Reference: White RA, Valcarcel Salamanca B. "CS9: An analysis framework for real-time disease surveillance." Norwegian Institute of Public Health.

## Development Commands

```bash
# Essential R package development workflow
devtools::load_all(".")              # Load package functions during development
devtools::document()                 # Generate documentation  
devtools::check()                    # Standard package check
R CMD check . --as-cran             # REQUIRED: CRAN compliance check
devtools::build()                   # Build package
devtools::install()                 # Install package

# Testing
testthat::test_dir("tests/testthat") # Run tests
```

## Architecture Overview

CS9's three-tier architecture implements the single-instance principle: **plans** define the iteration scope (which diseases, locations, age groups), **data selectors** provision data efficiently (one database query per plan rather than per analysis), and **action functions** contain the epidemiological logic operating on a single instance. The framework connects these layers to handle scaling, parallelization, and database management transparently.

### Core Framework Components

**SurveillanceSystem_v9**: The main R6 class that orchestrates the entire surveillance system
- Manages database tables, partitioned tables, and tasks
- Provides shortcuts for data access and task execution
- Located in `R/r6_SurveillanceSystem.R`

**Task**: R6 class representing individual surveillance tasks  
- Contains plans (data processing units) and analyses
- Supports parallel execution and database operations
- Located in `R/r6_Task.R`

**Database Integration**: Extended database table classes for surveillance data
- `DBTableExtended_v9`: Enhanced database table management
- `DBPartitionedTableExtended_v9`: Multi-table partitioning support
- Automatic logging of table updates and metadata tracking

### Task Development Pattern

CS9 follows a structured approach for implementing surveillance tasks:

1. **Schema Definition**: Define database table structure, field types, keys, and validation
2. **Task Configuration**: Specify task name, execution parameters, and schema mappings  
3. **Data Selector Function**: Extract and prepare data for analysis
4. **Action Function**: Core analysis logic that processes data and stores results

### Key Architectural Concepts

**Plans and Analyses**: Tasks are organized into plans (data processing units) containing analyses
- Plans can run sequentially or in parallel
- Each plan has a data selector that runs once per plan
- Each analysis within a plan runs the action function

**Argsets**: Named lists containing arguments passed to functions
- Universal argsets apply to all plans/analyses
- Plan-specific argsets vary by plan
- Analysis-specific argsets vary by analysis

**Schema System**: Comprehensive data validation and management
- Field type validation (TEXT, INTEGER, DOUBLE, DATE, DATETIME, BOOLEAN)
- Content validation for data integrity
- Access control through schema naming (anon, restr, etc.)

### Development Workflow Integration

**Interactive Development**: Use `plnr::is_run_directly()` blocks for interactive testing
```r
if(plnr::is_run_directly()){
  # Development code that only runs when manually executed
  # Allows treating functions as interactive scripts
  index_plan <- 1
  argset <- ss$shortcut_get_argset("task_name", index_plan = index_plan)
}
```

**Task Execution and Debugging**:
```r
# Run specific tasks
ss$run_task("task_name")

# Get task overview
ss$shortcut_get_plans_argsets_as_dt("task_name")

# Access data for debugging  
data <- ss$shortcut_get_data("task_name", index_plan = 1)
argset <- ss$shortcut_get_argset("task_name", index_plan = 1, index_analysis = 1)
```

## Development Best Practices

### Database Operations
- Use `keyby` in data.table aggregations to ensure proper database insertion
- Apply `cstidy::set_csfmt_rts_data_v1()` for structural data formatting
- Always validate data before database operations

### Task Implementation
- Place schema definitions in designated schema files
- Use RStudio addins for boilerplate code generation  
- Implement both data selector and action functions for each task
- Use `mandatory_db_filter()` for explicit data filtering

### Configuration and Logging
- Use `update_config_log()` for task execution logging
- Access configuration through `get_config_log()` for debugging
- Track performance metrics through built-in configuration tables

### Parallel Processing
- Set `cores` parameter in task configuration for parallel execution
- First and last plans always run sequentially for setup/cleanup
- Use `plnr::expand_list()` for plan structure definition

## File Structure

```
R/                              # Source code
├── r6_SurveillanceSystem.R    # Main surveillance system class
├── r6_Task.R                  # Task management class  
├── r6_DBTableExtended_v9.R    # Enhanced database tables
├── config_*.R                 # Configuration management
├── addins.R                   # RStudio addins for development
└── util_*.R                   # Utility functions

dev/                           # Development scripts
├── ss_example.R               # Example surveillance system setup
└── *.R                        # Other development utilities

vignettes/                     # Documentation
├── cs9.Rmd                    # Main package documentation
├── creating-a-task.Rmd.orig   # Task creation guide
└── file-layout.Rmd            # File organization guide

tests/testthat/                # Test suite
```

This framework enables systematic development of surveillance systems with robust data management, parallel processing capabilities, and comprehensive logging for epidemiological analysis.

## Implementation Lessons from Real-World Usage

### Project Structure Patterns

**Standard CS9 Implementation Layout**:
```
R/
├── 00_env_and_namespace.R     # Environment setup and exports
├── 01_definitions.R           # Project-specific definitions  
├── 02_surveillance_systems.R  # Initialize surveillance system
├── 03_tables.R               # Database table definitions
├── 04_tasks.R                # Task configuration
├── 05_deliverables.R         # Output/report configuration (optional)
├── 10_onLoad.R               # Package loading sequence
├── 11_onAttach.R             # Package attachment messages
└── [task_name].R             # Individual task implementations
```

**Critical `.onLoad()` Sequence**:
```r
.onLoad <- function(libname, pkgname) {
  # Authentication (if needed)
  if (file.exists("/bin/authenticate.sh")) {
    try(system2("/bin/authenticate.sh", stdout = NULL), TRUE)
  }
  
  # Initialize in correct order
  set_definitions()          # Global definitions first
  set_surveillance_systems() # Initialize ss object
  set_db_tables()           # Add tables to ss
  set_tasks()               # Add tasks to ss
  
  # Configure progress bars
  progressr::handlers(progressr::handler_progress(
    format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
    clear = FALSE
  ))
}
```

### Advanced Task Patterns

**Pipeline Tasks with Dependencies**:
Real implementations use complex multi-stage pipelines where tasks depend on outputs from previous tasks:

```r
# Pattern: Data processing pipeline
betting_upload_raw_data → anon_betting_raw_data
         ↓
betting_calculate_elos → anon_betting_runner_elos + anon_betting_jockey_elos
         ↓                                    ↓
betting_clean_basic_data → anon_betting_basic_clean_data
         ↓                                    ↓  
betting_merge_clean_data ← anon_betting_runner_elos + anon_betting_jockey_elos
         ↓
betting_summarize_data → validation reports + anon_betting_data_summary
```

**Dynamic Plan Generation with `plan_analysis_fn_name`**:
For complex scenarios where plans must be generated based on database state:

```r
# Task configuration using plan_analysis function
global$ss$add_task(
  name_grouping = "betting",
  name_action = "calculate", 
  name_variant = "elos",
  plan_analysis_fn_name = "horses::betting_calculate_elos_plan_analysis",
  for_each_plan = NULL,  # Generated dynamically
  for_each_analysis = NULL,  # Generated dynamically
  universal_argset = list(batch_size = 30000),
  # ... rest of configuration
)
```

**Historical Context Loading Pattern**:
Critical pattern for temporal calculations (ELO ratings, lag features):

1. **Plan Analysis**: Identify new data to process
2. **Data Selector**: 
   - Load batch data to process
   - Extract all unique participants from batch
   - Load complete historical context for those participants
3. **Action**: Use historical context for accurate calculations, insert only batch results

### Database Schema Best Practices

**Complex Field Types from Real Implementation**:
```r
field_types = c(
  # Standard identifiers
  "race_id" = "TEXT",
  "runner_id" = "TEXT", 
  "meeting_date" = "DATE",
  
  # Calculated features with proper types
  "runner_elo_before" = "DOUBLE",
  "forecast_price_decimal" = "DOUBLE", 
  "is_win" = "INTEGER",             # Boolean as INTEGER
  "data_split" = "TEXT",            # train/validation/holdout
  
  # Lag features (systematic naming)
  "lag1_finish_position" = "INTEGER",
  "lag2_finish_position" = "INTEGER",
  "lag3_finish_position" = "INTEGER",
  "lag4_finish_position" = "INTEGER",
  
  # Race-relative features
  "runner_elo_vs_race_avg" = "DOUBLE",
  "odds_rank_in_race" = "INTEGER"
)
```

**Multi-Purpose Database Indexes**:
```r
indexes = list(
  "ind1" = c("race_id", "runner_id"),      # Primary lookup
  "ind2" = c("meeting_date"),              # Temporal queries
  "ind3" = c("runner_id", "meeting_date"), # Historical lookups
  "ind4" = c("calculation_date")           # Processing tracking
)
```

### Robust Data Validation Patterns

**Multi-Part Analysis Structure**:
Real implementations use systematic 4-part validation:

```r
# Part 1: Processing Progress by Year
year_progress <- raw_data[, .(raw_records = .N), by = .(year = year(meeting_date), data_split)]

# Part 2: Data Validation (Same Time Ranges)  
validation_metrics <- merge(raw_metrics, clean_metrics, by = "data_split")

# Part 3: Variable Quality Analysis
missing_analysis <- clean_data[, lapply(.SD, function(x) sum(is.na(x))/.N*100)]

# Part 4: Final Dataset Characteristics
modeling_readiness <- clean_data[, .N, by = complete.cases(.SD)]
```

**Database Compliance Aggregation**:
```r
# CRITICAL: Always use keyby with required key fields
summary_results <- clean_data[, .(
  metric_value = mean(some_metric),
  metric_text = paste("Summary text")
), keyby = data_split]  # Ensures data_split key compliance

# Multi-key tables
summary_table <- data[, .(
  value = calculation
), keyby = .(summary_type, metric_name, data_split)]
```

### Production Deployment Considerations

**Docker Integration**:
Real implementations integrate with Docker-based infrastructure:
- Airflow for task scheduling (`0 2 * * *` daily schedules)
- Posit Workbench for development (port 8786)
- PostgreSQL databases for data storage

**Error Handling Patterns**:
```r
# Robust error handling in action functions
if(length(current_race_id) == 0 || is.na(current_race_id)) {
  return()  # Graceful exit for empty data
}

if(nrow(current_race) < 2) {
  return()  # Skip invalid races
}

# Use tryCatch for complex calculations
result <- tryCatch({
  complex_calculation()
}, error = function(e) {
  cat("Error in calculation:", e$message, "\n")
  return(NULL)
})
```

**Incremental Processing**:
Production systems process data incrementally:
- Use date-based filtering to avoid reprocessing
- Maintain calculation_date fields for tracking
- Implement proper upsert patterns for updates

### Documentation Standards for Production

**Comprehensive Task Documentation**:
```r
#' Task Name (action)
#'
#' Detailed description of what this task does in the pipeline context.
#'
#' @param data Named list containing input datasets
#' @param argset Named list containing analysis parameters  
#' @param tables Named list of database table connections
#' @return NULL (side effect: inserts data into database)
#' @details
#' Task's role in pipeline dependency chain:
#' \itemize{  
#'   \item Input: Expected table schemas and their purpose
#'   \item Processing: Key data transformations performed
#'   \item Output: Generated table schemas and contents
#'   \item Dependencies: Relationship to other pipeline stages
#' }
#' @export
```

This real-world usage demonstrates CS9's capabilities for complex data processing pipelines with robust validation, temporal calculations, and production deployment patterns.

## NorSySS: Real-World Surveillance Deployment

The Norwegian Syndromic Surveillance System (NorSySS) is the primary production deployment of CS9, operating continuously since 2014. These examples come from the academic paper (White & Valcarcel Salamanca, NIPH).

### NorSySS Pipeline Structure

```r
# NorSySS overnight processing pipeline (orchestrated by Apache Airflow)
import_data → cleaning_data → estimating_trends → nowcasting → producing_figures
     ↓              ↓                ↓                 ↓              ↓
  raw_data    ~1B rows output   short_term_trends   nowcast       106 portrait +
              ~1M time series   MEM thresholds      estimates     106 landscape
              106 diseases      weekly exceedance                 figures
              378 locations
              9 age groups
              3 sex groups
```

### Short-Term Trends Task Configuration

This shows how a real surveillance task uses the single-instance principle — `plnr::expand_list` generates all disease/location/age combinations, while the action function contains only the epidemiological analysis for one combination:

```r
ss$add_task(
  name_grouping = "norsyss",
  name_action = "short_term_trends",
  name_variant = NULL,
  cores = 20,
  for_each_plan = plnr::expand_list(
    location_code = c("nation_nor", "county_nor03", "county_nor11", ...),  # 21 locations
    age = c("000_004", "005_014", "015_019", "020_029", "030_064", "015_064", "065p")  # 7 age groups
  ),
  for_each_analysis = NULL,
  universal_argset = NULL,
  upsert_at_end_of_each_plan = FALSE,
  insert_at_end_of_each_plan = FALSE,
  action_fn_name = "norsyss::short_term_trends_action",
  data_selector_fn_name = "norsyss::short_term_trends_data_selector",
  tables = list(
    "anon_norsyss_data" = ss$tables$anon_norsyss_data,
    "anon_large_scale_surveillance_short_term_trends" = ss$tables$anon_large_scale_surveillance_short_term_trends
  )
)
```

### Single-Instance Action Function (from paper)

The action function contains only the epidemiological logic for **one** disease-location-age combination. CS9 scales this across all combinations automatically:

```r
short_term_trends_action <- function(data, argset, tables) {
  # Run the epidemiological analysis for one instance
  x <- csalert::short_term_trend(
    data$data,
    numerator = "numerator_n",
    denominator = "denominator_n",
    prX = c(100),
    trend_isoyearweeks = 5,
    remove_last_isoyearweeks = 0,
    forecast_isoyearweeks = 2,
    numerator_naming_prefix = "generic",
    denominator_naming_prefix = "generic",
    statistics_naming_prefix = "universal",
    remove_training_data = TRUE,
    include_decreasing = FALSE,
    alpha = 0.10
  )
  # Insert the results to a database table
  tables$anon_large_scale_surveillance_short_term_trends$insert_data(x)
}
```

### Data Selector (from paper)

```r
short_term_trends_data_selector <- function(argset, tables) {
  # Extract the requested data from the database
  data <- tables$anon_norsyss_data$tbl() %>%
    dplyr::filter(location_code %in% argset$location_code) %>%
    dplyr::filter(age %in% argset$age) %>%
    dplyr::collect()
  retval <- list(
    "data" = data
  )
  retval
}
```

### Surveillance Schema Example

```r
field_types = c(
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

  "date" = "DATE",

  "n_consultations" = "INTEGER",
  "n_population" = "INTEGER",
  "trend_status" = "TEXT",           # "increasing", "stable", "decreasing"
  "trend_value" = "DOUBLE",
  "threshold_exceedance" = "DOUBLE"
)
```

### NorSySS Performance Metrics (from Table 1)

| Task | Scope | Time | Cores |
|------|-------|------|-------|
| Cleaning data | ~300M consultations → ~1B rows, ~1M time series | 7 hours | 10 |
| Estimating short-term trends | 106 diseases × 9 ages × 21 locations × 20yr | 4 hours | 20 |
| Nowcasting | 106 diseases × 9 ages × 3 sexes × 1 location | 5 minutes | 4 |
| Estimating MEM thresholds | 1 disease × 8 ages × 21 locations × 10 seasons | 15 minutes | 2 |
| Estimating weekly exceedance | 106 diseases × 9 ages × 21 locations × 20yr | 10 minutes | 20 |
| Producing summary figures | 106 portrait + 106 landscape figures | 5 minutes | 20 |

## CRAN Deployment Considerations

### Package Distribution Strategy

**Infrastructure Requirements vs CRAN Compatibility**
CS9 is fundamentally designed for database-driven surveillance systems, which creates unique challenges for CRAN distribution:

- **Core Architecture**: Requires PostgreSQL/MariaDB backend for full functionality
- **CRAN Environment**: Minimal, no external database connections available
- **Solution**: Graceful degradation approach with clear user guidance

### Environment Configuration Best Practices

**Robust .onLoad() Implementation**
```r
.onLoad <- function(libname, pkgname) {
  # Phase 1: Environment variable setup with error handling
  env_result <- tryCatch({
    set_env_vars()
    TRUE
  }, error = function(e) {
    packageStartupMessage("Warning: Environment setup failed: ", e$message)
    FALSE
  })
  
  # Phase 2: Database setup (only if environment configured)
  if(!env_result || length(config$dbconfigs) == 0){
    packageStartupMessage("CS9 database configuration not available. Package loaded with limited functionality.")
    packageStartupMessage("Use cs9::check_environment_setup() to diagnose configuration issues.")
  } else {
    # Attempt database connection with graceful failure
    db_result <- tryCatch({
      setup_database_tables()
      TRUE
    }, error = function(e) {
      packageStartupMessage("Warning: Database table setup failed: ", e$message)
      packageStartupMessage("CS9 loaded with limited functionality.")
      FALSE
    })
  }
}
```

**Key Patterns**:
- **Modular setup**: Separate environment and database initialization
- **Error isolation**: Use tryCatch for each setup phase
- **User guidance**: Provide clear next steps when setup fails
- **Diagnostic tools**: Include `check_environment_setup()` function

### Required Environment Variables

**Database Configuration**
```bash
# Essential variables for CS9 operation
CS9_DBCONFIG_ACCESS="config/anon/restr"
CS9_DBCONFIG_DRIVER="PostgreSQL"
CS9_DBCONFIG_SERVER="localhost"
CS9_DBCONFIG_USER="username"
CS9_DBCONFIG_PASSWORD="password"

# Schema-specific configuration
CS9_DBCONFIG_SCHEMA_CONFIG="schema_name"  
CS9_DBCONFIG_DB_CONFIG="database_name"

# Optional variables with defaults
CS9_PATH=""                    # Defaults to empty string
CS9_AUTO="0"                   # Defaults to FALSE
```

### Testing Strategy for CRAN

**No Traditional Tests Approach**
- **Removed**: `/tests/` directory entirely for CRAN submission
- **Rationale**: Database infrastructure cannot be mocked in CRAN environment
- **Alternative**: Comprehensive documentation with `\dontrun{}` examples
- **Local testing**: Use `devtools::load_all()` in development environment

**Documentation Testing**
```r
# Examples that work in CRAN environment (no database)
#' @examples
#' \dontrun{
#' # Requires database configuration
#' ss <- cs9::SurveillanceSystem_v9$new()
#' ss$add_table(...)
#' }
#' 
#' # Simple examples that work without database
#' cs9::check_environment_setup()
```

### User Setup Guidance

**Installation Instructions**
Users need clear guidance for post-installation setup:

1. **Database Setup**: PostgreSQL/MariaDB instance required
2. **Environment Variables**: Set CS9_DBCONFIG_* variables
3. **Verification**: Use `cs9::check_environment_setup()` 
4. **Troubleshooting**: Clear error messages guide configuration

**Diagnostic Function Pattern**
```r
#' Check Environment Setup
#' @export
check_environment_setup <- function() {
  # Check required environment variables
  # Validate database configuration
  # Test database connectivity
  # Provide actionable recommendations
  # Return structured results
}
```

### Version Control Best Practices

**Build Artifact Management**
- **Never commit**: `..Rcheck/` directories from `R CMD check`
- **Git hygiene**: Use `git status` before commits
- **Cleanup workflow**: `git reset --soft HEAD~1` to fix commits

**CRAN Submission Branches**
- **Main branch**: Essential CRAN requirements only
- **Feature branches**: Optional robustness improvements
- **Clean separation**: Must-have vs nice-to-have changes

This approach ensures CS9 can be distributed via CRAN while maintaining its core database-driven architecture and providing clear guidance for users setting up the full surveillance infrastructure.

## `R CMD check` cannot see a `::` call inside an R6 method

`R6::R6Class()` takes its methods as `public = list(...)`. The dependency scan in `R CMD check`
walks top-level function definitions, and it does not walk that list. So a `pkg::fn()` call inside
an R6 method is invisible to it, in both directions:

- An **undeclared** package raises no NOTE. Nothing warns you that `DESCRIPTION` is missing it.
- A **declared** package still lands in `Namespaces in Imports field not imported from`, because
  the scan finds no use of it.

Measured on 2026-08-15, cs9 26.8.18: that NOTE listed `callr`, `foreach`, `future`, `later` and
`progress`. Four of the five WERE used, each by a `::` call inside an R6 method. Only `progress`
was genuinely unused. **The NOTE is therefore mostly a false positive and does not clear by using
the package more.** Read it as "check by hand", not as "delete these".

**Check both directions before you act on it.** `26.8.19` removed `future` and `foreach`, because
the audit showed those two calls reset a backend cs9 never sets. That is the opposite conclusion
from `callr` and `later`, which are live. The NOTE cannot tell you which case you have.

**The NOTE has an exact meaning, and `26.8.19` measured it.** It lists every package used ONLY
inside an R6 method, plus every package genuinely unused. Nothing else. That release declared
three packages and CI reported:

```
Namespaces in Imports field not imported from:
  'callr' 'csutil' 'later' 'pbmcapply' 'progress'
```

`csutil` and `pbmcapply` joined the list. `utils` did not, although all three were declared in the
same commit. The reason is where the calls sit:

| Package | Calls | In the NOTE |
|---|---|---|
| `csutil`, `pbmcapply` | R6 methods only | yes |
| `utils` | `.onAttach()` and `update_config_tasks_stats()` are plain functions, plus one R6 method | no |
| `callr`, `later` | R6 methods only | yes |
| `progress` | none | yes |

One call in a plain top-level function is enough to clear a package from the NOTE. So a package
listed there is either R6-only or dead, and telling those apart needs the hand sweep below.
`csutil` and `pbmcapply` are on the NOTE and are load-bearing: `26.8.19` proves it by reverting
`csutil` and watching four tests go red.

The cost is not cosmetic, and it hit twice.

`Task$run()` called `future::plan()` and `foreach::registerDoSEQ()` for years with neither package
declared. `R CMD check` could not see the calls, and no test executed them, so both safety nets
were down at once. It surfaced only when `26.8.17` added a test that drives the public `Task$run()`
to completion.

`Task` called `splutil::unnest_dfs_within_list_of_fully_named_lists()` at four sites, with
`splutil` undeclared and absent from every NorSySS pod. `upsert_at_end_of_each_plan = TRUE` and
`insert_at_end_of_each_plan = TRUE` therefore raised `there is no package called 'splutil'` on
every pod. `26.8.19` switched them to `csutil`, which publishes the same utilities and IS
installed, and added `tests/testthat/test-plan-end-write.R`. **Neither feature had a test at all**,
which is the reason a broken call sat there unseen.

**The four sites are in TWO methods, and that split is easy to miss.** `run_sequential()` holds
`R/r6_Task.R:295` and `:301`. `run_parallel_plans()` holds `:351` and `:357`. A test that drives
only the sequential branch leaves half of it uncovered, and the NEWS entry for 26.8.19 claimed
full coverage on the strength of exactly that mistake before an adversarial review caught it.

The parallel pair fails worse. The worker wraps its body in a `tryCatch` with five attempts and
`Sys.sleep(5)` between them, so a missing package costs 25 seconds before it surfaces as
`Error in index N`.

**Sweep by hand instead. Compare every `::` call in `R/` against `DESCRIPTION`:**

```bash
grep -rhoE "\b[a-zA-Z][a-zA-Z0-9._]*::" R/ --include='*.R' | sed 's/:://' | sort -u > /tmp/used.txt
sed -n '/^Depends:/,/^License:/p' DESCRIPTION | grep -oE "^ +[a-zA-Z][a-zA-Z0-9._]*" \
  | tr -d ' ' | sort -u > /tmp/decl.txt
comm -23 /tmp/used.txt /tmp/decl.txt
```

Read the output by hand. It reports a match inside a comment and a match inside a template string
as well as a real call, so check each one before you declare it.

**No undeclared namespace call remains**, as of 26.8.19. `26.8.19` declared `csutil`, `pbmcapply`
and `utils`, and removed the `future` and `foreach` calls.

**Read every match against the source. Five of the sweep's matches are not calls:**

| Match | What it really is |
|---|---|
| `PACKAGE` | the template string `"PACKAGE::TASK_NAME_action"` in `R/addins.R` |
| `future.apply` | inside a comment, `R/r6_Task.R:454` |
| `cs9` | the package itself |

**`devtools` is NOT on that list, and the reasoning that put it there was wrong.**
`devtools::load_all('.')` at `R/r6_SurveillanceSystem.R:414` and `R/r6_TaskJob.R:72` is generated
text: cs9 writes it into a temporary `.R` file. But cs9 then EXECUTES that file in a child R
process, so the feature genuinely needs `devtools` at run time. Process separation moves the
dependency; it does not remove it. `devtools` is therefore in `Suggests`, which is the correct
category for a package that one optional feature needs.

The rule that follows: **ask what runs the text, not just who wrote it.** A string is a false
positive only when nothing executes it. `"PACKAGE::TASK_NAME_action"` is a template a human edits.
`devtools::load_all('.')` is a program cs9 runs.

**`utils` needs declaring even though it always works.** `utils::packageDescription()` loads the
namespace directly, so it does not depend on `utils` being attached. It works because `utils` ships
with R and is therefore always installed. That is a property of the R distribution, not a declared
dependency, so declare it.

## Licensing

This package is `MIT + file LICENSE`. Two files carry the licence and they MUST
agree with each other:

- `LICENSE` holds exactly two lines, `YEAR:` and `COPYRIGHT HOLDER:`. CRAN
  requires that shape for `MIT + file LICENSE`. Do not put the licence text there.
- `DESCRIPTION` `Authors@R` MUST name the same holder, with `role = "cph"`.

The copyright holder for this package is **Folkehelseinstituttet**.

**Check the year at the start of each calendar year, and whenever you edit
`DESCRIPTION`.** Nothing in `R CMD check` tests the copyright year, so a stale one
goes unnoticed indefinitely. A fleet sweep on 2026-08-06 found years of 2021, 2023
and 2025 still in place across 15 packages, and not one package declared a `cph`
role at all.

Check both in one step:

```r
readLines("LICENSE")
a <- unclass(eval(parse(text = read.dcf("DESCRIPTION")[1, "Authors@R"])))
Filter(function(p) "cph" %in% p$role, a)
```
