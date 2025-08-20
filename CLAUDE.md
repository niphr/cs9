# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

CS9 (Core Surveillance 9) is a free and open-source framework for real-time analysis and disease surveillance. It's an R package that provides generic infrastructure for implementing surveillance systems with database integration, task scheduling, and parallel processing capabilities.

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