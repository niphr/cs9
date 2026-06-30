# Creating a task

This vignette shows how to create and configure surveillance tasks in
CS9.

## Overview

Creating a task involves three main steps:

1.  **Define database tables** with schemas that describe your data
    structure
2.  **Configure the task** — specify data processing workflows and
    execution parameters
3.  **Implement task functions** — the data selector and action
    functions that do the work

## Prerequisites

You need CS9 installed and configured before following this tutorial.
See
[`vignette("installation")`](https://niphr.github.io/cs9/articles/installation.md)
for database configuration and environment variable setup.

Verify your setup with:

``` r
# Check if CS9 environment is properly configured
cs9::check_environment_setup()
```

For an overview of CS9 concepts and architecture, see
[`vignette("cs9")`](https://niphr.github.io/cs9/articles/cs9.md); for
file organization, see
[`vignette("file-layout")`](https://niphr.github.io/cs9/articles/file-layout.md).

## Step 1: Initialize a surveillance system

Create a surveillance system instance, which manages your tables and
tasks:

``` r
# Create a surveillance system
library(cs9)

# Initialize the surveillance system
ss <- cs9::SurveillanceSystem_v9$new(
  name = "example_surveillance",
  implementation_version = "1.0.0"
)

# Verify the system was created
print(ss$name)
print(ss$implementation_version)
```

## Step 2: Define database tables

Tables must be defined before tasks can use them. Each table definition
requires:

- **Field types**: Column names and their data types
- **Keys**: Columns that uniquely identify a record
- **Access level**: Database access permissions (e.g., `"anon"`)

``` r
# Define a table for weather data
ss$add_table(
  name_access = "anon",           # Anonymous access level
  name_grouping = "weather",      # Data category
  name_variant = "daily_data",    # Specific variant
  field_types = c(
    # Temporal fields
    "date" = "DATE",
    "year" = "INTEGER", 
    "month" = "INTEGER",
    "day" = "INTEGER",
    
    # Geographic fields
    "location_code" = "TEXT",
    "location_name" = "TEXT",
    
    # Weather measurements
    "temperature_max" = "DOUBLE",
    "temperature_min" = "DOUBLE", 
    "precipitation" = "DOUBLE",
    "humidity" = "DOUBLE"
  ),
  keys = c("date", "location_code"),  # Unique identifier
  indexes = list(
    "idx_date" = "date",
    "idx_location" = "location_code", 
    "idx_date_location" = c("date", "location_code")
  )
)

# Define a table for processed results
ss$add_table(
  name_access = "anon",
  name_grouping = "weather", 
  name_variant = "weekly_summary",
  field_types = c(
    "year_week" = "TEXT",
    "location_code" = "TEXT",
    "avg_temp_max" = "DOUBLE",
    "avg_temp_min" = "DOUBLE",
    "total_precipitation" = "DOUBLE",
    "data_quality_score" = "DOUBLE"
  ),
  keys = c("year_week", "location_code")
)

# Check which tables are available
print("Available tables:")
print(names(ss$tables))
```

## Step 3: Implement task functions

Every task needs two functions:

1.  **Data selector function**: Extracts and prepares data once per plan
2.  **Action function**: Performs the analysis and stores results

### Data selector function

The data selector retrieves the data needed for each plan:

``` r
# Data selector function for weather processing
weather_data_selector <- function(argset, tables) {
  # In a real implementation, this would query your database
  # For demonstration, we'll create sample data
  
  sample_data <- data.table::data.table(
    date = seq.Date(
      from = as.Date(argset$date_from),
      to = as.Date(argset$date_to), 
      by = "day"
    ),
    location_code = rep(argset$location_code, 
                       length.out = as.numeric(as.Date(argset$date_to) - as.Date(argset$date_from)) + 1),
    temperature_max = rnorm(as.numeric(as.Date(argset$date_to) - as.Date(argset$date_from)) + 1, 
                           mean = 20, sd = 8),
    temperature_min = rnorm(as.numeric(as.Date(argset$date_to) - as.Date(argset$date_from)) + 1, 
                           mean = 10, sd = 5),
    precipitation = rgamma(as.numeric(as.Date(argset$date_to) - as.Date(argset$date_from)) + 1, 
                          shape = 2, rate = 4)
  )
  
  # Add derived fields
  sample_data[, year := as.integer(format(date, "%Y"))]
  sample_data[, month := as.integer(format(date, "%m"))]
  sample_data[, day := as.integer(format(date, "%d"))]
  sample_data[, location_name := paste("Location", location_code)]
  sample_data[, humidity := runif(.N, 30, 90)]
  
  return(list(
    data = sample_data
  ))
}
```

### Action function

The action function performs the analysis for each plan/analysis
combination:

``` r
# Action function for weather processing
weather_action <- function(data, argset, tables) {
  # Process the daily weather data into weekly summaries
  
  # Add week information
  data$data[, year_week := format(date, "%Y-W%U")]
  
  # Calculate weekly aggregates
  weekly_summary <- data$data[, .(
    avg_temp_max = mean(temperature_max, na.rm = TRUE),
    avg_temp_min = mean(temperature_min, na.rm = TRUE), 
    total_precipitation = sum(precipitation, na.rm = TRUE),
    data_quality_score = 1.0 - sum(is.na(temperature_max) | is.na(temperature_min)) / .N
  ), by = .(year_week, location_code)]
  
  # Insert results into database table
  # In a real implementation, this would use:
  # tables$anon_weather_weekly_summary$upsert_data(weekly_summary)
  
  # For demonstration, just print results
  cat("Processed weekly weather summary:\n")
  print(weekly_summary)
  
  # Log successful completion
  cs9::update_config_log(
    ss = argset$surveillance_system,
    task = "weather_process_weekly", 
    paste("Successfully processed", nrow(weekly_summary), "weekly records for", argset$location_code)
  )
}
```

## Step 4: Configure the task

Register the task with the surveillance system:

``` r
# Add the weather processing task
ss$add_task(
  name_grouping = "weather",
  name_action = "process", 
  name_variant = "weekly_summary",
  cores = 1,                              # Single core execution
  permission = NULL,                      # No special permissions needed
  
  # Task structure - one plan per location per month
  for_each_plan = list(
    location_code = c("LOC001", "LOC002", "LOC003"),
    date_from = c("2024-01-01", "2024-02-01", "2024-03-01"),  
    date_to = c("2024-01-31", "2024-02-28", "2024-03-31")
  ),
  
  # No analysis-level iteration needed
  for_each_analysis = NULL,
  
  # Common arguments for all plans  
  universal_argset = list(
    surveillance_system = "example_surveillance",
    data_format = "cs9_standard"
  ),
  
  # Automatically insert results at end of each plan
  upsert_at_end_of_each_plan = TRUE,
  insert_at_end_of_each_plan = FALSE,
  
  # Function names
  action_fn_name = "weather_action",
  data_selector_fn_name = "weather_data_selector",
  
  # Table mapping
  tables = list(
    weather_daily = "anon_weather_daily_data", 
    weather_weekly = "anon_weather_weekly_summary"
  )
)

# Verify the task was added
print("Available tasks:")
print(names(ss$tasks))
```

## Step 5: Execute the task

Run the task and inspect its plans:

``` r
# Get task information
task_name <- "weather_process_weekly_summary"

# View task plans and analyses
if(task_name %in% names(ss$tasks)) {
  plans_info <- ss$shortcut_get_plans_argsets_as_dt(task_name)
  cat("Task execution plans:\n")
  print(plans_info)
  
  # Run the task
  cat("\nExecuting surveillance task...\n")
  ss$run_task(task_name)
  
  # Check task completion status
  task_stats <- cs9::get_config_tasks_stats(task = task_name, last_run = TRUE)
  if(nrow(task_stats) > 0) {
    cat("\nTask execution completed successfully!\n")
    print(task_stats[, .(task, datetime, status)])
  }
}
```

## Step 6: Monitor and debug tasks

CS9 provides utilities for inspecting task execution:

``` r
# Get all available tables
cat("All surveillance system tables:\n")
print(names(ss$tables))

# Get task execution logs  
recent_logs <- cs9::get_config_log(
  task = "weather_process_weekly",
  start_date = Sys.Date() - 7
)
if(nrow(recent_logs) > 0) {
  cat("\nRecent task logs:\n") 
  print(recent_logs[, .(datetime, task, message)])
}

# Get task performance statistics
task_performance <- cs9::get_config_tasks_stats(last_run = TRUE)
if(nrow(task_performance) > 0) {
  cat("\nTask performance summary:\n")
  print(task_performance[, .(task, datetime, runtime_seconds, status)])
}

# Access data for a specific plan (for debugging)
if(task_name %in% names(ss$tasks)) {
  # Get data for first plan
  debug_data <- ss$shortcut_get_data(task_name, index_plan = 1)
  cat("\nData structure for plan 1:\n")
  if(!is.null(debug_data$data)) {
    print(str(debug_data$data))
  }
  
  # Get arguments for first plan, first analysis
  debug_args <- ss$shortcut_get_argset(task_name, index_plan = 1, index_analysis = 1)
  cat("\nArgument set for plan 1, analysis 1:\n")
  print(debug_args)
}
```

## Advanced task features

### Parallel processing

Set `cores > 1` for computationally intensive tasks:

``` r
# Configure a task for parallel execution
ss$add_task(
  name_grouping = "weather",
  name_action = "analyze",
  name_variant = "trends", 
  cores = 4,  # Use 4 CPU cores
  
  # Large number of plans that can benefit from parallelization
  for_each_plan = list(
    location_code = sprintf("LOC%03d", 1:100),  # 100 locations
    analysis_year = rep(2020:2024, length.out = 100)  # 5 years
  ),
  
  universal_argset = list(
    min_data_quality = 0.8,
    trend_method = "linear_regression"
  ),
  
  action_fn_name = "weather_trend_action",
  data_selector_fn_name = "weather_trend_data_selector",
  
  tables = list(
    input_data = "anon_weather_daily_data",
    trend_results = "anon_weather_trends"
  )
)
```

### Dynamic plan generation

When the plan list depends on database state, use
`plan_analysis_fn_name` to generate plans at runtime:

``` r
# Function to generate plans based on database state
generate_weather_plans <- function() {
  # In real implementation, query database for available data
  available_locations <- c("LOC001", "LOC002", "LOC003")
  available_years <- 2020:2024
  
  # Generate plans for locations with sufficient data
  plans <- list()
  for(location in available_locations) {
    for(year in available_years) {
      plans <- append(plans, list(list(
        location_code = location,
        year = year,
        min_date = paste0(year, "-01-01"),
        max_date = paste0(year, "-12-31")
      )))
    }
  }
  
  return(list(
    for_each_plan = plans,
    for_each_analysis = NULL
  ))
}

# Use the custom plan generator
ss$add_task(
  name_grouping = "weather",
  name_action = "validate", 
  name_variant = "data_quality",
  cores = 2,
  
  # Use function to generate plans
  plan_analysis_fn_name = "generate_weather_plans",
  for_each_plan = NULL,  # Will be generated by function
  for_each_analysis = NULL,
  
  universal_argset = list(
    quality_threshold = 0.85,
    validation_rules = c("completeness", "consistency", "accuracy")
  ),
  
  action_fn_name = "weather_validation_action",
  data_selector_fn_name = "weather_validation_data_selector", 
  
  tables = list(
    source_data = "anon_weather_daily_data",
    validation_results = "anon_weather_validation"
  )
)
```

## Best practices

### Error handling

Include error handling in task functions so failures are logged and
diagnosable:

``` r
robust_weather_action <- function(data, argset, tables) {
  tryCatch({
    # Validate input data
    if(is.null(data$data) || nrow(data$data) == 0) {
      warning("No data available for processing")
      return(invisible(NULL))
    }
    
    # Check required columns
    required_cols <- c("date", "location_code", "temperature_max")
    missing_cols <- setdiff(required_cols, names(data$data))
    if(length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    # Perform analysis with validation
    result <- data$data[, .(
      avg_temp = mean(temperature_max, na.rm = TRUE),
      record_count = .N
    ), by = location_code]
    
    # Validate results before storing
    if(any(is.na(result$avg_temp))) {
      warning("Some temperature averages could not be calculated")
    }
    
    # Log successful completion
    cs9::update_config_log(
      ss = argset$surveillance_system,
      task = "weather_analysis",
      paste("Successfully processed", nrow(result), "location summaries")
    )
    
  }, error = function(e) {
    # Log errors for debugging
    cs9::update_config_log(
      ss = argset$surveillance_system, 
      task = "weather_analysis",
      paste("ERROR:", e$message)
    )
    stop(e)
  })
}
```

### Data validation

Check completeness, ranges, and temporal consistency before writing to
the database:

``` r
validate_weather_data <- function(data) {
  validation_results <- list()
  
  # Check data completeness
  completeness <- data[, lapply(.SD, function(x) sum(!is.na(x))/.N)]
  validation_results$completeness <- completeness
  
  # Check data ranges
  temp_range_check <- data[temperature_max < -50 | temperature_max > 60, .N]
  validation_results$temperature_outliers <- temp_range_check
  
  # Check temporal consistency
  date_gaps <- data[order(date), .(
    max_gap = max(as.numeric(diff(date)), na.rm = TRUE)
  ), by = location_code]
  validation_results$temporal_gaps <- date_gaps
  
  return(validation_results)
}
```

### Modular design

Shared transformations can be extracted into utility functions and
reused across tasks:

``` r
# Utility function for common data transformations
standardize_weather_data <- function(data) {
  # Apply common transformations
  data[, temperature_celsius := round(temperature_max, 1)]
  data[, date_formatted := format(date, "%Y-%m-%d")]
  data[, is_weekend := weekdays(date) %in% c("Saturday", "Sunday")]
  
  return(data)
}

# Reusable data selector template
create_weather_data_selector <- function(date_column = "date", 
                                       location_column = "location_code") {
  function(argset, tables) {
    # Common data selection logic
    query_data <- tables[[argset$source_table]]$tbl() %>%
      dplyr::filter(
        !!rlang::sym(date_column) >= argset$date_from,
        !!rlang::sym(date_column) <= argset$date_to,
        !!rlang::sym(location_column) %in% argset$location_codes
      ) %>%
      dplyr::collect() %>%
      data.table::as.data.table()
    
    # Apply standardization
    standardized_data <- standardize_weather_data(query_data)
    
    return(list(data = standardized_data))
  }
}
```

## Summary

The workflow for creating a task in CS9:

1.  Initialize a surveillance system
2.  Define database table schemas
3.  Implement the data selector and action functions
4.  Register the task with `ss$add_task()`
5.  Run with `ss$run_task()` and inspect execution logs

## See also

- [`vignette("cs9")`](https://niphr.github.io/cs9/articles/cs9.md) —
  package overview and architecture
- [`vignette("installation")`](https://niphr.github.io/cs9/articles/installation.md)
  — installation and environment setup
- [`vignette("file-layout")`](https://niphr.github.io/cs9/articles/file-layout.md)
  — package structure and file organization
- [`?SurveillanceSystem_v9`](https://niphr.github.io/cs9/reference/SurveillanceSystem_v9.md)
  — surveillance system class reference
- [`?check_environment_setup`](https://niphr.github.io/cs9/reference/check_environment_setup.md)
  — environment diagnostics
