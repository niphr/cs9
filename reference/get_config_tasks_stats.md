# Get Configuration Tasks Statistics

Retrieves runtime statistics and performance metrics for surveillance
tasks from the configuration database.

## Usage

``` r
get_config_tasks_stats(task = NULL, last_run = FALSE)
```

## Arguments

- task:

  Character string specifying the task name to filter by. If NULL,
  returns statistics for all tasks.

- last_run:

  Logical value indicating whether to return only the most recent run
  statistics. If FALSE, returns all historical data.

## Value

A data.table containing task statistics with columns including: task,
datetime, runtime_seconds, memory_usage, status, and other metrics

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all task statistics
get_config_tasks_stats()

# Get statistics for a specific task
get_config_tasks_stats(task = "covid_analysis")

# Get only the last run for a task
get_config_tasks_stats(task = "covid_analysis", last_run = TRUE)
} # }
```
