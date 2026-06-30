# Get Configuration Data Hash for Each Plan

Retrieves data hash configuration entries from the database table for
tracking data changes across surveillance task plans and elements.

## Usage

``` r
get_config_data_hash_for_each_plan(
  task = NULL,
  index_plan = NULL,
  element_tag = NULL
)
```

## Arguments

- task:

  Character string specifying the task name to filter by. If NULL,
  returns data for all tasks.

- index_plan:

  Integer specifying the plan index to filter by. If NULL, returns data
  for all plans.

- element_tag:

  Character string specifying the element tag to filter by. If NULL,
  returns data for all elements.

## Value

A data.table containing the filtered hash configuration entries with
columns: task, index_plan, element_tag, date, datetime, element_hash,
all_hash

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all hash data for a specific task
get_config_data_hash_for_each_plan(task = "covid_analysis")

# Get hash data for specific task and plan
get_config_data_hash_for_each_plan(task = "covid_analysis", index_plan = 1)
} # }
```
