# Get Configuration Tables Last Updated

Retrieves the last updated timestamps for database tables from the
configuration tracking system.

## Usage

``` r
get_config_tables_last_updated(table_name = NULL)
```

## Arguments

- table_name:

  Character string specifying the table name to filter by. If NULL,
  returns data for all tables.

## Value

A data.table containing last updated information with columns:
table_name, last_updated_datetime, and other tracking metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Get last updated info for all tables
get_config_tables_last_updated()

# Get info for a specific table
get_config_tables_last_updated(table_name = "anon_covid_cases")
} # }
```
