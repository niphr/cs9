# Get Configuration Log

Retrieves configuration log entries from the `config_log` table with
optional filtering by surveillance system identifier, task name, and
date range.

## Usage

``` r
get_config_log(ss = NULL, task = NULL, start_date = NULL, end_date = NULL)
```

## Arguments

- ss:

  Character. Surveillance system identifier to filter by. Defaults to
  `NULL` (no filtering).

- task:

  Character. Task name to filter by. Defaults to `NULL` (no filtering).

- start_date:

  Character. Start date (`YYYY-MM-DD`) for filtering log entries.
  Defaults to `NULL`.

- end_date:

  Character. End date (`YYYY-MM-DD`) for filtering log entries. Defaults
  to `NULL`.

## Value

A `data.table` containing the filtered log entries.

## Details

The function retrieves entries from the `config_log` table in the
current configuration. If date filters are provided, they are applied to
the `timestamp` field of the log entries.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all log entries
get_config_log()

# Get logs for a specific surveillance system
get_config_log(ss = "weather")

# Get logs for a specific task and date range
get_config_log(task = "data_import", start_date = "2024-01-01", end_date = "2024-12-31")
} # }
```
