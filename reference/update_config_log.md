# Update Configuration Log

Logs configuration updates with relevant metadata such as timestamp,
session state, task name, and a custom message. The function inserts the
log entry into the `config_log` table in the current configuration.

## Usage

``` r
update_config_log(ss = "unspecified", task = "unspecified", ...)
```

## Arguments

- ss:

  Character. Surveillance system identifier. Defaults to
  `"unspecified"`.

- task:

  Character. Name of the task being logged. Defaults to `"unspecified"`.

- ...:

  Character. Custom message describing the log entry. Must not be
  `NULL`.

## Value

No return value; this function is called for its side effect of
inserting a log entry into the `config_log` table.

## Details

The function records the type of interaction (automatic or interactive),
session state, task description, and a user-provided message in the
configuration log. It throws an error if the `message` argument is
`NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
update_config_log(ss = "weather", task = "data_import", message = "Imported dataset successfully.")
} # }
```
