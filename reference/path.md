# Get Results Folder Path

Constructs the appropriate folder path for surveillance system results,
with automatic switching between production and interactive modes.

## Usage

``` r
path(
  ...,
  create_dir = FALSE,
  trailing_slash = FALSE,
  auto = cs9::config$is_auto
)
```

## Arguments

- ...:

  Character strings specifying the second level directory and beyond

- create_dir:

  Logical value indicating whether to create the directory if it doesn't
  exist. Defaults to FALSE.

- trailing_slash:

  Logical value indicating whether to add a trailing slash to the
  returned path. Defaults to FALSE.

- auto:

  Logical value indicating whether this is running in automatic mode
  (uses base directory) or interactive mode (adds "\_interactive"
  subdirectory). Defaults to the current cs9::config\$is_auto setting.

## Value

Character string containing the constructed file path

## Examples

``` r
if (FALSE) { # \dontrun{
# Get basic output path
path("reports", "daily")

# Create directory if it doesn't exist
path("reports", "daily", create_dir = TRUE)

# Get path with trailing slash
path("reports", "daily", trailing_slash = TRUE)
} # }
```
