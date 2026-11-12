# Use SQLite databases in one directory as the database configuration

Points every access in `access` at a SQLite file in `dir` and rebuilds
the four configuration tables. It reads and sets no environment
variable.

## Usage

``` r
use_sqlite_default(dir, access = c("config", "anon"))
```

## Arguments

- dir:

  The directory that holds the SQLite files.

- access:

  The access names, one SQLite file for each.

## Value

`invisible(NULL)`, called for its effect on `config`.

## Details

Access `a` uses the file `fs::path(dir, paste0(a, ".sqlite"))`. The
function creates `dir` if it does not exist. Each entry of
`config$dbconfigs` has the fields and the `id` rule that
`CS9_DBCONFIG_DRIVER=SQLite` gives through
[`reload_db_config()`](https://niphr.github.io/cs9/reference/reload_db_config.md).
The fields that SQLite does not read are empty strings, and `port` is
`NA_integer_`.

The function sets `config$path` to `dir` and `config$is_auto` to
`FALSE`. It does not change `config$share`.

A child R process does not inherit this configuration, because no
environment variable carries it. Call it in a package's `.onLoad()`.
Each child R process that loads that package then calls it again, with
the child's own arguments.

## State safety

The function disconnects every table in `config$tables` and empties the
list before it changes `config$dbconfigs`. A failure after that point
leaves `config$tables` empty. `access` MUST contain `"config"`, because
the four configuration tables use the `config` access.

## See also

[`reload_db_config()`](https://niphr.github.io/cs9/reference/reload_db_config.md),
which reads the configuration from the `CS9_DBCONFIG_*` environment
variables.

## Examples

``` r
if (FALSE) { # \dontrun{
# In a dependent package's .onLoad()
cs9::use_sqlite_default(file.path(tempdir(), "cs9"))
} # }
```
