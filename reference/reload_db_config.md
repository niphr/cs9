# Reload the database configuration from the environment

Re-reads every `CS9_DBCONFIG_*` environment variable and rebuilds the
four configuration tables.

## Usage

``` r
reload_db_config()
```

## Value

`invisible(NULL)`, called for its effect on `config`.

## Details

`cs9` reads the environment once, in its own `.onLoad()`. A package that
sets its own `CS9_DBCONFIG_*` values inside `.onLoad()` therefore sets
them too late: `cs9` is a dependency, so it loads first and has already
read the environment by the time the dependent package runs. Call this
function immediately after the
[`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) block and `cs9`
picks the new values up.

It re-runs `set_env_vars()`, which rebuilds `config$dbconfigs`, and then
`setup_database_tables()`, which rebuilds `config$tables`. Neither opens
a database connection:
[`csdb::DBTable_v9`](https://niphr.github.io/csdb/reference/DBTable_v9.html)
creates its table lazily, on first use.

## State safety

The reload is safe against both a failed reload and a repeated one.

It disconnects every table it is about to discard, so a second reload
does not leak the connections held by the R6 objects it replaces.

It then empties `config$tables` before it touches `config$dbconfigs`. A
failure anywhere after that point therefore leaves `config$tables` empty
rather than holding tables built from the previous configuration. An
empty table list is an honest "not configured"; a stale one describes a
database that `config$dbconfigs` no longer points at.

## See also

[`check_environment_setup()`](https://niphr.github.io/cs9/reference/check_environment_setup.md),
which reports whether the environment is complete. The installation
vignette,
[`vignette("installation", package = "cs9")`](https://niphr.github.io/cs9/articles/installation.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# In a dependent package's .onLoad(), before its own setup runs
Sys.setenv(CS9_DBCONFIG_ACCESS = "config/anon")
Sys.setenv(CS9_DBCONFIG_DRIVER = "SQLite")
Sys.setenv(CS9_DBCONFIG_DB_CONFIG = "/tmp/config.sqlite")
Sys.setenv(CS9_DBCONFIG_DB_ANON = "/tmp/anon.sqlite")
cs9::reload_db_config()
} # }
```
