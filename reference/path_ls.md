# List the entries of a folder on the share

`path_ls()` takes the arguments of
[`path()`](https://niphr.github.io/cs9/reference/path.md) and lists the
entries directly in that folder on the SMB share in `cs9::config$share`.
When `cs9::config$share` is empty, it lists the local folder with
[`fs::dir_info()`](https://fs.r-lib.org/reference/dir_ls.html).

## Usage

``` r
path_ls(
  ...,
  create_dir = FALSE,
  trailing_slash = FALSE,
  auto = cs9::config$is_auto
)
```

## Arguments

- ...:

  Character strings specifying the second level directory and beyond.

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

A `data.table` with one row per entry and four columns:

- `path`: the local path of the entry, under `cs9::config$path`.

- `type`: `"file"` or `"directory"`. Without a share, the type from
  [`fs::dir_info()`](https://fs.r-lib.org/reference/dir_ls.html), as
  text.

- `size`: the size in bytes, as a number.

- `modification_time`: a `POSIXct` in the time zone UTC.

## Details

The entries keep the order of `smbclient ls`, and `.` and `..` are not
entries. The checks of the path and of the names in
[`path_put()`](https://niphr.github.io/cs9/reference/path_put.md) apply
here too, before any `smbclient` call. Without a share, only the checks
of the path apply. A folder that does not exist on the share is an error
that contains the local path.

## See also

[`path_put()`](https://niphr.github.io/cs9/reference/path_put.md),
[`path_get()`](https://niphr.github.io/cs9/reference/path_get.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cs9::path_ls("input", "weather")
} # }
```
