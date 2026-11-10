# Upload a file under CS9_PATH to the same path on the share

`path_put()` takes the arguments of
[`path()`](https://niphr.github.io/cs9/reference/path.md) and uploads
the file at that path to the same relative path on the SMB share in
`cs9::config$share`. It returns the local path invisibly.

## Usage

``` r
path_put(
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

The local path from
[`path()`](https://niphr.github.io/cs9/reference/path.md), invisibly.

## Details

The share comes from the environment variable `CS9_SHARE`, for example
`//server/share`. Each call runs `smbclient` with
`--use-kerberos=required -N`, so the session needs a Kerberos ticket.

The upload has four steps:

1.  `mkdir` for each remote folder above the file, one by one. A folder
    that exists already is no error.

2.  `put` to the temporary name `<name>.partial-<pid>-<hex>` in the same
    remote folder.

3.  `ls` of the temporary name. The size on the share MUST be the local
    size.

4.  `rename ... -f`, which replaces the final name.

When step 2 or step 3 fails, `path_put()` sends no `rename` and deletes
nothing under the final name. It tries to delete the temporary name, and
it stops with an error that contains the local path.

`path_put()` always stops in these two cases, also without a share:

- The path is not under `cs9::config$path`, or `cs9::config$path` is
  empty.

- A path component is `.` or `..`.

When `cs9::config$share` is empty, `path_put()` then returns the path.
It uploads nothing and does not check the local file. With a share, it
also stops before it runs `smbclient` in these cases:

- A name contains `"`, `\`, `*`, `?`, `;` or a control character.

- The local file does not exist.

## See also

[`path_get()`](https://niphr.github.io/cs9/reference/path_get.md),
[`path_ls()`](https://niphr.github.io/cs9/reference/path_ls.md)

## Examples

``` r
if (FALSE) { # \dontrun{
saveRDS(mtcars, cs9::path("output", "weather", "results.rds"))
cs9::path_put("output", "weather", "results.rds")
} # }
```
