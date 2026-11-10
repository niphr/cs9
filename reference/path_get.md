# Download a file or a folder under CS9_PATH from the same path on the share

`path_get()` takes the arguments of
[`path()`](https://niphr.github.io/cs9/reference/path.md) and downloads
that path from the SMB share in `cs9::config$share`, a file or a folder
with all its subfolders. It returns the local path when each remote file
exists locally with the remote size.

## Usage

``` r
path_get(
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
[`path()`](https://niphr.github.io/cs9/reference/path.md).

## Details

`path_get()` first lists the path on the share. A folder is listed
again, folder by folder, to find every file below it. `path_get()`
creates the local folders, also the empty ones, and sends one `get` per
file. It then compares the size of each local file with the size in that
listing.

`path_get()` stops, with an error that contains the local path, in these
cases:

- The remote path does not exist.

- A local file is missing, or its size is not the remote size.

- Any `smbclient` call reports an `NT_STATUS` code or a non-zero exit
  status.

The checks of the path and of the names in
[`path_put()`](https://niphr.github.io/cs9/reference/path_put.md) apply
here too, before any `smbclient` call. When `cs9::config$share` is
empty, only the checks of the path apply. `path_get()` then downloads
nothing, does not check the local path, and returns the path.

## See also

[`path_put()`](https://niphr.github.io/cs9/reference/path_put.md),
[`path_ls()`](https://niphr.github.io/cs9/reference/path_ls.md)

## Examples

``` r
if (FALSE) { # \dontrun{
folder <- cs9::path_get("input", "weather")
} # }
```
