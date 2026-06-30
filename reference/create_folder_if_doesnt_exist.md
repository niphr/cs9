# Create Folder If It Doesn't Exist

Creates a directory and all necessary parent directories if they don't
already exist.

## Usage

``` r
create_folder_if_doesnt_exist(path)
```

## Arguments

- path:

  Character string specifying the directory path to create

## Value

Character string containing the created directory path

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a new directory
create_folder_if_doesnt_exist("/tmp/my_analysis/results")
} # }
```
