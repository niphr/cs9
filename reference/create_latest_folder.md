# Create Latest Folder

Copies results from a dated folder to a "latest" folder, providing easy
access to the most recent analysis results.

## Usage

``` r
create_latest_folder(results_folder_name, date)
```

## Arguments

- results_folder_name:

  Character string specifying the name of the results folder
  (subdirectory under "output")

- date:

  Character string specifying the date of extraction (used to identify
  the source folder)

## Value

No return value. This function is called for its side effect of copying
files from the dated folder to the latest folder.

## Details

This function copies all contents from `output/results_folder_name/date`
to `output/results_folder_name/latest`, overwriting existing files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Copy today's results to latest folder
create_latest_folder("covid_reports", "2024-01-15")
} # }
```
