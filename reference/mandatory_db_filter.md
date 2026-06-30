# Filter surveillance data by standard epidemiological dimensions

Applies a set of standard include/exclude filters to a database table or
data frame using the conventional surveillance field names. Each `NULL`
argument is a no-op, so callers only need to supply the dimensions they
actually want to restrict.

## Usage

``` r
mandatory_db_filter(
  .data,
  granularity_time = NULL,
  granularity_time_not = NULL,
  granularity_geo = NULL,
  granularity_geo_not = NULL,
  country_iso3 = NULL,
  location_code = NULL,
  age = NULL,
  age_not = NULL,
  sex = NULL,
  sex_not = NULL
)
```

## Arguments

- .data:

  A data frame, `data.table`, or remote database table (e.g. the result
  of [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html))
  that contains the standard surveillance columns.

- granularity_time:

  Character vector. Values of `granularity_time` to keep. `NULL`
  (default) applies no filter.

- granularity_time_not:

  Character vector. Values of `granularity_time` to drop. `NULL`
  (default) applies no filter.

- granularity_geo:

  Character vector. Values of `granularity_geo` to keep. `NULL`
  (default) applies no filter.

- granularity_geo_not:

  Character vector. Values of `granularity_geo` to drop. `NULL`
  (default) applies no filter.

- country_iso3:

  Character vector. Values of `country_iso3` to keep. `NULL` (default)
  applies no filter.

- location_code:

  Character vector. Values of `location_code` to keep. `NULL` (default)
  applies no filter.

- age:

  Character vector. Values of `age` to keep. `NULL` (default) applies no
  filter.

- age_not:

  Character vector. Values of `age` to drop. `NULL` (default) applies no
  filter.

- sex:

  Character vector. Values of `sex` to keep. `NULL` (default) applies no
  filter.

- sex_not:

  Character vector. Values of `sex` to drop. `NULL` (default) applies no
  filter.

## Value

The filtered object in the same class as `.data`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inside a data_selector function, filter a remote table before collecting:
d <- schema$anon_covid_cases$tbl() |>
  mandatory_db_filter(
    granularity_time = "isoweek",
    granularity_geo  = "county",
    age_not          = "total"
  ) |>
  dplyr::collect()
} # }
```
