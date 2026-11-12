# A generator that gives one row per combination of world elements

Returns a `function(profile, n, seed, world)` for the `generator` of a
synthetic spec. The function gives one row for each combination of the
world elements in `grid`, and draws the other columns with
[`csdb::simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.html).

## Usage

``` r
synthetic_grid_generator(grid, finish = NULL)
```

## Arguments

- grid:

  A character vector of names of world elements.

- finish:

  `NULL`, or a `function(d, seed, world)` that returns the data.table
  `d` with values for one table.

## Value

A `function(profile, n, seed, world)` that returns a data.table.

## Details

The generator works in this order:

1.  It stops when `world` is `NULL` or does not hold every element of
    `grid`.

2.  It takes one unit from each element of `grid`. A vector is one
    column of unique values. A data.frame gives its columns that are in
    `names(profile$field_types)`, as unique rows, so the values in one
    row stay together. An element with no column in the table is
    skipped.

3.  It crosses the units.
    [`data.table::CJ()`](https://rdrr.io/pkg/data.table/man/J.html)
    crosses the vectors, in the order of `grid`, and each data.frame is
    then crossed with every row so far. The grid is the result.

4.  It calls
    [`csdb::simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.html)
    for `nrow(grid)` rows, with `seed`. It removes `profile$keys`,
    because the grid makes the rows unique. It removes every data.frame
    element from the world. csdb draws wrong values for a data.frame
    element named like a TEXT column, and stops for one named like an
    INTEGER column.

5.  It replaces each grid column of the draw with the grid.

6.  It calls `finish(d, seed, world)` when `finish` is not `NULL`.
    `world` is the world as the generator got it, with its data.frame
    elements.

7.  It orders the columns as `names(profile$field_types)`, and returns
    the data.table.

The generator ignores `n`: the grid sets the number of rows. For a
partitioned table, put the partition column in the world and in `grid`,
so that every partition gets every combination.

The generator stops when the elements of `grid` give no column of the
table, or give one column twice. It installs the grid values with their
R types, so a world element MUST have the R type of its column.
[`synthetic_weeks()`](https://niphr.github.io/cs9/reference/synthetic_weeks.md)
and
[`synthetic_years()`](https://niphr.github.io/cs9/reference/synthetic_weeks.md)
give the time columns with those types.

## Examples

``` r
generator <- synthetic_grid_generator(c("location", "age", "weeks"))
world <- list(
  location = data.frame(
    location_code = c("nation_nor", "county_nor03"),
    granularity_geo = c("nation", "county")
  ),
  age = c("total", "000_004"),
  weeks = synthetic_weeks("2024-01", "2024-03")
)
profile <- csdb::profile_schema(
  c(location_code = "TEXT", granularity_geo = "TEXT", age = "TEXT",
    isoyearweek = "TEXT", n = "INTEGER"),
  keys = c("location_code", "age", "isoyearweek")
)
generator(profile, n = NULL, seed = 1L, world = world)
#>     location_code granularity_geo     age isoyearweek     n
#>            <char>          <char>  <char>      <char> <int>
#>  1:    nation_nor          nation   total     2024-01    38
#>  2:    nation_nor          nation   total     2024-02    50
#>  3:    nation_nor          nation   total     2024-03    41
#>  4:  county_nor03          county   total     2024-01     5
#>  5:  county_nor03          county   total     2024-02    23
#>  6:  county_nor03          county   total     2024-03    31
#>  7:    nation_nor          nation 000_004     2024-01    13
#>  8:    nation_nor          nation 000_004     2024-02     1
#>  9:    nation_nor          nation 000_004     2024-03    44
#> 10:  county_nor03          county 000_004     2024-01    17
#> 11:  county_nor03          county 000_004     2024-02    21
#> 12:  county_nor03          county 000_004     2024-03    77
```
