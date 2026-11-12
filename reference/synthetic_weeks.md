# The time columns of weeks and of isoyears

`synthetic_weeks()` returns one row for each isoyearweek from `from` to
`to`. `synthetic_years()` returns one row for each isoyear of those
weeks.

## Usage

``` r
synthetic_weeks(from, to)

synthetic_years(from, to)
```

## Arguments

- from, to:

  The first and the last isoyearweek, as text, for example `"2021-30"`.

## Value

A data.table with the 12 time columns.

## Details

Each function returns the 12 time columns of csfmt_rts_data_v2:
`granularity_time`, `isoyearweek`, `isoyear`, `isoweek`, `isoquarter`,
`isoyearquarter`, `season`, `seasonweek`, `date`, `calyear`, `calmonth`
and `calyearmonth`. cstime derives each column except `granularity_time`
from `isoyearweek`. `date` is the last date of the week, and `calyear`,
`calmonth` and `calyearmonth` come from `date`. The integer columns are
integer, `seasonweek` is numeric, and `date` is a `Date`.

`synthetic_weeks()` sets `granularity_time` to `"isoyearweek"`.
`synthetic_years()` sets it to `"isoyear"`. A row of `synthetic_years()`
takes the last isoyearweek of its isoyear, from
[`cstime::isoyear_to_last_isoyearweek_c()`](https://rdrr.io/pkg/cstime/man/isoyear_to_last_isoyearweek_c.html).
That week can be after `to`.

Put the result in the world as a data.frame element. Then
[`synthetic_grid_generator()`](https://niphr.github.io/cs9/reference/synthetic_grid_generator.md)
gives each table the time columns that it holds.

## Examples

``` r
synthetic_weeks("2021-30", "2021-32")
#>    granularity_time isoyearweek isoyear isoweek isoquarter isoyearquarter
#>              <char>      <char>   <int>   <int>      <int>         <char>
#> 1:      isoyearweek     2021-30    2021      30          3        2021-Q3
#> 2:      isoyearweek     2021-31    2021      31          3        2021-Q3
#> 3:      isoyearweek     2021-32    2021      32          3        2021-Q3
#>       season seasonweek       date calyear calmonth calyearmonth
#>       <char>      <num>     <Date>   <int>    <int>       <char>
#> 1: 2021/2022         48 2021-08-01    2021        8     2021-M08
#> 2: 2021/2022         49 2021-08-08    2021        8     2021-M08
#> 3: 2021/2022         50 2021-08-15    2021        8     2021-M08
synthetic_years("2021-30", "2022-10")
#>    granularity_time isoyearweek isoyear isoweek isoquarter isoyearquarter
#>              <char>      <char>   <int>   <int>      <int>         <char>
#> 1:          isoyear     2021-52    2021      52          4        2021-Q4
#> 2:          isoyear     2022-52    2022      52          4        2022-Q4
#>       season seasonweek       date calyear calmonth calyearmonth
#>       <char>      <num>     <Date>   <int>    <int>       <char>
#> 1: 2021/2022         18 2022-01-02    2022        1     2022-M01
#> 2: 2022/2023         18 2023-01-01    2023        1     2023-M01
```
