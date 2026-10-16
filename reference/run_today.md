# The date of the task run

Returns the date of the run with a `run_key` in progress: the date of
its first attempt, also on a retry after midnight. Outside such a run it
returns today's date, as
[`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) does.

## Usage

``` r
run_today()
```

## Value

A `Date`.

## Details

cs9 sets `argset$today` to this date. Use `run_today()` in a planner
instead of [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html), so that
every attempt of a run builds the same plans.

## Examples

``` r
run_today()
#> [1] "2026-10-16"
```
