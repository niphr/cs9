# Check a comparison of staged and live rows against rules

`check_staged_against_live()` checks each key of a comparison from
[`compare_staged_to_live()`](https://niphr.github.io/cs9/reference/compare_staged_to_live.md)
against the rules in `rules`. It stops with an error that names every
failing key, and otherwise returns the comparison.

## Usage

``` r
check_staged_against_live(
  comparison,
  rules = c("partitions_agree", "no_missing", "no_decrease")
)
```

## Arguments

- comparison:

  A data.table from
  [`compare_staged_to_live()`](https://niphr.github.io/cs9/reference/compare_staged_to_live.md).
  Every column except `partition`, `staged` and `live` is a key column.

- rules:

  A character vector with one or more of `"partitions_agree"`,
  `"no_missing"` and `"no_decrease"`. An unknown name is an error.

## Value

`comparison`, invisibly, when every key passes. Otherwise the function
stops with an error of class `cs9_staged_check_error`. Its message names
each failing key and its problem. Its element `failures` is a data.table
with the key columns and `problem`, with one row per failing key.

## Details

A key is live when a live row of any partition holds it. These are the
rules:

- `partitions_agree`: the partitions that hold the key have one value in
  the live rows, and one value in the staged rows. "One value" means
  that [`unique()`](https://rdrr.io/r/base/unique.html) of the values
  has length 1.

- `no_missing`: a live key has a staged value.

- `no_decrease`: no staged value of a live key is below a live value of
  it. A staged value equal to the live value passes.

`no_missing` and `no_decrease` pass a key that is not live.
`partitions_agree` checks every key, also a key that is not live.

A key fails at most once, with the first problem in this order:

1.  `the live partitions disagree on the stored total (<values>)`

2.  `the partitions disagree on the staged total (<values>)`

3.  `staged none, stored <values>`

4.  `staged <values>, stored <values>`

`<values>` are the distinct values, sorted, without scientific notation,
and separated by ", ". These texts and this order are those of
`consultations_check()` in norsyss.cs9.

## See also

[`compare_staged_to_live()`](https://niphr.github.io/cs9/reference/compare_staged_to_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In the action of a task that has the table `results` in its tables.
comparison <- compare_staged_to_live(tables$results, "isoyearweek", "n")
check_staged_against_live(comparison, rules = "no_decrease")
} # }
```
