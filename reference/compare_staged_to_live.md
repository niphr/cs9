# Compare the staged rows of a task run with the live rows

`compare_staged_to_live()` reads the rows that the task run in progress
staged in `table`, and the live rows of `table`. It returns the values
of one column of both, side by side, for each key and partition.

## Usage

``` r
compare_staged_to_live(table, key, value, filter = NULL)
```

## Arguments

- table:

  A `DBTableExtended_v9` or a `DBPartitionedTableExtended_v9` of the
  task run, as `tables$<name>` in the action of the task.

- key:

  A character vector of column names. The values of these columns
  identify a key. The vector MUST NOT contain `partition`, `staged`,
  `live` or `problem`. The result and
  [`check_staged_against_live()`](https://niphr.github.io/cs9/reference/check_staged_against_live.md)
  use these names.

- value:

  The name of one column. Its values MUST be numeric.

- filter:

  `NULL`, or a function that takes a lazy table and returns a lazy
  table. The function applies it to the staged rows and to each live
  table before it collects them.

## Value

A data.table with the `key` columns and these columns:

- `partition`: character. The name of the partition, as in
  `names(table$tables)`. `NA` for a table without partitions.

- `staged`: numeric. The staged value, or `NA` where no staged row holds
  the key in that partition.

- `live`: numeric. The live value, or `NA` where no live row holds the
  key in that partition.

The rows are sorted by the `key` columns, `partition`, `staged` and
`live`, with `NA` first.

## Details

The function reads the staged rows with `table$staged_tbl()`. It reads
the live rows with `tbl()` of `table`, or of each partition table of a
partitioned table. It skips a live table that does not exist. It
collects each side with
[`csdb::collect_and_sort_by_pk()`](https://niphr.github.io/csdb/reference/collect_and_sort_by_pk.html).

The function works only during a task run of a task that has `table` in
its `tables`. Outside a run it stops with the error of `staged_tbl()`.

A row of the result holds at most one staged value and one live value.
Several rows of one side can share a key in one partition, for example
when `key` is not the whole primary key. The function then keeps each
distinct value. The result then has one row for each combination of a
distinct staged value and a distinct live value of that key and
partition.
[`check_staged_against_live()`](https://niphr.github.io/cs9/reference/check_staged_against_live.md)
then sees the disagreement.

The function reads a value of `NA` as no value.

## See also

[`check_staged_against_live()`](https://niphr.github.io/cs9/reference/check_staged_against_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In the action of a task that has the table `results` in its tables.
weeks <- c("2026-45", "2026-46")
comparison <- compare_staged_to_live(
  tables$results,
  key = "isoyearweek",
  value = "n",
  filter = function(x) dplyr::filter(x, isoyearweek %in% !!weeks)
)
check_staged_against_live(comparison)
} # }
```
