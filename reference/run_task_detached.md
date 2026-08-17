# Run a cs9 task in a detached process

Starts one task in a process that outlives the R session which started
it, and returns at once. The task's exit code is written to a status
file when it ends.

## Usage

``` r
run_task_detached(
  task_name,
  package_dir = ".",
  ss_prefix = "global$ss",
  run_dir = NULL
)
```

## Arguments

- task_name:

  Character string. Name of the task to run.

- package_dir:

  Character string. Directory of the implementation package, which must
  hold a `DESCRIPTION`. Defaults to `"."`.

- ss_prefix:

  Character string. R expression that resolves to the surveillance
  system object in the child process. Defaults to `"global$ss"`.

- run_dir:

  Character string or `NULL`. Where to write the log, status and lock
  files. `NULL` uses the `CS9_RUN_DIR` environment variable, and
  `~/.cs9/task-runs` when that is unset.

## Value

Invisibly, a list with elements `task`, `log`, `status`, `errors` and
`pid`, all character strings. The `status` and `errors` files do not
exist until the task ends.

## Details

This is the non-interactive counterpart to
[`run_task_sequentially_as_callr_bg_using_load_all()`](https://niphr.github.io/cs9/reference/run_task_sequentially_as_callr_bg_using_load_all.md).
Use that one in Positron, where a person watches a live console. Use
this one when nobody is watching: a script, a cron entry, or an agent
driving a machine over SSH.

The difference is not style.
[`TaskJob`](https://niphr.github.io/cs9/reference/TaskJob.md) passes
`supervise = TRUE`, so callr kills the task when the parent R process
exits, and it streams output through
[`later::later()`](https://later.r-lib.org/reference/later.html), which
needs an event loop that `Rscript` does not drive.

## Waiting

Wait on the status file, never on the log. A log cannot separate
"finished cleanly" from "died at line 4", so a caller reading the tail
of a log is guessing. The status file appears only when the task ends,
and holds the exit code. No file means still running, never done.

## Read the error count as well as the exit code

Exit code 0 does not mean the work succeeded. Measured on 2026-08-17: a
NorSySS import exited 0 with 212 rejected `COPY` statements in its log,
because the PostgreSQL `load_data_infile` method runs `psql` through
[`system2()`](https://rdrr.io/r/base/system2.html) without reading its
exit status, so a rejected `COPY` never reaches R. A status of 0 beside
a non-zero error count is the shape that failure takes.

The count comes from a grep over the log, so it is a heuristic. A count
above zero is worth reading. A count of zero does NOT prove the task did
what it should: only checking the data can tell you that.

## Provenance

The log opens with the task name, the implementation package and its
version, the git branch and commit of the package directory when it is a
repository, and whether that tree was clean. A log that reports only a
duration cannot answer "which code ran" afterwards.

## Examples

``` r
if (FALSE) { # \dontrun{
run <- run_task_detached("my_task", package_dir = ".")
# later, from anywhere:
readLines(run$status)
} # }
```
