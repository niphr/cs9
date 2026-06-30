# TaskJob R6 Class

Run a cs9 task in a background R process with live output streaming back
to the current R console via
[`later::later()`](https://later.r-lib.org/reference/later.html)
polling. Equivalent in spirit to
[`run_task_sequentially_as_rstudio_job_using_load_all()`](https://niphr.github.io/cs9/reference/run_task_sequentially_as_rstudio_job_using_load_all.md)
but does not depend on RStudio's job API (which Positron does not
implement).

Methods: \$start() spawn background process and begin polling \$wait(t)
block this session, draining the event loop, until done \$is_alive()
TRUE while the background process is running \$status() summary
\$tail(n) last n lines of the log file (snapshot, not live) \$kill()
terminate the background process

## Public fields

- `task_name`:

  Character string. Name of the task being run.

- `ss_prefix`:

  Character string. R expression used to access the surveillance system
  in the child process.

- `script_path`:

  Character string. Path to the temporary R script that is sourced in
  the background process.

- `log_path`:

  Character string. Path to the log file where background process output
  is written.

- `started_at`:

  POSIXct. Time at which `$start()` was called.

- `finished_at`:

  POSIXct. Time at which the background process exited.

## Methods

### Public methods

- [`TaskJob$new()`](#method-TaskJob-initialize)

- [`TaskJob$start()`](#method-TaskJob-start)

- [`TaskJob$is_alive()`](#method-TaskJob-is_alive)

- [`TaskJob$status()`](#method-TaskJob-status)

- [`TaskJob$wait()`](#method-TaskJob-wait)

- [`TaskJob$kill()`](#method-TaskJob-kill)

- [`TaskJob$tail()`](#method-TaskJob-tail)

- [`TaskJob$clone()`](#method-TaskJob-clone)

------------------------------------------------------------------------

### `TaskJob$new()`

Create a new TaskJob.

#### Usage

    TaskJob$new(
      task_name,
      ss_prefix = "global$ss",
      log_dir = tempdir(check = TRUE)
    )

#### Arguments

- `task_name`:

  Character string. Name of the task to run, as registered in the
  surveillance system (e.g. `ss$tasks[["my_task"]]`).

- `ss_prefix`:

  Character string. R expression that evaluates to the surveillance
  system object in the child process. Defaults to `"global$ss"`.

- `log_dir`:

  Character string. Directory where the temporary script and log file
  are written. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

#### Returns

A new `TaskJob` object. Call `$start()` to launch the background
process.

------------------------------------------------------------------------

### `TaskJob$start()`

Spawn the background process and begin polling its output.

#### Usage

    TaskJob$start()

#### Returns

Invisibly returns the `TaskJob` object.

------------------------------------------------------------------------

### `TaskJob$is_alive()`

Check whether the background process is still running.

#### Usage

    TaskJob$is_alive()

#### Returns

Logical. `TRUE` while the background process is alive.

------------------------------------------------------------------------

### `TaskJob$status()`

Print a brief status summary (task name, start time, alive status, log
path).

#### Usage

    TaskJob$status()

#### Returns

Invisibly returns the `TaskJob` object.

------------------------------------------------------------------------

### `TaskJob$wait()`

Block the current session until the background task finishes or the
timeout expires, draining the later event loop while waiting.

#### Usage

    TaskJob$wait(timeout_s = 600)

#### Arguments

- `timeout_s`:

  Numeric. Maximum number of seconds to wait. Defaults to `600` (10
  minutes).

#### Returns

Invisibly returns the `TaskJob` object.

------------------------------------------------------------------------

### `TaskJob$kill()`

Terminate the background process.

#### Usage

    TaskJob$kill()

#### Returns

Invisibly returns the `TaskJob` object.

------------------------------------------------------------------------

### `TaskJob$tail()`

Print the last `n` lines of the task log file (snapshot, not live).

#### Usage

    TaskJob$tail(n = 20)

#### Arguments

- `n`:

  Integer. Number of trailing lines to show. Defaults to `20`.

#### Returns

Invisibly returns the `TaskJob` object.

------------------------------------------------------------------------

### `TaskJob$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskJob$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
job <- TaskJob$new("my_task")
job$start()
job$status()
job$wait(timeout_s = 120)
job$tail(n = 30)
} # }
```
