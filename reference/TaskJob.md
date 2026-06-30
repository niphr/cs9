# TaskJob R6 Class

Run a cs9 task in a background R process with live output streaming back
to the current R console via
[`later::later()`](https://later.r-lib.org/reference/later.html)
polling. Equivalent in spirit to
[`run_task_sequentially_as_rstudio_job_using_load_all()`](https://niphr.github.io/cs9/reference/run_task_sequentially_as_rstudio_job_using_load_all.md)
but does not depend on RStudio's job API (which Positron does not
implement).

## Details

Methods: \$start() spawn background process and begin polling \$wait(t)
block this session, draining the event loop, until done \$is_alive()
TRUE while the background process is running \$status() summary
\$tail(n) last n lines of the log file (snapshot, not live) \$kill()
terminate the background process

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

#### Usage

    TaskJob$new(
      task_name,
      ss_prefix = "global$ss",
      log_dir = tempdir(check = TRUE)
    )

------------------------------------------------------------------------

### `TaskJob$start()`

#### Usage

    TaskJob$start()

------------------------------------------------------------------------

### `TaskJob$is_alive()`

#### Usage

    TaskJob$is_alive()

------------------------------------------------------------------------

### `TaskJob$status()`

#### Usage

    TaskJob$status()

------------------------------------------------------------------------

### `TaskJob$wait()`

#### Usage

    TaskJob$wait(timeout_s = 600)

------------------------------------------------------------------------

### `TaskJob$kill()`

#### Usage

    TaskJob$kill()

------------------------------------------------------------------------

### `TaskJob$tail()`

#### Usage

    TaskJob$tail(n = 20)

------------------------------------------------------------------------

### `TaskJob$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskJob$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
