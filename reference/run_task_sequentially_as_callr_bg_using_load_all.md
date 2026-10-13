# Run a cs9 task in a background process, streaming its output live

Positron-friendly counterpart to
[`run_task_sequentially_as_rstudio_job_using_load_all()`](https://niphr.github.io/cs9/reference/run_task_sequentially_as_rstudio_job_using_load_all.md).

## Usage

``` r
run_task_sequentially_as_callr_bg_using_load_all(
  task_name,
  ss_prefix = "global$ss",
  run_key = NULL
)
```

## Arguments

- task_name:

  Character string. Name of the task to run.

- ss_prefix:

  Character string. R expression that resolves to the surveillance
  system object in the child process. Defaults to `"global$ss"`.

- run_key:

  A string that names the run, or `NULL`. The child process passes it to
  `run_task()`. Pass a key to stage the run, as a scheduler does. With
  `NULL` the task writes its tables directly.

## Value

Invisibly returns the
[TaskJob](https://niphr.github.io/cs9/reference/TaskJob.md) R6 object
(already started).

## Examples

``` r
if (FALSE) { # \dontrun{
job <- run_task_sequentially_as_callr_bg_using_load_all("my_task")
job$wait()
} # }
```
