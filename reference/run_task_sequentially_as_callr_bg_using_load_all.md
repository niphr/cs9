# Run a cs9 task in a background process, streaming its output live

Positron-friendly counterpart to
[`run_task_sequentially_as_rstudio_job_using_load_all()`](https://niphr.github.io/cs9/reference/run_task_sequentially_as_rstudio_job_using_load_all.md).

## Usage

``` r
run_task_sequentially_as_callr_bg_using_load_all(
  task_name,
  ss_prefix = "global$ss"
)
```

## Arguments

- task_name:

  Task name.

- ss_prefix:

  Surveillance-system prefix. Defaults to `"global$ss"`.

## Value

Invisibly returns the
[TaskJob](https://niphr.github.io/cs9/reference/TaskJob.md) R6 object.
