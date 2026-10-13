# Run a Task Sequentially as an RStudio Job

Executes a surveillance task as an RStudio job using
devtools::load_all() for package development. This function creates a
temporary R script that loads the package and runs the specified task
sequentially (cores = 1).

## Usage

``` r
run_task_sequentially_as_rstudio_job_using_load_all(
  task_name,
  ss_prefix = "global$ss",
  run_key = NULL
)
```

## Arguments

- task_name:

  Character string specifying the name of the task to run.

- ss_prefix:

  Character string specifying the prefix used to access the surveillance
  system object. Defaults to "global\$ss".

- run_key:

  A string that names the run, or `NULL`. The job passes it to
  `run_task()`. Pass a key to stage the run, as a scheduler does. With
  `NULL` the task writes its tables directly.

## Value

No return value. This function is called for its side effect of
launching an RStudio job that executes the surveillance task.

## Details

Use this function mainly during package development, to test tasks
interactively. It creates a temporary R script that:

- Loads the package using
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html).

- Sets the task to run with single core (cores = 1).

- Executes the task via the surveillance system.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run a task as RStudio job during development
run_task_sequentially_as_rstudio_job_using_load_all("covid_analysis")

# Use custom surveillance system prefix
run_task_sequentially_as_rstudio_job_using_load_all(
  "covid_analysis",
  ss_prefix = "my_ss"
)
} # }
```
