# Run a Task Sequentially as an RStudio Job

Executes a surveillance task as an RStudio job using
devtools::load_all() for package development. This function creates a
temporary R script that loads the package and runs the specified task
sequentially (cores = 1).

## Usage

``` r
run_task_sequentially_as_rstudio_job_using_load_all(
  task_name,
  ss_prefix = "global$ss"
)
```

## Arguments

- task_name:

  Character string specifying the name of the task to run

- ss_prefix:

  Character string specifying the prefix used to access the surveillance
  system object. Defaults to "global\$ss"

## Value

No return value. This function is called for its side effect of
launching an RStudio job that executes the surveillance task.

## Details

This function is primarily used during package development to test tasks
interactively. It creates a temporary R script that:

- Loads the package using
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)

- Sets the task to run with single core (cores = 1)

- Executes the task via the surveillance system

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
