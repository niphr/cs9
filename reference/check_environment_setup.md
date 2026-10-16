# Check Environment Setup

Diagnostic function that checks the CS9 environment configuration. This
function checks the required environment variables and the database
connectivity. It reports detailed feedback that helps you troubleshoot
configuration problems.

## Usage

``` r
check_environment_setup(verbose = TRUE, use_startup_message = FALSE)
```

## Arguments

- verbose:

  Logical. If TRUE (default), prints detailed diagnostic output. If
  FALSE, runs the check silently and only returns the result object.

- use_startup_message:

  Logical. If TRUE, uses packageStartupMessage() for output
  (suppressible). If FALSE (default), uses cat() for console output.

## Value

A list containing environment setup status and recommendations:

- status: "ok", "warning", or "error".

- issues: Character vector of identified problems.

- recommendations: Character vector of suggested fixes.

## Details

CS9 needs specific environment variables for database connectivity and
configuration. This function checks for:

- Required environment variables (CS9_DBCONFIG_ACCESS,
  CS9_DBCONFIG_DRIVER, etc.).

- Database configuration availability.

- Database table initialization status.

When you install CS9 from CRAN without a database configuration, the
package loads with limited functionality. Use this function to identify
what you must configure for full database-driven surveillance
functionality.

## See also

The installation vignette:
[`vignette("installation", package = "cs9")`](https://niphr.github.io/cs9/articles/installation.md).

## Examples

``` r
# Check environment setup with verbose output
check_environment_setup()
#> CS9 Environment Check Results:
#> Status:  error 
#> 
#> Issues found:
#>  -  Missing required environment variables: CS9_AUTO, CS9_PATH, CS9_DBCONFIG_ACCESS, CS9_DBCONFIG_DRIVER 
#>  -  Missing server-specific variables: CS9_DBCONFIG_PORT, CS9_DBCONFIG_SERVER 
#>  -  No database configurations available 
#>  -  No configuration tables initialized 
#> 
#> Recommendations:
#>  -  Set required environment variables before loading CS9 
#>  -  Refer to CS9 documentation for environment setup instructions 
#>  -  Every driver except SQLite connects to a database server 
#>  -  Set CS9_DBCONFIG_SERVER and CS9_DBCONFIG_PORT, or use CS9_DBCONFIG_DRIVER=SQLite 
#>  -  Verify CS9_DBCONFIG_ACCESS is properly set 
#>  -  Check database connectivity and permissions 
#> 

# Check silently and examine results
result <- check_environment_setup(verbose = FALSE)
if(result$status != "ok") {
  cat("Issues found:", result$issues, "\n")
  cat("Recommendations:", result$recommendations, "\n")
}
#> Issues found: Missing required environment variables: CS9_AUTO, CS9_PATH, CS9_DBCONFIG_ACCESS, CS9_DBCONFIG_DRIVER Missing server-specific variables: CS9_DBCONFIG_PORT, CS9_DBCONFIG_SERVER No database configurations available No configuration tables initialized 
#> Recommendations: Set required environment variables before loading CS9 Refer to CS9 documentation for environment setup instructions Every driver except SQLite connects to a database server Set CS9_DBCONFIG_SERVER and CS9_DBCONFIG_PORT, or use CS9_DBCONFIG_DRIVER=SQLite Verify CS9_DBCONFIG_ACCESS is properly set Check database connectivity and permissions 
```
