#' cs9: A Framework for Real-Time Analysis and Disease Surveillance
#'
#' @description
#' CS9 (Core Surveillance 9) is a comprehensive framework for building real-time 
#' disease surveillance systems. It provides R6-based infrastructure for 
#' database-driven surveillance tasks, with support for parallel processing, 
#' data validation, and automated pipeline execution.
#'
#' @details
#' ## Package Overview
#' 
#' CS9 enables systematic development of surveillance systems with:
#' \itemize{
#'   \item **Database Integration**: Extended database table classes with automatic 
#'         logging and metadata tracking
#'   \item **Task Orchestration**: R6 classes for managing surveillance tasks with 
#'         parallel execution capabilities
#'   \item **Data Validation**: Comprehensive schema system with field type and 
#'         content validation
#'   \item **Graceful Degradation**: Package works in limited mode without database 
#'         configuration, suitable for CRAN distribution
#' }
#'
#' ## Getting Started
#'
#' ### CRAN Installation (Limited Functionality)
#' When installed from CRAN without database configuration, CS9 provides:
#' \itemize{
#'   \item Documentation and examples
#'   \item Environment diagnostic tools
#'   \item Code templates and development aids
#' }
#'
#' Use \code{\link{check_environment_setup}()} to diagnose configuration requirements.
#'
#' ### Full Installation (Database-Driven)
#' For complete surveillance functionality:
#' 1. Set up PostgreSQL or MariaDB database
#' 2. Configure environment variables (see \code{vignette("installation")})
#' 3. Create \code{\link{SurveillanceSystem_v9}} instance
#' 4. Define database tables and surveillance tasks
#'
#' ## Key Components
#'
#' ### Core Classes
#' \describe{
#'   \item{\code{\link{SurveillanceSystem_v9}}}{Main class orchestrating the surveillance system}
#'   \item{\code{DBTableExtended_v9}}{Enhanced database table management}
#'   \item{\code{DBPartitionedTableExtended_v9}}{Multi-table partitioning support}
#'   \item{\code{Task}}{Individual surveillance task management}
#' }
#'
#' ### Key Functions
#' \describe{
#'   \item{\code{\link{check_environment_setup}}}{Diagnostic tool for configuration}
#'   \item{\code{\link{get_config_log}}}{Retrieve task execution logs}
#'   \item{\code{\link{update_config_log}}}{Log surveillance system events}
#'   \item{\code{\link{mandatory_db_filter}}}{Standardized data filtering}
#' }
#'
#' ## Architecture
#'
#' CS9 follows a structured approach:
#' \enumerate{
#'   \item **Schema Definition**: Define database table structures and validation
#'   \item **Task Configuration**: Specify data selectors and analysis functions
#'   \item **Execution**: Run tasks with parallel processing and error handling
#'   \item **Monitoring**: Track performance and log execution details
#' }
#'
#' Tasks are organized into plans (data processing units) containing analyses.
#' Each task has:
#' \itemize{
#'   \item Data selector function (extracts data for analysis)
#'   \item Action function (core analysis logic)
#'   \item Configuration parameters (execution settings)
#'   \item Schema mappings (database table specifications)
#' }
#'
#' ## Examples
#'
#' ### Basic Usage (Works in CRAN Environment)
#' ```r
#' # Check if CS9 is properly configured
#' cs9::check_environment_setup()
#' 
#' # Access package configuration
#' cs9::config$is_auto
#' ```
#'
#' ### Full Surveillance System (Requires Database)
#' ```r
#' # Create surveillance system
#' ss <- cs9::SurveillanceSystem_v9$new(
#'   name = "disease_surveillance",
#'   implementation_version = "1.0.0"
#' )
#' 
#' # Add database table
#' ss$add_table(
#'   name_access = "anon",
#'   name_grouping = "disease",
#'   name_variant = "weekly_reports",
#'   field_types = c(
#'     "date" = "DATE",
#'     "location_code" = "TEXT", 
#'     "cases" = "INTEGER"
#'   ),
#'   keys = c("date", "location_code")
#' )
#' 
#' # Add surveillance task
#' ss$add_task(
#'   name_grouping = "disease",
#'   name_action = "calculate",
#'   name_variant = "weekly_summary", 
#'   action_fn_name = "calculate_weekly_cases",
#'   data_selector_fn_name = "get_daily_case_data"
#' )
#' ```
#'
#' ## Infrastructure Requirements
#' 
#' For full functionality, CS9 requires:
#' \itemize{
#'   \item **Database**: PostgreSQL (recommended) or MariaDB
#'   \item **Environment Variables**: Database connection configuration
#'   \item **R Packages**: Dependencies listed in DESCRIPTION
#' }
#' 
#' See \code{vignette("installation")} for detailed setup instructions.
#'
#' @seealso 
#' Useful links:
#' \itemize{
#'   \item Website: \url{https://www.csids.no/cs9/}
#'   \item GitHub: \url{https://github.com/csids/cs9}
#'   \item Example Implementation: \url{https://github.com/csids/cs9example}
#'   \item Docker Setup: \url{https://github.com/csids/docker-examples-csids}
#' }
#'
#' @author Richard Aubrey White \email{hello@rwhite.no}
#' @author CSIDS \email{hello@csids.no}
#'
#' @name cs9-package
#' @aliases cs9
"_PACKAGE"