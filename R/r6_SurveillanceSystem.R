#' @title A Surveillance System Object
#'
#' @description
#' Core R6 class for creating and managing disease surveillance systems.
#' This class orchestrates database tables, tasks, and analyses for real-time
#' epidemiological monitoring and public health surveillance.
#'
#' @details
#' SurveillanceSystem_v9 provides infrastructure for:
#' \itemize{
#'   \item Database table management with automated logging
#'   \item Task scheduling and parallel execution
#'   \item Data validation and schema enforcement  
#'   \item Configuration and performance monitoring
#' }
#'
#' The surveillance system follows a structured approach:
#' 1. Define database tables with \code{add_table()}
#' 2. Configure surveillance tasks with \code{add_task()}
#' 3. Execute tasks with \code{run_task()} or external schedulers
#'
#' @examples
#' \dontrun{
#' # Create surveillance system
#' ss <- SurveillanceSystem_v9$new(
#'   name = "covid_surveillance", 
#'   implementation_version = "1.0"
#' )
#' 
#' # Add database table
#' ss$add_table(
#'   name_access = "anon",
#'   name_grouping = "covid",
#'   name_variant = "cases",
#'   field_types = c("date" = "DATE", "cases" = "INTEGER"),
#'   keys = c("date")
#' )
#' 
#' # Add surveillance task
#' ss$add_task(
#'   name_grouping = "covid",
#'   name_action = "import", 
#'   name_variant = "daily_data",
#'   action_fn_name = "import_covid_data",
#'   data_selector_fn_name = "select_covid_sources"
#' )
#' 
#' # Run task
#' ss$run_task("covid_import_daily_data")
#' }
#'
#' @field tables List of database tables managed by the surveillance system
#' @field partitionedtables List of partitioned database tables 
#' @field tasks List of surveillance tasks configured for execution
#' @field name Character string identifying the surveillance system instance
#' @field implementation_version Character string tracking the analytics code version
#' @import R6
#' @export
SurveillanceSystem_v9 <- R6::R6Class(
  "SurveillanceSystem_v9",
  public = list(
    tables = list(),
    partitionedtables = list(),
    tasks = list(),
    name = NULL,
    implementation_version = NULL,
    #' Constructor
    #' @param name A string that the user may choose to use to track performance metrics (runtime and RAM usage)
    #' @param implementation_version A string that the user may choose to use to track performance metrics (runtime and RAM usage)
    initialize = function(
      name = "unspecified",
      implementation_version = "unspecified"
    ) {
      self$name <- name
      self$implementation_version <- implementation_version
    },
    #' @description
    #' Add a table
    #' @param name_access First part of table name, corresponding to the database where it will be stored.
    #' @param name_grouping Second part of table name, corresponding to some sort of grouping.
    #' @param name_variant Final part of table name, corresponding to a distinguishing variant.
    #' @param field_types Named character vector, where the names are the column names, and the values are the column types. Valid types are BOOLEAN, CHARACTER, INTEGER, DOUBLE, DATE, DATETIME
    #' @param keys Character vector, containing the column names that uniquely identify a row of data.
    #' @param indexes Named list, containing indexes.
    #' @param validator_field_types Function corresponding to a validator for the field types.
    #' @param validator_field_contents Function corresponding to a validator for the field contents.
    #' @examples
    #' \dontrun{
    #' global$ss$add_table(
    #'   name_access = c("anon"),
    #'   name_grouping = "example_weather",
    #'   name_variant = "data",
    #'   field_types =  c(
    #'     "granularity_time" = "TEXT",
    #'     "granularity_geo" = "TEXT",
    #'     "country_iso3" = "TEXT",
    #'     "location_code" = "TEXT",
    #'     "border" = "INTEGER",
    #'     "age" = "TEXT",
    #'     "sex" = "TEXT",
    #'
    #'     "isoyear" = "INTEGER",
    #'     "isoweek" = "INTEGER",
    #'     "isoyearweek" = "TEXT",
    #'     "season" = "TEXT",
    #'     "seasonweek" = "DOUBLE",
    #'
    #'     "calyear" = "INTEGER",
    #'     "calmonth" = "INTEGER",
    #'     "calyearmonth" = "TEXT",
    #'
    #'     "date" = "DATE",
    #'
    #'     "temp_max" = "DOUBLE",
    #'     "temp_min" = "DOUBLE",
    #'     "precip" = "DOUBLE"
    #'   ),
    #'   keys = c(
    #'     "granularity_time",
    #'     "location_code",
    #'     "date",
    #'     "age",
    #'     "sex"
    #'   ),
    #'   validator_field_types = csdb::validator_field_types_csfmt_rts_data_v1,
    #'   validator_field_contents = csdb::validator_field_contents_csfmt_rts_data_v1
    #' )
    #' }
    add_table = function(
      name_access,
      name_grouping = NULL,
      name_variant = NULL,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
      ){
      force(name_access)
      force(name_grouping)
      force(name_variant)
      force(field_types)
      force(keys)
      force(indexes)
      force(validator_field_types)
      force(validator_field_contents)

      table_name <- paste0(c(name_access, name_grouping, name_variant), collapse = "_")

      dbtable <- DBTableExtended_v9$new(
        dbconfig = config$dbconfigs[[name_access]],
        table_name = table_name,
        field_types = field_types,
        keys = keys,
        indexes = indexes,
        validator_field_types = validator_field_types,
        validator_field_contents = validator_field_contents
      )

      self$tables[[table_name]] <- dbtable
    },
    #' @description
    #' Add a partitioned table to the surveillance system
    #' @param name_access First part of table name, corresponding to the database where it will be stored
    #' @param name_grouping Second part of table name, corresponding to some sort of grouping
    #' @param name_variant Final part of table name, corresponding to a distinguishing variant
    #' @param name_partitions Character string specifying partition naming scheme
    #' @param column_name_partition Column name used for partitioning
    #' @param value_generator_partition Function to generate partition values
    #' @param field_types Named character vector of column names and types
    #' @param keys Character vector of column names that uniquely identify rows
    #' @param indexes Named list containing index definitions
    #' @param validator_field_types Function to validate field types
    #' @param validator_field_contents Function to validate field contents
    add_partitionedtable = function(
      name_access,
      name_grouping = NULL,
      name_variant = NULL,
      name_partitions = "default",
      column_name_partition = "partition",
      value_generator_partition = NULL,
      field_types,
      keys,
      indexes = NULL,
      validator_field_types = csdb::validator_field_types_blank,
      validator_field_contents = csdb::validator_field_contents_blank
    ){
      force(name_access)
      force(name_grouping)
      force(name_variant)
      force(name_partitions)
      force(column_name_partition)
      force(value_generator_partition)
      force(field_types)
      force(keys)
      force(indexes)
      force(validator_field_types)
      force(validator_field_contents)

      table_name_base <- paste0(c(name_access, name_grouping, name_variant), collapse = "_")

      dbtable <- DBPartitionedTableExtended_v9$new(
        dbconfig = config$dbconfigs[[name_access]],
        table_name_base = table_name_base,
        table_name_partitions = name_partitions,
        column_name_partition = column_name_partition,
        value_generator_partition = value_generator_partition,
        field_types = field_types,
        keys = keys,
        indexes = indexes,
        validator_field_types = validator_field_types,
        validator_field_contents = validator_field_contents
      )

      self$partitionedtables[[table_name_base]] <- dbtable
    },
    #' @description
    #' add_task_from_config
    #' @param name_grouping Name of the task (grouping)
    #' @param name_action Name of the task (action)
    #' @param name_variant Name of the task (variant)
    #' @param cores Number of CPU cores
    #' @param permission A permission R6 instance
    #' @param plan_analysis_fn_name The name of a function that returns a named list \code{list(for_each_plan = list(), for_each_analysis = NULL)}.
    #' @param for_each_plan A list, where each unit corresponds to one data extraction. Generally recommended to use \code{plnr::expand_list}.
    #' @param for_each_analysis A list, where each unit corresponds to one analysis within a plan (data extraction). Generally recommended to use \code{plnr::expand_list}.
    #' @param universal_argset A list, where these argsets are applied to all analyses univerally
    #' @param upsert_at_end_of_each_plan Do you want to upsert your results automatically at the end of each plan?
    #' @param insert_at_end_of_each_plan Do you want to insert your results automatically at the end of each plan?
    #' @param action_fn_name The name of the function that will be called for each analysis with arguments \code{data}, \code{argset}, \code{schema}
    #' @param data_selector_fn_name The name of a function that will be called to obtain the data for each analysis. The function must have the arguments \code{argset}, \code{schema} and must return a named list.
    #' @param tables A named list that maps \code{cs9::config$schemas} for use in \code{action_fn_name} and \code{data_selector_fn_name}
    #' @export
    add_task = function(
      name_grouping = NULL,
      name_action = NULL,
      name_variant = NULL,
      cores = 1,
      permission = NULL,
      plan_analysis_fn_name = NULL,
      for_each_plan = NULL,
      for_each_analysis = NULL,
      universal_argset = NULL,
      upsert_at_end_of_each_plan = FALSE,
      insert_at_end_of_each_plan = FALSE,
      action_fn_name,
      data_selector_fn_name = NULL,
      tables = NULL
      ) {
        force(name_grouping)
        force(name_action)
        force(name_variant)
        force(cores)
        force(permission)
        force(plan_analysis_fn_name)
        force(for_each_plan)
        force(for_each_analysis)
        force(universal_argset)
        force(upsert_at_end_of_each_plan)
        force(insert_at_end_of_each_plan)
        force(action_fn_name)
        force(data_selector_fn_name)

        task <- task_from_config_v9(
          name_grouping = name_grouping,
          name_action = name_action,
          name_variant = name_variant,
          cores = cores,
          permission = permission,
          plan_analysis_fn_name = plan_analysis_fn_name,
          for_each_plan = for_each_plan,
          for_each_analysis = for_each_analysis,
          universal_argset = universal_argset,
          upsert_at_end_of_each_plan = upsert_at_end_of_each_plan,
          insert_at_end_of_each_plan = insert_at_end_of_each_plan,
          action_fn_name = action_fn_name,
          data_selector_fn_name = data_selector_fn_name,
          tables = tables
        )
        task$ss <- self$name
        task$implementation_version <- self$implementation_version
        self$tasks[[task$name]] <- task
    },
    #' @description
    #' Get a surveillance task by name
    #' @param task_name Character string specifying the task name
    get_task = function(task_name){
      retval <- self$tasks[[task_name]]
      retval$update_plans()
      retval
    },
    #' @description
    #' Execute a surveillance task by name
    #' @param task_name Character string specifying the task name to run
    run_task = function(task_name){
      task <- self$get_task(task_name)
      task$run()
    },
    #' @description
    #' Get database tables associated with a task
    #' @param task_name Character string specifying the task name
    shortcut_get_tables = function(task_name){
      self$get_task(task_name)$tables
    },
    #' @description
    #' Get argument set for a specific plan and analysis
    #' @param task_name Character string specifying the task name
    #' @param index_plan Integer specifying which plan to access
    #' @param index_analysis Integer specifying which analysis to access
    shortcut_get_argset = function(task_name, index_plan = 1, index_analysis = 1){
      self$get_task(task_name)$plans[[index_plan]]$get_argset(index_analysis)
    },
    #' @description
    #' Get data for a specific plan
    #' @param task_name Character string specifying the task name
    #' @param index_plan Integer specifying which plan to access
    shortcut_get_data = function(task_name, index_plan = 1){
      self$get_task(task_name)$plans[[index_plan]]$get_data()
    },
    #' @description
    #' Get plans and argsets as a data.table
    #' @param task_name Character string specifying the task name
    shortcut_get_plans_argsets_as_dt = function(task_name){
      plans <- self$get_task(task_name)$plans

      retval <- lapply(plans, function(x) analyses_to_dt(x$analyses))
      retval <- rbindlist(retval)
      setcolorder(retval, c("index_plan", "index_analysis"))
      retval
    },
    #' @description
    #' Get the total number of analyses for a task
    #' @param task_name Character string specifying the task name
    shortcut_get_num_analyses = function(task_name){
      self$get_task(task_name)$num_analyses()
    }
  )
)

analyses_to_dt <- function(analyses) {
  retval <- lapply(analyses, function(x) {
    data.table(t(x$argset))
  })
  retval <- rbindlist(retval)
  # retval[, index_analysis := 1:.N]

  return(retval)
}

#' Run a task sequentially as an RStudio job
#'
#' Description
#' @param task_name Task name
#' @param ss_prefix The prefix that locates the surveillance system
#' @export
run_task_sequentially_as_rstudio_job_using_load_all <- function(
    task_name,
    ss_prefix = "global$ss"
    ){

  tempfile <- fs::path(tempdir(check = T), paste0(task_name, ".R"))

  cat(glue::glue(
    "
        cat('\n**devtools::load_all**\n\n')
        devtools::load_all('.')
        {ss_prefix}$tasks[['{task_name}']]$cores <- 1
        cat('\n**run_task**\n\n')
        {ss_prefix}$run_task('{task_name}')
    "
  ), file = tempfile)
  rstudioapi::jobRunScript(
    path = tempfile,
    name = task_name,
    workingDir = getwd(),
  )
}

generic_data_function_factory_v9 <- function(tables, argset, fn_name) {
  force(tables)
  force(argset)
  force(fn_name)
  function() {
    # for (i in tables) i$connect()
    # on.exit(for (i in tables) i$disconnect())
    fn <- plnr::get_anything(fn_name)
    fn(argset, tables)
  }
}

generic_list_plan_function_factory_v9 <- function(universal_argset,
                                                  tables,
                                                  plan_analysis_fn_name,
                                                  action_fn_name,
                                                  data_selector_fn_name) {
  force(universal_argset)
  force(tables)
  force(plan_analysis_fn_name)
  force(action_fn_name)
  force(data_selector_fn_name)

  function() {
    # for (i in tables) i$connect()
    # on.exit(for (i in tables) i$disconnect())

    fn <- plnr::get_anything(plan_analysis_fn_name)
    plan_analysis <- fn(universal_argset, tables)
    list_plan <- task_from_config_v9_list_plan(
      for_each_plan = plan_analysis$for_each_plan,
      for_each_analysis = plan_analysis$for_each_analysis,
      universal_argset = universal_argset,
      action_fn_name = action_fn_name,
      data_selector_fn_name = data_selector_fn_name,
      tables = tables
    )
    return(list_plan)
  }
}

task_from_config_v9_list_plan <- function(for_each_plan,
                                          for_each_analysis = NULL,
                                          universal_argset = NULL,
                                          action_fn_name,
                                          data_selector_fn_name,
                                          tables) {
  index <- 1
  list_plan <- list()

  for (index_plan in seq_along(for_each_plan)) {
    # create a new plan
    list_plan[[length(list_plan) + 1]] <- plnr::Plan$new()

    # add data
    argset <- c(
      "**universal**" = "*",
      universal_argset,
      "**plan**" = "*",
      for_each_plan[[index_plan]],
      "**automatic**" = "*",
      index = index
    )
    argset$today <- lubridate::today()
    argset$yesterday <- lubridate::today() - 1

    if (!is.null(data_selector_fn_name)) {
      list_plan[[length(list_plan)]]$add_data(
        name = "data__________go_up_one_level",
        fn = generic_data_function_factory_v9(
          tables = tables,
          argset = argset,
          fn_name = data_selector_fn_name
        )
      )
    }

    # add analyses
    if (is.null(for_each_analysis)) {
      for_each_analysis <- list(NULL)
    }
    for (index_analysis in seq_along(for_each_analysis)) {
      # add analysis
      argset <- c(
        "**universal**" = "*",
        universal_argset,
        "**plan**" = "*",
        for_each_plan[[index_plan]],
        "**analysis**" = "*",
        for_each_analysis[[index_analysis]],
        "**automatic**" = "*",
        index = index
      )
      argset$today <- lubridate::today()
      argset$yesterday <- lubridate::today() - 1

      list_plan[[length(list_plan)]]$add_analysis_from_list(
        fn_name = action_fn_name,
        l = list(argset)
      )

      index <- index + 1
    }
  }

  return(list_plan)
}

task_from_config_v9 <- function(
  name_grouping = NULL,
  name_action = NULL,
  name_variant = NULL,
  cores = 1,
  permission = NULL,
  plan_analysis_fn_name = NULL,
  for_each_plan = NULL,
  for_each_analysis = NULL,
  universal_argset = NULL,
  upsert_at_end_of_each_plan = FALSE,
  insert_at_end_of_each_plan = FALSE,
  action_fn_name,
  data_selector_fn_name = NULL,
  tables = NULL
  ) {
  if (is.null(for_each_plan) & is.null(plan_analysis_fn_name)) stop("You must provide at least one of for_each_plan or plan_analysis_fn_name")

  if (!is.null(for_each_plan)) {
    stopifnot(is.list(for_each_plan))
    list_plan <- task_from_config_v9_list_plan(
      for_each_plan = for_each_plan,
      for_each_analysis = for_each_analysis,
      universal_argset = universal_argset,
      action_fn_name = action_fn_name,
      data_selector_fn_name = data_selector_fn_name,
      tables = tables
    )
    update_plans_fn <- NULL
  } else if (!is.null(plan_analysis_fn_name)) {
    list_plan <- NULL
    update_plans_fn <- generic_list_plan_function_factory_v9(
      universal_argset = universal_argset,
      tables = tables,
      plan_analysis_fn_name = plan_analysis_fn_name,
      action_fn_name = action_fn_name,
      data_selector_fn_name = data_selector_fn_name
    )
  }

  task <- Task$new(
    name_grouping = name_grouping,
    name_action = name_action,
    name_variant = name_variant,
    permission = permission,
    plans = list_plan,
    update_plans_fn = update_plans_fn,
    tables = tables,
    cores = cores,
    upsert_at_end_of_each_plan = upsert_at_end_of_each_plan,
    insert_at_end_of_each_plan = insert_at_end_of_each_plan
  )

  return(task)
}


