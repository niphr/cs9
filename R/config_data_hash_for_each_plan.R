update_config_data_hash_for_each_plan <- function(task, index_plan, element_tag, element_hash = NULL, all_hash = NULL, date = NULL, datetime = NULL) {
  on.exit(config$tables$config_data_hash_for_each_plan$disconnect())

  if (!is.null(datetime)) datetime <- as.character(datetime)

  if (is.null(date) & is.null(datetime)) {
    date <- lubridate::today()
    datetime <- cstime::now_c()
  }
  if (is.null(date) & !is.null(datetime)) {
    date <- stringr::str_sub(datetime, 1, 10)
  }
  if (!is.null(date) & is.null(datetime)) {
    datetime <- paste0(date, " 00:01:00")
  }

  if(is.null(element_tag)) element_tag <- "NULL"
  if(is.null(element_hash)) element_hash <- "NULL"
  if(is.null(all_hash)) all_hash <- "NULL"

  to_upload <- data.table(
    "task" = task,
    "index_plan" = index_plan,
    "element_tag" = element_tag,
    "date" = date,
    "datetime" = datetime,
    "element_hash" = element_hash,
    "all_hash" = all_hash
  )
  config$tables$config_data_hash_for_each_plan$upsert_data(to_upload)
}

#' Get Configuration Data Hash for Each Plan
#'
#' Retrieves data hash configuration entries from the database table for 
#' tracking data changes across surveillance task plans and elements.
#'
#' @param task Character string specifying the task name to filter by. If NULL, 
#'   returns data for all tasks.
#' @param index_plan Integer specifying the plan index to filter by. If NULL,
#'   returns data for all plans.
#' @param element_tag Character string specifying the element tag to filter by. 
#'   If NULL, returns data for all elements.
#'
#' @return A data.table containing the filtered hash configuration entries with
#'   columns: task, index_plan, element_tag, date, datetime, element_hash, all_hash
#'
#' @examples
#' \dontrun{
#' # Get all hash data for a specific task
#' get_config_data_hash_for_each_plan(task = "covid_analysis")
#' 
#' # Get hash data for specific task and plan
#' get_config_data_hash_for_each_plan(task = "covid_analysis", index_plan = 1)
#' }
#'
#' @export
get_config_data_hash_for_each_plan <- function(task = NULL, index_plan = NULL, element_tag = NULL) {
  on.exit(config$tables$config_data_hash_for_each_plan$disconnect())

  if (!is.null(task)) {
    temp <- config$tables$config_data_hash_for_each_plan$tbl() %>%
      dplyr::filter(task == !!task) %>%
      dplyr::collect() %>%
      as.data.table()
  } else {
    temp <- config$tables$config_data_hash_for_each_plan$tbl() %>%
      dplyr::collect() %>%
      as.data.table()
  }
  if (!is.null(index_plan)) {
    x_index_plan <- index_plan
    temp <- temp[index_plan == x_index_plan]
  }
  if (!is.null(element_tag)) {
    x_element_tag <- element_tag
    temp <- temp[element_tag == x_element_tag]
  }

  return(temp)
}

# this uses get_config_data_hash_for_each_plan to put it into plnr format
# i.e. hash$last_run and hash$last_run_elements$blah
get_last_run_data_hash_split_into_plnr_format <- function(task, index_plan, expected_element_tags = NULL){
  datetime <- NULL

  hash <- get_config_data_hash_for_each_plan(task = task, index_plan = index_plan)
  retval <- list()
  if(nrow(hash)==0){
    retval$last_run <- as.character(stats::runif(1))
    retval$last_run_elements <- list()
  } else {
    hash <- hash[datetime==max(datetime)]
    retval$last_run <- hash$all_hash[1]
    retval$last_run_elements <- list()
    for(i in seq_len(nrow(hash))){
      retval$last_run_elements[[hash$element_tag[i]]] <- hash$element_hash[i]
    }
  }
  # if provided element names that we expect, check to make sure that they exist
  # if they don't exist, set to random
  for(i in expected_element_tags){
    if(!i %in% names(retval$last_run_elements)){
      retval$last_run_elements[[i]] <- as.character(stats::runif(1))
    }
  }

  return(retval)
}

