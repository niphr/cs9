ss <- cs9::SurveillanceSystem_v9$new()

ss$add_table(
  name_access = c("anon"),
  name_grouping = "example_weather",
  name_variant = NULL,
  field_types =  c(
    "granularity_time" = "TEXT",
    "granularity_geo" = "TEXT",
    "country_iso3" = "TEXT",
    "location_code" = "TEXT",
    "border" = "INTEGER",
    "age" = "TEXT",
    "sex" = "TEXT",

    "isoyear" = "INTEGER",
    "isoweek" = "INTEGER",
    "isoyearweek" = "TEXT",
    "season" = "TEXT",
    "seasonweek" = "DOUBLE",

    "calyear" = "INTEGER",
    "calmonth" = "INTEGER",
    "calyearmonth" = "TEXT",

    "date" = "DATE",

    "tg" = "DOUBLE",
    "tx" = "DOUBLE",
    "tn" = "DOUBLE"
  ),
  keys = c(
    "granularity_time",
    "location_code",
    "date",
    "age",
    "sex"
  ),
  validator_field_types = csdb::validator_field_types_csfmt_rts_data_v1,
  validator_field_contents = csdb::validator_field_contents_csfmt_rts_data_v1
)

# tm_run_task("example_weather_import_data_from_api")
ss$add_task(
  name_grouping = "example_weather",
  name_action = "import_data_from_api",
  name_variant = NULL,
  cores = 1,
  plan_analysis_fn_name = NULL, # "PACKAGE::TASK_NAME_plan_analysis"
  for_each_plan = plnr::expand_list(
    location_code = "county03" # fhidata::norway_locations_names()[granularity_geo %in% c("county")]$location_code
  ),
  for_each_analysis = NULL,
  universal_argset = NULL,
  upsert_at_end_of_each_plan = FALSE,
  insert_at_end_of_each_plan = FALSE,
  action_fn_name = "example_weather_import_data_from_api_action",
  data_selector_fn_name = "example_weather_import_data_from_api_data_selector",
  tables = list(
    # input

    # output
    "anon_example_weather" = ss$tables$anon_example_weather
  )
)

# **** data_selector **** ----
#' example_weather_import_data_from_api (data selector)
#' @param argset Argset
#' @param tables DB tables
#' @export
example_weather_import_data_from_api_data_selector = function(argset, tables){
  if(plnr::is_run_directly()){
    # sc::tm_get_plans_argsets_as_dt("example_weather_import_data_from_api")

    index_plan <- 1

    argset <- ss$shortcut_get_argset("example_weather_import_data_from_api", index_plan = index_plan)
    tables <- ss$shortcut_get_tables("example_weather_import_data_from_api")
  }

  # find the mid lat/long for the specified location_code
  gps <- fhimaps::norway_nuts3_map_b2020_default_dt[location_code == argset$location_code,.(
    lat = mean(lat),
    long = mean(long)
  )]

  # download the forecast for the specified location_code
  d <- httr::GET(glue::glue("https://api.met.no/weatherapi/locationforecast/2.0/classic?lat={gps$lat}&lon={gps$long}"), httr::content_type_xml())
  d <- xml2::read_xml(d$content)

  # The variable returned must be a named list
  retval <- list(
    "data" = d
  )
  retval
}

# **** action **** ----
#' example_weather_import_data_from_api (action)
#' @param data Data
#' @param argset Argset
#' @param tables DB tables
#' @export
example_weather_import_data_from_api_action <- function(data, argset, tables) {
  # tm_run_task("example_weather_import_data_from_api")

  if(plnr::is_run_directly()){
    # sc::tm_get_plans_argsets_as_dt("example_weather_import_data_from_api")

    index_plan <- 1
    index_analysis <- 1

    data <- ss$shortcut_get_data("example_weather_import_data_from_api", index_plan = index_plan)
    argset <- ss$shortcut_get_argset("example_weather_import_data_from_api", index_plan = index_plan, index_analysis = index_analysis)
    tables <- ss$shortcut_get_tables("example_weather_import_data_from_api")
  }

  # code goes here
  # special case that runs before everything
  if(argset$first_analysis == TRUE){

  }

  a <- data$data

  baz <- xml2::xml_find_all(a, ".//maxTemperature")
  res <- vector("list", length = length(baz))
  for (i in seq_along(baz)) {
    parent <- xml2::xml_parent(baz[[i]])
    grandparent <- xml2::xml_parent(parent)
    time_from <- xml2::xml_attr(grandparent, "from")
    time_to <- xml2::xml_attr(grandparent, "to")
    x <- xml2::xml_find_all(parent, ".//minTemperature")
    temp_min <- xml2::xml_attr(x, "value")
    x <- xml2::xml_find_all(parent, ".//maxTemperature")
    temp_max <- xml2::xml_attr(x, "value")
    res[[i]] <- data.frame(
      time_from = as.character(time_from),
      time_to = as.character(time_to),
      tx = as.numeric(temp_max),
      tn = as.numeric(temp_min)
    )
  }
  res <- rbindlist(res)
  res <- res[stringr::str_sub(time_from, 12, 13) %in% c("00", "06", "12", "18")]
  res[, date := as.Date(stringr::str_sub(time_from, 1, 10))]
  res[, N := .N, by = date]
  res <- res[N == 4]
  res <- res[
    ,
    .(
      tg = NA,
      tx = max(tx),
      tn = min(tn)
    ),
    keyby = .(date)
  ]

  # we look at the downloaded data
  print("Data after downloading")
  print(res)

  # we now need to format it
  res[, granularity_time := "date"]
  res[, sex := "total"]
  res[, age := "total"]
  res[, location_code := argset$location_code]
  res[, border := 2020]

  # fill in missing structural variables
  cstidy::set_csfmt_rts_data_v1(res)
  cstidy::remove_class_csfmt_rts_data(res)

  # we look at the downloaded data
  print("Data after missing structural variables filled in")
  print(res)

  # put data in db table
  # tables$TABLE_NAME$insert_data(d)
  tables$anon_example_weather$upsert_data(res)
  # tables$TABLE_NAME$drop_all_rows_and_then_upsert_data(d)

  # special case that runs after everything
  # copy to anon_web?
  if(argset$last_analysis == TRUE){
    # sc::copy_into_new_table_where(
    #   table_from = "anon_X",
    #   table_to = "anon_web_X"
    # )
  }
}

library(data.table)
ss$run_task("example_weather_import_data_from_api")
ss$tables$anon_example_weather$tbl() |> dplyr::collect() |> data.table()
