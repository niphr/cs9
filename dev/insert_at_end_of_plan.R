.libPaths("~/R/x86_64-pc-linux-gnu-library/4.1")
library(data.table)
system("/bin/authenticate.sh")


# anon_test ----
cs9::add_schema_v8(
  name_access = c("anon"),
  name_grouping = "test",
  name_variant = NULL,
  db_configs = cs9::config$db_configs,
  field_types =  c(
    "uuid" = "INTEGER",
    "n" = "INTEGER"
  ),
  keys = c(
    "uuid"
  ),
  censors = list(
    anon = list(
    )
  ),
  info = "This db table is used for..."
)


# TASK_NAME ----
# tm_run_task("test")
cs9::add_task_from_config_v8(
  name_grouping = "test",
  name_action = NULL,
  name_variant = NULL,
  cores = 1,
  permission = NULL,
  plan_analysis_fn_name = NULL, # "PACKAGE::TASK_NAME_plan_analysis"
  for_each_plan = plnr::expand_list(
    x = 1:1000
  ),
  for_each_analysis = NULL,
  universal_argset = NULL,
  upsert_at_end_of_each_plan = FALSE,
  insert_at_end_of_each_plan = TRUE,
  action_fn_name = "test_action",
  data_selector_fn_name = "test_data_selector",
  schema = list(
    # input

    # output
    "output" = cs9::config$schemas$anon_test
  ),
  info = "This task does..."
)


# **** data_selector **** ----
#' TASK_NAME (data selector)
#' @param argset Argset
#' @param schema DB Schema
#' @export
test_data_selector = function(argset, schema){
  if(plnr::is_run_directly()){
    # cs9::tm_get_plans_argsets_as_dt("TASK_NAME")

    index_plan <- 1

    argset <- cs9::tm_get_argset("test", index_plan = index_plan)
    schema <- cs9::tm_get_schema("test")
  }

  # The database schemas can be accessed here
  d = data.table(uuid = argset$x)
  d$n = 1

  # The variable returned must be a named list
  retval <- list(
    "data" = d
  )
  retval
}


# **** action **** ----
#' test (action)
#' @param data Data
#' @param argset Argset
#' @param schema DB Schema
#' @export
test_action <- function(data, argset, schema) {
  # tm_run_task("TASK_NAME")

  if(plnr::is_run_directly()){
    # cs9::tm_get_plans_argsets_as_dt("TASK_NAME")

    index_plan <- 1
    index_analysis <- 1

    data <- cs9::tm_get_data("test", index_plan = index_plan)
    argset <- cs9::tm_get_argset("test", index_plan = index_plan, index_analysis = index_analysis)
    schema <- cs9::tm_get_schema("test")
  }

  # code goes here
  # special case that runs before everything
  if(argset$first_analysis == TRUE){
    schema$output$drop_all_rows()
  }

  return(data$data)
}

cs9::tm_run_task("test")




d = data.table(uuid = 1:10000)
d$n = 1
d[, n:=as.character(n)]
d[10, n := "a"]

cs9::config$schemas$anon_test$tbl()
cs9::tbl("anon_test")
# setkey(d, uuid)
#setorder(d, -uuid)
a <- Sys.time()
cs9::config$schemas$anon_test$drop_all_rows_and_then_insert_data(d)
b <- Sys.time()

b - a

cs9::config$schemas$anon_test$tbl() |>dplyr::collect() |> setDT() -> f


cs9::config$schemas$anon_test$tbl()
cs9::tbl("anon_test")
cs9::print_tables()

cs9::config$schemas$anon_test$tbl() |> dplyr::summarize(n()) |> dplyr::collect()



sql <- glue::glue("SELECT * INTO sykdomspulsen_interactive_restr.dbo.test FROM sykdomspulsen_interactive_anon.dbo.anon_test")
DBI::dbExecute(pools$no_db, sql)

sql <- glue::glue("SELECT TOP 1 * FROM [sykdomspulsen_interactive_anon].[dbo].[anon_test]")
DBI::dbGetQuery(pool, sql)


sql <- glue::glue("SELECT TOP 1 * FROM [sykdomspulsen_interactive_anon].[dbo].[anon_test]")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)


sql <- glue::glue("SELECT TOP 1 * FROM [anon_test]")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)

sql <- glue::glue("SELECT TOP 1 * FROM [sykdomspulsen_interactive_anon].[].[anon_test]")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)

sql <- glue::glue("ALTER SCHEMA dbo TRANSFER [FHI\\AK_Sykdomspulsen].[datar_weather]")
DBI::dbGetQuery(get_db_connection(), sql)

sql <- glue::glue("SELECT TOP 1 * FROM [FHI\\AK_Sykdomspulsen].[datar_weather]")
DBI::dbGetQuery(get_db_connection(), sql)


try(DBI::dbRemoveTable(conn, name = table_to), TRUE)

sql <- glue::glue("EXEC sp_rename '{temp_name}', '{table_to}'")
DBI::dbExecute(conn, sql)
t1 <- Sys.time()
dif <- round(as.numeric(difftime(t1, t0, units = "secs")), 1)
if(config$verbose) message(glue::glue("Copied rows in {dif} seconds from {table_from} to {table_to}"))


DBI::dbListTables(pools$`dm-prod/sykdomspulsen_interactive_anon`)



sql <- glue::glue("
CREATE TABLE dbo.anon_persons (
    PersonID int,
    LastName varchar(255),
    FirstName varchar(255),
    Address varchar(255),
    City varchar(255)
);")
DBI::dbExecute(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)
cs9::tbl("anon_persons")
cs9::print_tables()

sql <- glue::glue("
CREATE TABLE [FHI\\AK_Sykdomspulsen].anon_persons (
    PersonID int,
    LastName varchar(255),
    FirstName varchar(255),
    Address varchar(255),
    City varchar(255)
);")
DBI::dbExecute(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)
cs9::tbl("anon_persons")
cs9::print_tables()


sql <- glue::glue("SELECT SUSER_NAME() ")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)

sql <- glue::glue("SELECT SCHEMA_NAME() ")
DBI::dbGetQuery(pools$`[sykdomspulsen_interactive_anon].[dbo]`, sql)

sql <- glue::glue("SELECT SCHEMA_NAME() ")
DBI::dbGetQuery(pools$`[sykdomspulsen_interactive_restr].[dbo]`, sql)

sql <- glue::glue("SELECT name, has_dbaccess(name)
FROM sys.databases")
DBI::dbGetQuery(pools$`dm-prod/sykdomspulsen_interactive_anon`, sql)
