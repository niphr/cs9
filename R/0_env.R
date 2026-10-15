#' Flags/values to be used in the 'dashboards' scene
#' @export config
config <- new.env()

config$is_auto <- FALSE
config$verbose <- FALSE
config$in_parallel <- FALSE
config$tables <- list()

# cs9 runs each plan once since 26.10.12, so plan_attempt_index is always 1.
# The scheduler retries the whole task. The value stays for callers that read
# it, such as norsyss.cs9, which sends an email only when it is 1.
config$plan_attempt_index <- 1

# The state of the task run in progress, or NULL. Task$run() sets it and clears
# it on exit. See R/run_state.R.
config$run <- NULL
# The date of the keyed run in progress that stages nothing, or NULL. A staged
# run keeps its date in config$run$today. run_begin() sets it, and run_today()
# reads it. Task$run() empties it before run_begin() and gives the value of the
# outer run back on exit.
config$run_today <- NULL
# TRUE while Task$run() builds the plans. staged_tbl() then stops, because a
# plan MAY read published tables only.
config$planning <- FALSE
