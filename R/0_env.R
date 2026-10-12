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
