progressr_handler <- function(interval = 10, clear = FALSE, ...) {
  reporter <- local({
    start_time <- NULL
    list(
      initiate = function(config, state, ...) {
        if (!state$enabled || config$times <= 2L) return()
        start_time <<- Sys.time()
        return(cat("\n"))
      },

      update = function(config, state, progression, ...) {
        time_so_far <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)
        total_time <- round(config$max_steps * time_so_far / state$step,1)
        remaining_time <- total_time - time_so_far
        f_perc <- formatC(floor(100*state$step/config$max_steps),width = 3, digits = 0, format="f")

        f_remaining_time <- formatC(remaining_time, width = 3, digits = 0, format="f")
        f_time_so_far <- formatC(time_so_far, width = 3, digits = 0, format="f")
        f_total_time <- formatC(total_time, width = 5, digits = 1, format="f")

        f_current_step <- formatC(state$step, width = log10(config$max_steps)+1, digits = 0, format="f")

        return(cat(glue::glue("{f_current_step} / {config$max_steps} = {f_perc}%     {f_remaining_time}m -> {f_time_so_far}m = {f_total_time}m"), "\n"))
      },

      finish = function(...) {
        # cat("finished...")
      }
    )
  })

  return(progressr::make_progression_handler("cs9", reporter, intrusiveness = 1, target = "terminal", interval = interval, clear = clear, ...))
}
