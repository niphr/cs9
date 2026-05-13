#' TaskJob R6 Class
#'
#' Run a cs9 task in a background R process with live output streaming back to
#' the current R console via [later::later()] polling. Equivalent in spirit to
#' [cs9::run_task_sequentially_as_rstudio_job_using_load_all()] but does not
#' depend on RStudio's job API (which Positron does not implement).
#'
#' Methods:
#'   $start()      spawn background process and begin polling
#'   $wait(t)      block this session, draining the event loop, until done
#'   $is_alive()   TRUE while the background process is running
#'   $status()     summary
#'   $tail(n)      last n lines of the log file (snapshot, not live)
#'   $kill()       terminate the background process
#'
#' @export
TaskJob <- R6::R6Class(
  "TaskJob",
  public = list(
    task_name   = NULL,
    ss_prefix   = NULL,
    script_path = NULL,
    log_path    = NULL,
    started_at  = NULL,
    finished_at = NULL,

    initialize = function(task_name, ss_prefix = "global$ss", log_dir = tempdir(check = TRUE)) {
      self$task_name <- task_name
      self$ss_prefix <- ss_prefix
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      self$script_path <- file.path(log_dir, paste0(task_name, "_", ts, ".R"))
      self$log_path    <- file.path(log_dir, paste0(task_name, "_", ts, ".log"))

      cat(glue::glue(
"cat('\n**devtools::load_all**\n\n'); flush.console()
devtools::load_all('.')
{ss_prefix}$tasks[['{task_name}']]$cores <- 1
cat('\n**run_task**\n\n'); flush.console()
{ss_prefix}$run_task('{task_name}')
"
      ), file = self$script_path)
    },

    start = function() {
      if (!is.null(private$.process) && private$.process$is_alive()) {
        stop("Task is already running. Use $kill() first.")
      }
      self$started_at <- Sys.time()
      private$.log_con <- file(self$log_path, open = "w")
      private$.last_ended_with_newline <- TRUE
      private$.drained_after_exit <- FALSE

      private$.process <- callr::r_bg(
        function(script_path) {
          source(script_path, echo = FALSE)
        },
        args = list(script_path = self$script_path),
        stdout = "|",
        stderr = "2>&1",
        supervise = TRUE,
        user_profile = TRUE,
        wd = getwd()
      )
      private$.schedule_poll()
      message(sprintf("[%s] started (pid=%s). Log: %s",
                      self$task_name, private$.process$get_pid(), self$log_path))
      invisible(self)
    },

    is_alive = function() {
      if (is.null(private$.process)) FALSE else private$.process$is_alive()
    },

    status = function() {
      cat(sprintf("Task:    %s\n", self$task_name))
      cat(sprintf("Started: %s\n",
                  if (!is.null(self$started_at)) format(self$started_at) else "(not started)"))
      cat(sprintf("Alive:   %s\n", self$is_alive()))
      cat(sprintf("Log:     %s\n", self$log_path))
      invisible(self)
    },

    wait = function(timeout_s = 600) {
      if (is.null(private$.process)) return(invisible(self))
      deadline <- Sys.time() + timeout_s
      while (private$.process$is_alive() && Sys.time() < deadline) {
        later::run_now(timeoutSecs = 0.5)
      }
      later::run_now(timeoutSecs = 0.1)
      invisible(self)
    },

    kill = function() {
      if (!is.null(private$.process)) try(private$.process$kill(), silent = TRUE)
      invisible(self)
    },

    tail = function(n = 20) {
      if (!file.exists(self$log_path)) {
        cat("(no log file yet)\n"); return(invisible(self))
      }
      lines <- readLines(self$log_path, warn = FALSE)
      cat(utils::tail(lines, n), sep = "\n"); cat("\n")
      invisible(self)
    }
  ),
  private = list(
    .process                  = NULL,
    .log_con                  = NULL,
    .poll_interval            = 0.5,
    .last_ended_with_newline  = TRUE,
    .drained_after_exit       = FALSE,

    .schedule_poll = function() {
      poller <- function() private$.poll_once()
      later::later(poller, delay = private$.poll_interval)
    },

    .drain = function() {
      out <- private$.process$read_output()
      if (!nzchar(out)) return()
      # Raw log
      cat(out, file = private$.log_con, sep = "")
      flush(private$.log_con)
      # Prefixed console
      prefix <- paste0("[", self$task_name, "] ")
      prefixed <- if (isTRUE(private$.last_ended_with_newline)) paste0(prefix, out) else out
      prefixed <- gsub("\n(?=.)", paste0("\n", prefix), prefixed, perl = TRUE)
      cat(prefixed)
      private$.last_ended_with_newline <- substr(out, nchar(out), nchar(out)) == "\n"
    },

    .poll_once = function() {
      private$.drain()
      if (self$is_alive()) {
        private$.schedule_poll()
      } else if (!isTRUE(private$.drained_after_exit)) {
        private$.drain()
        try(close(private$.log_con), silent = TRUE)
        self$finished_at <- Sys.time()
        exit <- tryCatch(private$.process$get_exit_status(), error = function(e) NA)
        # Ensure we start the finished marker on a fresh line
        if (!isTRUE(private$.last_ended_with_newline)) cat("\n")
        cat(sprintf("[%s] *** finished in %.1fs (exit=%s) ***\n",
                    self$task_name,
                    as.numeric(difftime(self$finished_at, self$started_at, units = "secs")),
                    exit))
        private$.drained_after_exit <- TRUE
      }
    }
  )
)

#' Run a cs9 task in a background process, streaming its output live
#'
#' Positron-friendly counterpart to
#' [cs9::run_task_sequentially_as_rstudio_job_using_load_all()].
#'
#' @param task_name Task name.
#' @param ss_prefix Surveillance-system prefix. Defaults to `"global$ss"`.
#'
#' @return Invisibly returns the [TaskJob] R6 object.
#'
#' @export
run_task_sequentially_as_callr_bg_using_load_all <- function(task_name, ss_prefix = "global$ss") {
  TaskJob$new(task_name, ss_prefix)$start()
}
