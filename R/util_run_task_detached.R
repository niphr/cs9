#' Run a cs9 task in a detached process
#'
#' Starts one task in a process that outlives the R session which started it,
#' and returns at once. The task's exit code is written to a status file when it
#' ends.
#'
#' This is the non-interactive counterpart to
#' \code{\link{run_task_sequentially_as_callr_bg_using_load_all}()}. Use that one
#' in Positron, where a person watches a live console. Use this one when nobody
#' is watching: a script, a cron entry, or an agent driving a machine over SSH.
#'
#' The difference is not style. \code{\link{TaskJob}} passes
#' \code{supervise = TRUE}, so \pkg{callr} kills the task when the parent R
#' process exits, and it streams output through \code{later::later()}, which
#' needs an event loop that \command{Rscript} does not drive.
#'
#' @section Waiting:
#' Wait on the status file, never on the log. A log cannot separate "finished
#' cleanly" from "died at line 4", so a caller reading the tail of a log is
#' guessing. The status file appears only when the task ends, and holds the exit
#' code. No file means still running, never done.
#'
#' @section Read the error count as well as the exit code:
#' Exit code 0 does not mean the work succeeded. Measured on 2026-08-17: a
#' NorSySS import exited 0 with 212 rejected \command{COPY} statements in its
#' log. The csdb of that date did not read the exit status of its load client,
#' so a rejected \command{COPY} never reached R. Since csdb 2026.10.3 a refused
#' load stops with an R error. A task that catches an error and continues still
#' exits 0, so a status of 0 beside a non-zero error count means: read the log.
#'
#' The count comes from a grep over the log, so it is a heuristic. A count above
#' zero is worth reading. A count of zero does NOT prove the task did what it
#' should: only checking the data can tell you that.
#'
#' @section Provenance:
#' The log opens with the task name, the implementation package and its version,
#' the git branch and commit of the package directory when it is a repository,
#' and whether that tree was clean. A log that reports only a duration cannot
#' answer "which code ran" afterwards.
#'
#' @param task_name Character string. Name of the task to run.
#' @param package_dir Character string. Directory of the implementation package,
#'   which must hold a \file{DESCRIPTION}. Defaults to \code{"."}.
#' @param ss_prefix Character string. R expression that resolves to the
#'   surveillance system object in the child process. Defaults to
#'   \code{"global$ss"}.
#' @param run_dir Character string or \code{NULL}. Where to write the log,
#'   status and lock files. \code{NULL} uses the \env{CS9_RUN_DIR} environment
#'   variable, and \file{~/.cs9/task-runs} when that is unset.
#' @param run_key A string that names the run, or \code{NULL}. The child
#'   process passes it to \code{run_task()}. Pass a key to stage the run, as a
#'   scheduler does. With \code{NULL} the task writes its tables directly.
#'   See \code{vignette("rebuilding-a-table-safely")}.
#'
#' @return Invisibly, a list with elements \code{task}, \code{log},
#'   \code{status}, \code{errors} and \code{pid}, all character strings. The
#'   \code{status} and \code{errors} files do not exist until the task ends.
#'
#' @examples
#' \dontrun{
#' run <- run_task_detached("my_task", package_dir = ".")
#' # later, from anywhere:
#' readLines(run$status)
#' }
#'
#' @export
run_task_detached <- function(
  task_name,
  package_dir = ".",
  ss_prefix = "global$ss",
  run_dir = NULL,
  run_key = NULL
) {
  # A wrong key stops here, in the calling session, and not in the child.
  if (!is.null(run_key)) {
    run_check_key(run_key)
  }
  script <- system.file("scripts", "run-task.sh", package = "cs9")
  if (!nzchar(script)) {
    stop("run-task.sh is missing from the installed cs9.", call. = FALSE)
  }

  # shQuote() puts the path in single quotes, and bash does not expand a tilde
  # there. Expand it here, or "~/pkg" reaches the script as a literal directory
  # that does not exist.
  args <- c(
    shQuote(script),
    "--dir",
    shQuote(path.expand(package_dir)),
    "--ss",
    shQuote(ss_prefix)
  )
  if (!is.null(run_dir)) {
    args <- c(args, "--runs", shQuote(path.expand(run_dir)))
  }
  # The Rscript of this R, by its full path. R CMD check refuses an Rscript
  # found on PATH, and the child then runs the same R version as the caller.
  args <- c(args, "--rscript", shQuote(file.path(R.home("bin"), "Rscript")))
  # The key reaches the child R process as a command-line argument.
  if (!is.null(run_key)) {
    args <- c(args, "--key", shQuote(run_key))
  }
  args <- c(args, shQuote(task_name))

  out <- suppressWarnings(
    system2("bash", args, stdout = TRUE, stderr = TRUE)
  )
  code <- attr(out, "status")
  if (!is.null(code) && code != 0) {
    stop(paste(c(paste0("run-task.sh exited ", code, ":"), out), collapse = "\n"), call. = FALSE)
  }

  # The script prints exactly TASK, LOG, STATUS, ERRORS and PID, one per line.
  keep <- grepl("^(TASK|LOG|STATUS|ERRORS|PID)=", out)
  fields <- strsplit(out[keep], "=", fixed = TRUE)
  retval <- lapply(fields, function(x) paste(x[-1], collapse = "="))
  names(retval) <- tolower(vapply(fields, `[`, character(1), 1L))
  message("Task ", task_name, " started. Status file: ", retval$status)
  return(invisible(retval))
}
