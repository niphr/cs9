# The middle plans of a parallel task run. Task$run() forks its workers once.
# Each worker takes the next plan from a queue that the workers share, runs it
# and records its result, until no plan is left. See
# private$run_parallel_plans() in R/r6_Task.R.
#
# The queue is a directory in tempdir(). The file "next" holds the number of
# plans taken, and filelock guards it. Each plan that ends writes the file
# "plan-<position>", with "ok" or "error" on the first line and the error
# message after it.

parallel_queue_new <- function() {
  dir <- tempfile("cs9_plans_")
  dir.create(dir)
  writeLines("0", file.path(dir, "next"))
  return(dir)
}

# The position of the next plan in the queue, from 1. A position above the
# number of plans means that no plan is left.
parallel_queue_take <- function(dir) {
  lock <- filelock::lock(file.path(dir, "next.lock"))
  on.exit(filelock::unlock(lock), add = TRUE)
  position <- as.integer(readLines(file.path(dir, "next"))) + 1L
  writeLines(as.character(position), file.path(dir, "next"))
  return(position)
}

# The rename makes the file appear whole, also when the worker dies.
parallel_queue_result <- function(dir, position, result) {
  f <- file.path(dir, paste0("plan-", position))
  writeLines(
    c(if (result$error) "error" else "ok", result$msg),
    paste0(f, ".part")
  )
  file.rename(paste0(f, ".part"), f)
  return(invisible(NULL))
}

# The error message of each plan that failed, in plan order. A plan without a
# result failed too: its worker died during the plan, or no worker took it.
# `labels` names each plan in the queue, as the message of a failed plan does.
parallel_queue_failures <- function(dir, labels) {
  taken <- as.integer(readLines(file.path(dir, "next")))
  out <- character()
  for (position in seq_along(labels)) {
    f <- file.path(dir, paste0("plan-", position))
    if (file.exists(f)) {
      lines <- readLines(f)
      if (lines[1] == "error") {
        out <- c(out, paste(lines[-1], collapse = "\n"))
      }
      next
    }
    why <- if (position <= taken) {
      "The worker that ran the plan stopped before the plan ended."
    } else {
      "No worker started the plan."
    }
    out <- c(out, parallel_plan_error(labels[position], why))
  }
  return(out)
}

# The message of a failed plan. `label` is argset$index of its first analysis.
parallel_plan_error <- function(label, message) {
  return(paste0(
    "Error in index ",
    label,
    ".\n********\n",
    message,
    "\n********\n"
  ))
}

# Names the database sessions of this process after `context`, such as
# "task|3". csdb names each connection when it opens it. A connection that is
# open already keeps its old name, and a worker keeps its connections from one
# plan to the next. So this also sets application_name on each open
# PostgreSQL connection of `tables` and of config$tables, to the name that
# csdb would give it.
run_set_context <- function(context, tables) {
  csdb::csdb_set_context(context)
  name <- run_application_name(context)
  if (!nzchar(name)) {
    return(invisible(NULL))
  }
  for (x in c(tables, config$tables)) {
    conn <- x$dbconnection
    if (is.null(conn) || !identical(conn$config$driver, "PostgreSQL Unicode")) {
      next
    }
    # NULL when another process opened the connection. A closed connection
    # keeps its handle, and DBI reports it as not valid.
    con <- conn$connection
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbGetQuery(
        con,
        "SELECT set_config('application_name', ?, false) AS name",
        params = list(name)
      )
    }
  }
  return(invisible(NULL))
}

# The application_name for a context, by the rule of csdb 2026.10.15: the
# characters in [A-Za-z0-9_|.:-], cut to the first 63. csdb does not export
# its helper, so this repeats it. A test compares the two.
run_application_name <- function(context) {
  return(substr(gsub("[^A-Za-z0-9_|.:-]", "", context), 1L, 63L))
}
