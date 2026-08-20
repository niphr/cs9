# `R CMD check` walks top-level function definitions to decide which declared
# Imports are used. It does not walk the `public = list(...)` of an R6Class, so
# a `pkg::fn()` call inside an R6 method is invisible to it, and the package is
# reported as declared but unused.
#
# Every reference below is a real call site elsewhere in this package. Naming
# them once in a plain top-level function is enough for the scan to see them.
# It changes no behaviour: the function is never called, and evaluating a
# `pkg::fn` name has no effect.
#
# Do NOT add a package here to silence the note. A package that is genuinely
# unused should be removed from Imports instead. `progress` was removed for
# exactly that reason.
ignore_unused_imports <- function() {
  callr::r_bg # R/r6_TaskJob.R
  csutil::unnest_dfs_within_list_of_fully_named_lists # R/r6_Task.R
  later::later # R/r6_TaskJob.R
  later::run_now # R/r6_TaskJob.R
  pbmcapply::pbmclapply # R/r6_Task.R
  pbmcapply::pbmcmapply # R/r6_Task.R
}
