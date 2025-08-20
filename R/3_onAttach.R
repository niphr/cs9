#' @import data.table
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("cs9", fields = "Version"),
    warning = function(w){
      1
    }
  )

  packageStartupMessage(paste0(
    "cs9 ",
    version,
    "\n",
    "https://www.csids.no/cs9/"
  ))
  
  # Show environment diagnostics when package is attached (only in interactive sessions)
  if(interactive()) {
    check_environment_setup(verbose = TRUE, use_startup_message = TRUE)
  }
}
