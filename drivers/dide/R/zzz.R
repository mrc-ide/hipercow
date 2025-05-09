cache <- new.env(parent = emptyenv())


.onLoad <- function(...) {
  # nocov start
  if (nzchar(Sys.getenv("DEVTOOLS_LOAD"))) {
    return()
  }
  dide_check_versions(report_failure_only = TRUE)
  # nocov end
}


.onAttach <- function(...) {
  # nocov start
  if (nzchar(Sys.getenv("DEVTOOLS_LOAD"))) {
    return()
  }
  msg <- paste("You have called 'library(hipercow.windows)' but you don't need",
               "to; this package just needs to be installed to work, and you",
               "only need to call 'library(hipercow)'")
  packageStartupMessage(paste(strwrap(msg), collapse = "\n"))
  # nocov end
}


DEFAULT_ENVVARS <- hipercow::hipercow_envvars( # nolint
  "CMDSTAN" = "I:/cmdstan/cmdstan-2.35.0",
  "CMDSTANR_USE_RTOOLS" = "TRUE")
