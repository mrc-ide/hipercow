cache <- new.env(parent = emptyenv())


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
