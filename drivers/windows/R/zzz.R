cache <- new.env(parent = emptyenv())


.onAttach <- function(...) {
  # nocov start
  msg <- paste("You have called 'library(hermod.windows)' but you don't need",
               "to; this package just needs to be installed to work, and you",
               "only need to call 'library(hermod)'")
  packageStartupMessage(paste(strwrap(msg), collapse = "\n"))
  # nocov end
}
