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

DIDE_SERVERS <- as.data.frame(rbind(
  c(from = "wpia-hn",                   to = "wpia-hn.hpc", infiniband = TRUE),
  c(from = "wpia-hn.dide.ic.ac.uk",     to = "wpia-hn.hpc", infiniband = TRUE),
  c(from = "wpia-hn.dide.local",        to = "wpia-hn.hpc", infiniband = TRUE),
  c(from = "wpia-hn.hpc",               to = "wpia-hn.hpc", infiniband = TRUE),
  c(from = "wpia-hn.hpc.dide.ic.ac.uk", to = "wpia-hn.hpc", infiniband = TRUE),
  c(from = "wpia-hn.hpc.dide.local",    to = "wpia-hn.hpc", infiniband = TRUE),

  c(from = "wpia-hn2",                   to = "wpia-hn2.hpc", infiniband = TRUE),
  c(from = "wpia-hn2.dide.ic.ac.uk",     to = "wpia-hn2.hpc", infiniband = TRUE),
  c(from = "wpia-hn2.dide.local",        to = "wpia-hn2.hpc", infiniband = TRUE),
  c(from = "wpia-hn2.hpc",               to = "wpia-hn2.hpc", infiniband = TRUE),
  c(from = "wpia-hn2.hpc.dide.ic.ac.uk", to = "wpia-hn2.hpc", infiniband = TRUE),
  c(from = "wpia-hn2.hpc.dide.local",    to = "wpia-hn2.hpc", infiniband = TRUE),

  c(from = "qdrive",                     to = "qdrive", infiniband = FALSE),
  c(from = "qdrive.dide.ic.ac.uk",       to = "qdrive", infiniband = FALSE),
  c(from = "qdrive.dide.local",          to = "qdrive", infiniband = FALSE),

  c(from = "wpia-san04",                 to = "wpia-san04", infiniband = FALSE),
  c(from = "wpia-san04.dide.ic.ac.uk",   to = "wpia-san04", infiniband = FALSE),
  c(from = "wpia-san04.dide.local",      to = "wpia-san04", infiniband = FALSE),

  c(from = "wpia-didef4",                to = "wpia-didef4", infiniband = FALSE),
  c(from = "wpia-didef4.dide.local",     to = "wpia-didef4", infiniband = FALSE),
  c(from = "wpia-didef4.dide.ic.ac.uk",  to = "wpia-didef4", infiniband = FALSE),

  c(from = "projects",                   to = "projects", infiniband = FALSE),
  c(from = "projects.dide.local",        to = "projects", infiniband = FALSE),
  c(from = "projects.dide.ic.ac.uk",     to = "projects", infiniband = FALSE)
))
