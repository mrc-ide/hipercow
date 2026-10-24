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

# These describe our DIDE servers and shares, and how they are mounted
# on the linux nodes. We provide all the reasonable aliases of how they
# might be mounted locally on Windows, "alias", to turn into a standardised
# "server". Then for each share (colon-separated), there will be a matching
# colon-separated linux mountpoint. We also include whether infini is available
# from the cluster node to that share.

# \\wpia-hn\Cluster-Storage is mounted as /mnt/wpia-hn

DIDE_SERVERS <- data.frame( # nolint
   alias = c("wpia-hn", "wpia-hn.dide.ic.ac.uk", "wpia-hn.dide.local",
             "wpia-hn.hpc", "wpia-hn.hpc.dide.ic.ac.uk",
             "wpia-hn.hpc.dide.local"),
    server = "wpia-hn.hpc",
    folder = "cluster-storage",
     linux = "/mnt/cluster",
    infini = TRUE
)

# \\wpia-hn2\Climate-Storage is /mnt/vimc-cc1
# \\wpia-hn2\vimc-cc2-storage is /mnt/vimc-cc2

DIDE_SERVERS <- rbind(DIDE_SERVERS, data.frame( # nolint
  alias = c("wpia-hn2", "wpia-hn2.dide.ic.ac.uk", "wpia-hn2.dide.local",
           "wpia-hn2.hpc", "wpia-hn2.hpc.dide.ic.ac.uk",
           "wpia-hn2.hpc.dide.local"),
  server = "wpia-hn2.hpc",
  folder = "climate-storage:vimc-cc2-storage",
  linux = "/mnt/vimc-cc1:/mnt/vimc-cc2",
  infini = TRUE
))

# \\qdrive\homes\username is /mnt/homes/username

DIDE_SERVERS <- rbind(DIDE_SERVERS, data.frame( # nolint
  alias = c("qdrive", "qdrive.dide.ic.ac.uk", "qdrive.dide.local",
            "wpia-san04", "wpia-san04.dide.ic.ac.uk", "wpia-san04.dide.local"),
  server = "qdrive",
  folder = "homes",
  linux = "/mnt/homes",
  infini = FALSE
))
