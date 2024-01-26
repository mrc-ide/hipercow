## windows-specific cluster functions, called from hipercow.

windows_cluster_info <- function(config, path_root) {
  switch(config$cluster,
         "wpia-hn" = cluster_info_wpia_hn(),
         cli::cli_abort(c(
           "Cluster '{config$cluster}' not supported by windows driver",
           i = "Use wpia-hn.")))
}


cluster_info_wpia_hn <- function() {
  resources <- list(
    max_cores = 32,
    max_ram = 512,
    queues = c("AllNodes", "Training"),
    default_queue = "AllNodes",
    nodes = sprintf("wpia-%003d", (1:70)[-c(41, 42, 49, 50)])
  )
  redis_url <- NULL # not ready to be used yet
  list(resources = resources,
       r_versions = r_versions(),
       redis_url = redis_url)
}
