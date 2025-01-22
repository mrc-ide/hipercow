## windows-specific cluster functions, called from hipercow.

windows_cluster_info <- function(config, path_root) {
  switch(config$cluster,
         "wpia-hn" = cluster_info_wpia_hn(),
         cli::cli_abort(c(
           "Cluster '{config$cluster}' not supported by windows driver",
           i = "Use wpia-hn.")))
}


cluster_info_wpia_hn <- function() {
  resources <- cluster_resources("wpia-hn", "hipercow.windows")
  list(resources = resources,
       r_versions = r_versions(),
       redis_url = resources$redis_url)
}
