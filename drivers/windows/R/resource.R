## windows-specific cluster functions, called from hipercow.

windows_cluster_info <- function(config, platform, path_root) {
  switch(config$cluster,
         "wpia-hn" = cluster_info_wpia_hn(platform),
         cli::cli_abort(c(
           "Cluster '{config$cluster}' not supported by windows driver",
           i = "Use wpia-hn.")))
}


cluster_info_wpia_hn <- function(platform) {
  resources <- cluster_resources(platform)
  list(resources = resources,
       r_versions = r_versions(platform),
       redis_url = resources$redis_url)
}
