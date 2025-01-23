## windows-specific cluster functions, called from hipercow.

windows_cluster_info <- function(config, path_root, platform = "windows") {
  switch(config$cluster,
         "wpia-hn" = cluster_info_wpia_hn(platform),
         cli::cli_abort(c(
           "Cluster '{config$cluster}' not supported by windows driver",
           i = "Use wpia-hn.")))
}


cluster_info_wpia_hn <- function(platform = "windows") {
  resources <- cluster_resources(platform)
  list(resources = resources,
       r_versions = r_versions(platform),
       redis_url = resources$redis_url)
}
