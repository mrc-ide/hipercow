## windows-specific cluster functions, called from hipercow.

windows_cluster_info <- function(config, path_root) {
  stopifnot(config$cluster == "wpia-hn") # assert for now, remove later.

  resources <- cluster_resources("wpia-hn", "hipercow.windows")
  list(resources = resources,
       r_versions = r_versions(config$platform),
       redis_url = resources$redis_url)
}
