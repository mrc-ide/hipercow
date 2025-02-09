## windows-specific cluster functions, called from hipercow.

windows_cluster_info <- function(config, path_root) {
  if (config$cluster != "wpia-hn") {
    cli::cli_abort(c(
      "Cluster '{config$cluster}' not supported by windows driver",
      i = "Use wpia-hn."))
  }

  if (!(config$platform %in% c("windows", "linux"))) {
    cli::cli_abort(c(
      "Platform '{config$platform}' not supported by windows driver",
      i = "Use 'windows' or 'linux'."))
  }

  resources <- cluster_resources("wpia-hn", "hipercow.windows")
  list(resources = resources,
       r_versions = r_versions(config$platform),
       redis_url = resources$redis_url)
}
