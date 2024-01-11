## windows-specific cluster functions, called from hipercow.

windows_cluster_info <- function(config, path_root) {
  if (config$cluster == "wpia-hn") {
    info <- list(
      max_cores = 32,
      max_ram = 512,
      queues = c("AllNodes", "Training"),
      nodes = sprintf("wpia-%003d", (1:70)[-c(41, 42, 49, 50)])
    )
    class(info) <- "windows_cluster_info"
    return(info)
  }
  cli::cli_abort(c(
    "Cluster {config$cluster} not supported by windows driver.",
    i = "Use wpia-hn."))
}
