bootstrap_update <- function(development = NULL, root = NULL,
                             platform = "windows") {
  path_script <- "hipercow/bootstrap-windows.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  bootstrap <- read_template("bootstrap.R")

  bootstrap_path <- "bootstrap"
  if (platform == "linux") {
    bootstrap_path <- "bootstrap-linux"
  }
  if (is.null(development)) {
    data <- list(bootstrap_path = bootstrap_path,
                 development_ref = "NULL")
  } else {
    data <- list(bootstrap_path = paste0(bootstrap_path, "-dev"),
                 development_ref = dquote(development))
  }
  writelines_if_different(glue_whisker(bootstrap, data),
                          path_script_abs)
  hipercow::hipercow_provision("script", script = path_script, root = root,
                               platform = platform)
}


bootstrap_update_all <- function(development = NULL, root = NULL,
                                 versions = NULL,
                                 platforms = "windows") {
  for (platform in platforms) {
    platform_versions <- r_versions(platform)
    if (!is.null(versions)) {
      platform_versions <- platform_versions[platform_versions %in% versions]
    }
    platform_versions <- recent_versions(as.numeric_version(platform_versions))
    for (i in seq_along(platform_versions)) {
      version <- platform_versions[[i]]
      cli::cli_alert_info("Setting up bootstrap for R {version}")
      hipercow::hipercow_init(root %||% ".", driver = "windows",
                              r_version = version)
      bootstrap_update(development = development, root = root,
                       platform = platform)
    }
  }
}


recent_versions <- function(versions = r_versions()) {
  v <- max(versions)
  v[[c(1, 3)]] <- 0
  v[[c(1, 2)]] <- as.integer(v[[c(1, 2)]]) - 1
  versions[versions >= v]
}
