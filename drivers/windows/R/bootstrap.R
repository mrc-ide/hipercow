bootstrap_update <- function(development = NULL, root = NULL) {
  path_script <- "hipercow/bootstrap-windows.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  bootstrap <- read_template("bootstrap.R")
  if (is.null(development)) {
    data <- list(bootstrap_path = "bootstrap",
                 development_ref = "NULL")
  } else {
    data <- list(bootstrap_path = "bootstrap-dev",
                 development_ref = dquote(development))
  }
  writelines_if_different(glue_whisker(bootstrap, data),
                          path_script_abs)
  hipercow::hipercow_provision("script", script = path_script, root = root)
}


bootstrap_update_all <- function(development = NULL, root = NULL,
                                 versions = r_versions()) {
  versions <- recent_versions(versions)
  for (i in seq_along(versions)) {
    version <- versions[[i]]
    cli::cli_alert_info("Setting up bootstrap for R {version}")
    hipercow::hipercow_init(root %||% ".", driver = "windows",
                            r_version = version)
    bootstrap_update(development = development, root = root)
  }
}


recent_versions <- function(versions = r_versions()) {
  v <- max(versions)
  v[[c(1, 3)]] <- 0
  v[[c(1, 2)]] <- as.integer(v[[c(1, 2)]]) - 1
  versions[versions >= v]
}
