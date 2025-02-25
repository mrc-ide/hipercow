bootstrap_update <- function(development = NULL, root = NULL,
                             platform = "windows") {
  path_script <- "hipercow/bootstrap-windows.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  bootstrap <- read_template("bootstrap.R")
  prefix <- if (platform == "windows") "I:" else "/wpia-hn/Hipercow"
  suffix <- if (platform == "windows") "" else "-linux"

  bootstrap_repos <- c("https://mrc-ide.r-universe.dev",
                       "https://cloud.r-project.org")
  bootstrap_repos <- sprintf('c("%s")',
                             paste0(bootstrap_repos, collapse = "\", \""))

  bootstrap_pkgs <- c("hipercow", "remotes", "pkgdepends", "renv", "rrq")
  deps <- pkgdepends::pkg_deps$new(bootstrap_pkgs)
  deps$resolve()
  bootstrap_pkgs <- deps$get_resolution()$package
  bootstrap_pkgs <- sprintf('c("%s")',
                            paste0(bootstrap_pkgs, collapse = "\", \""))

  if (is.null(development)) {
    data <- list(bootstrap_path = sprintf("%s/bootstrap%s", prefix, suffix),
                 development_ref = "NULL",
                 bootstrap_repos = bootstrap_repos,
                 bootstrap_pkgs = bootstrap_pkgs)

  } else {
    data <- list(bootstrap_path = sprintf("%s/bootstrap-dev%s", prefix, suffix),
                 development_ref = dquote(development),
                 bootstrap_repos = bootstrap_repos,
                 bootstrap_pkgs = bootstrap_pkgs)
  }

  writelines_if_different(glue_whisker(bootstrap, data),
                          path_script_abs)

  hipercow::hipercow_provision("script", script = path_script, root = root)
}


bootstrap_update_all <- function(development = NULL, root = NULL,
                                 versions = NULL,
                                 platforms = c("windows", "linux")) {
  for (platform in platforms) {
    os_versions <- r_versions(platform)
    if (!is.null(versions)) {
      os_versions <- intersect(os_versions, versions)
    }
    os_versions <- recent_versions(as.numeric_version(os_versions))
    for (i in seq_along(os_versions)) {
      version <- os_versions[[i]]
      cli::cli_alert_info("Setting up bootstrap for R {version} on {platform}")
      hipercow::hipercow_init(root %||% ".",
                              driver = sprintf("dide-%s", platform),
                              r_version = version)
      bootstrap_update(development = development, root = root,
                       platform = platform)
    }
  }
}


recent_versions <- function(versions = r_versions("windows")) {
  v <- max(versions)
  v[[c(1, 3)]] <- 0
  v[[c(1, 2)]] <- as.integer(v[[c(1, 2)]]) - 1
  versions[versions >= v]
}
