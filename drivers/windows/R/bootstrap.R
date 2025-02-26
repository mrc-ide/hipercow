bootstrap_path_windows <- function(development) {
  if (!is.null(development) && !isFALSE(development)) {
    "I:/bootstrap-dev-windows"
  } else {
    "I:/bootstrap-windows"
  }
}

bootstrap_path_linux <- function(development) {
  if (!is.null(development) && !isFALSE(development)) {
    "/wpia-hn/Hipercow/bootstrap-dev-linux"
  } else {
    "/wpia-hn/Hipercow/bootstrap-linux"
  }
}

bootstrap_path <- function(platform, development) {
  if (platform == "windows") {
    bootstrap_path_windows(development)
  } else {
    bootstrap_path_linux(development)
  }
}

all_bootstrap_packages <- function() {
  bootstrap_pkgs <- c("hipercow", "remotes", "pkgdepends", "renv", "rrq")
  deps <- pkgdepends::pkg_deps$new(bootstrap_pkgs)
  deps$resolve()
  deps$get_resolution()$package
}

all_repos <- function() {
  c("https://mrc-ide.r-universe.dev",
    "https://cloud.r-project.org")
}

bootstrap_update <- function(platform, development = NULL, root = NULL) {
  path_script <- "hipercow/bootstrap-windows.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  bootstrap <- read_template("bootstrap.R")

  dev_ref <- ""
  if (!is.null(development)) {
    dev_ref <- dquote(development)
  }
  data <- list(bootstrap_path = bootstrap_path(platform, development),
               development_ref = dev_ref,
               bootstrap_repos = quoted_comma_sep(all_repos()),
               bootstrap_pkgs = quoted_comma_sep(all_bootstrap_packages()))

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
    if (length(os_versions) == 0) {
      cli::cli_abort(c("No matching R version(s) available on {platform}",
                       i = "You requested {versions}",
                       i = "Available: {r_versions(platform)}"))
    }
    os_versions <- recent_versions(as.numeric_version(os_versions))
    for (i in seq_along(os_versions)) {
      version <- os_versions[[i]]
      cli::cli_alert_info("Setting up bootstrap for R {version} on {platform}")
      hipercow::hipercow_init(root %||% ".",
                              driver = sprintf("dide-%s", platform),
                              r_version = version)
      bootstrap_update(platform = platform, development = development,
                       root = root)
    }
  }
}


recent_versions <- function(versions = r_versions("windows")) {
  v <- max(versions)
  v[[c(1, 3)]] <- 0
  v[[c(1, 2)]] <- as.integer(v[[c(1, 2)]]) - 1
  versions[versions >= v]
}
