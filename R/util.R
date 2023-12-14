`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


ensure_package <- function(name, call = NULL) {
  if (!requireNamespace(name, quietly = TRUE)) {
    instructions <- paste(
      "Please try installing '{name}' by running (in an empty session)",
      'install.packages("{name}", repos = c("https://mrc-ide.r-universe.dev",',
      '"https://cloud.r-project.org")')
    if (getOption("hipercow.auto_install_missing_packages", TRUE)) {
      cli::cli_alert_info("Trying to install '{name}'")
      cli::cli_alert_info(paste(
        "To prevent this, set",
        "options(hipercow.auto_install_missing_packages = FALSE)"))
      repos <- c("https://mrc-ide.r-universe.dev",
                 CRAN = "https://cloud.r-project.org")
      utils::install.packages(name, repos = repos)
      if (!requireNamespace(name, quietly = TRUE)) {
        cli::cli_abort(
          c("Installation of '{name}' failed!",
            i = instructions),
          call = call)
      }
      cli::cli_alert_success("Installation of '{name}' successful")
    } else {
      cli::cli_abort(
        c("Package '{name}' is not available",
          i = instructions,
          i = paste("To automatically install missing packages, set",
                    "options(hipercow.auto_install_missing_packages = TRUE)")),
        call = call)
    }
  }
  getNamespace(name)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


na_omit <- function(x) {
  x[!is.na(x)]
}


saverds_if_different <- function(object, path) {
  skip <- file.exists(path) && identical(readRDS(path), object)
  if (!skip) {
    saveRDS(object, path)
  }
  !skip
}
