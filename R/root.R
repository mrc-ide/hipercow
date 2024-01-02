##' Create a hipercow root.  This marks the directory where your task
##' information will be saved, along with a local copy of your R
##' packages (a "library" for the cluster).  Immediately after running
##' this the first time, you probably want to run
##' [hipercow::hipercow_configure()] in order to control how we set up
##' your projects network paths and R version.
##'
##' @title Create a hipercow root
##'
##' @param root The path to the root, defaulting the current
##'   directory.
##'
##' @param driver Optionally, the name of a driver to configure
##'
##' @param ... Arguments passed through to [hipercow_configure] if
##'   `driver` is non-NULL.
##'
##' @return Invisibly, the root object
##'
##' @export
hipercow_init <- function(root = ".", driver = NULL, ...) {
  dest <- file.path(root, "hipercow")
  if (fs::dir_exists(dest)) {
    cli::cli_alert_info("hipercow already initialised at '{root}'")
  } else if (fs::file_exists(dest)) {
    cli::cli_abort(
      "Unexpected file 'hipercow' (rather than directory) found at '{root}'")
  } else {
    fs::dir_create(dest)
    cli::cli_alert_success("Initialised hipercow at '{root}'")
  }
  root <- hipercow_root(root)
  if (is.null(driver)) {
    if (is.null(root$config)) {
      cli::cli_alert_info("Next, call 'hipercow_configure()'")
    }
  } else {
    tryCatch(
      hipercow_configure(driver, ..., root = root),
      error = function(e) {
        cli::cli_abort(
          c("Configuration failed",
            i = paste("Your root is still initialised, and if previously",
                      " configured the configuration is unchanged"),
            i = "Try again with 'hipercow::hipercow_configure()' directly"),
          parent = e)
      })
  }
  invisible(root)
}


hipercow_root <- function(root = NULL) {
  if (inherits(root, "hipercow_root")) {
    return(root)
  }
  path <- hipercow_root_find(root)
  if (is.null(cache$roots[[path]])) {
    ret <- new.env(parent = emptyenv())
    ret$path <- list(
      root = path,
      tasks = file.path(path, "hipercow", "tasks"),
      environments = file.path(path, "hipercow", "environments"),
      config = file.path(path, "hipercow", "config"))
    if (file.exists(ret$path$config)) {
      ## TODO: for now we assume that config is saved/loaded by rds;
      ## that's not going to work once we get a polyglot root with
      ## python.  For now at least just load rds configuration.
      files <- dir(ret$path$config, pattern = "\\.rds$", full.names = TRUE)
      ret$config <- set_names(lapply(files, readRDS),
                              sub("\\.rds$", "", basename(files)))
    }
    ret$cache$task_driver <- character()
    ret$cache$task_status_terminal <- character()
    class(ret) <- "hipercow_root"
    if (is.null(cache$roots)) {
      cache$roots <- list()
    }
    cache$roots[[path]] <- ret
  }
  cache$roots[[path]]
}


hipercow_root_find <- function(path) {
  path <- rprojroot::find_root(rprojroot::has_dir("hipercow"), path %||% ".")
  normalize_path(path)
}
