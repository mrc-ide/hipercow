##' Create a hermod root.  This marks the directory where your task
##' information will be saved, along with a local copy of your R
##' packages (a "library" for the cluster).  Immediately after running
##' this the first time, you probably want to run
##' [hermod::hermod_configure()] in order to control how we set up
##' your projects network paths and R version.
##'
##' @title Create a hermod root
##'
##' @param root The path to the root, defaulting the current
##'   directory.
##'
##' @param driver Optionally, the name of a driver to configure
##'
##' @param ... Arguments passed through to [hermod_configure] if
##'   `driver` is non-NULL.
##'
##' @return Invisibly, the root object
##'
##' @export
hermod_init <- function(root = ".", driver = NULL, ...) {
  dest <- file.path(root, "hermod.json")
  if (file.exists(dest)) {
    cli::cli_alert_info("hermod already initialised at '{root}'")
  } else {
    dir.create(root, FALSE, TRUE)
    writeLines("{}", dest)
    cli::cli_alert_success("Initialised hermod at '{root}'")
  }
  root <- hermod_root(root)
  if (is.null(driver)) {
    if (is.null(root$config)) {
      cli::cli_alert_info("Next, call 'hermod_configure()'")
    }
  } else {
    tryCatch(
      hermod_configure(driver, ..., root = root),
      error = function(e) {
        cli::cli_abort(
          c("Configuration failed",
            i = paste("Your root is still initialised, and if previously",
                      " configured the configuration is unchanged"),
            i = "Try again with 'hermod::hermod_configure()' directly"),
          parent = e)
      })
  }
  invisible(root)
}


hermod_root <- function(root = NULL) {
  if (inherits(root, "hermod_root")) {
    return(root)
  }
  path <- hermod_root_find(root)
  if (is.null(cache$roots[[path]])) {
    ret <- new.env(parent = emptyenv())
    ret$path <- list(
      root = path,
      tasks = file.path(path, "hermod", "tasks"),
      environments = file.path(path, "hermod", "environments"),
      config = file.path(path, "hermod", "config"))
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
    class(ret) <- "hermod_root"
    if (is.null(cache$roots)) {
      cache$roots <- list()
    }
    cache$roots[[path]] <- ret
  }
  cache$roots[[path]]
}


hermod_root_find <- function(path) {
  path <- rprojroot::find_root(rprojroot::has_file("hermod.json"),
                               path %||% ".")
  normalize_path(path)
}
