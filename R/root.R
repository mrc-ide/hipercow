##' Create a hermod root.  This marks the directory where your task
##' information will be saved, along with a local copy of your R
##' packages (a "library" for the cluster).  Immediately after running
##' this the first time, you probably want to run
##' [hermod::hermod_configure()] in order to control how we set up
##' your projects network paths and R version.
##'
##' @title Create a hermod root
##'
##' @param path The path to the root, defaulting the current
##'   directory.
##'
##' @return Invisibly, the root object
##'
##' @export
hermod_init <- function(path = ".") {
  dest <- file.path(path, "hermod.json")
  if (file.exists(dest)) {
    cli::cli_alert_info("hermod already initialised at '{path}'")
  } else {
    dir.create(path, FALSE, TRUE)
    writeLines("{}", dest)
    cli::cli_alert_success("Initialised hermod at '{path}'")
  }
  root <- hermod_root(path)
  if (is.null(root$config)) {
    cli::cli_alert_info("Next, call 'hermod_configure()'")
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
    ret$path <- list(root = path,
                     tasks = file.path(path, "hermod", "tasks"),
                     config = file.path(path, "hermod", "config"))
    if (file.exists(ret$path$config)) {
      ret$config <- readRDS(ret$path$config)
    }
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
