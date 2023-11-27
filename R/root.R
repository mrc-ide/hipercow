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


##' Configure your hermod root.  `hermod_configure` creates the
##' configuration and `hermod_get_configuration` looks it up.
##'
##' @title Configure your hermod root
##'
##' @param driver The hermod driver; probably you want this to be
##'   `"windows"` as that is all we support at the moment!
##'
##' @param ... Arguments passed to your driver. We'll work out how to
##'   point you at appropriate documentation once it is written.
##'
##' @param root Hermod root, usually best `NULL`
##'
##' @export
hermod_configure <- function(driver, ..., root = NULL) {
  assert_scalar_character(driver)
  package <- sprintf("hermod.%s", driver)
  ns <- ensure_package(package)

  root <- hermod_root(root)
  ## TODO: what name here
  config <- withr::with_dir(root$path$root, ns$make_configuration(...))
  fs::dir_create(root$path$config)
  saveRDS(config, file.path(root$path$config, paste0(driver, ".rds")))
  if (is.null(root$config)) {
    root$config <- list()
  }
  root$config[[driver]] <- config
  invisible()
}


##' @export
##' @rdname hermod_configure
hermod_get_configuration <- function(driver, root = NULL) {
  assert_scalar_character(driver)
  config <- hermod_root(root)$config[[driver]]
  if (is.null(config)) {
    cli::cli_abort(
      c("This hermod root is not configured for driver '{driver}'",
        i = "Please run 'hermod_configure(\"{driver}\", ...)'"))
  }
  config
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
      ## TODO: for now we assume that config is saved/loaded by rds;
      ## that's not going to work once we get a polyglot root with
      ## python.  For now at least just load rds configuration.
      files <- dir(ret$path$config, pattern = "\\.rds$", full.names = TRUE)
      ret$config <- set_names(lapply(files, readRDS),
                              sub("\\.rds$", "", basename(files)))
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
