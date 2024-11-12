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
##' @examples
##'
##' # Create an empty root
##' path <- withr::local_tempfile()
##' hipercow_init(path)
hipercow_init <- function(root = ".", driver = NULL, ...) {
  if (is.null(driver) && length(list(...)) > 0) {
    cli::cli_alert_warning(
      "`driver` was not specified, but extra args were detected.")
  }
  abs_path <- fs::path_abs(root)
  norm_path <- fs::path_norm(root)
  not_same_path <- (abs_path != norm_path)
  desc_root <- sprintf("'%s'%s", root,
    if (not_same_path) sprintf(" (%s)", abs_path) else "")
  dest <- file.path(root, "hipercow")
  if (fs::dir_exists(dest)) {
    cli::cli_alert_info("hipercow already initialised at {desc_root}")
  } else if (fs::file_exists(dest)) {
    cli::cli_abort(
      "Unexpected file 'hipercow' (rather than directory) found at {desc_root}")
  } else {
    fs::dir_create(dest)
    cli::cli_alert_success("Initialised hipercow at {desc_root}")
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
  path <- hipercow_root_find(root %||% getwd())
  if (is.null(cache$roots[[path]])) {
    check_old_path_format(path)
    ret <- new.env(parent = emptyenv())
    ret$path <- list(
      root = path,
      tasks = file.path(path, "hipercow", "tasks"),
      environments = file.path(path, "hipercow", "environments"),
      bundles = file.path(path, "hipercow", "bundles"),
      retry = file.path(path, "hipercow", "retry"),
      rrq = file.path(path, "hipercow", "rrq"),
      config = file.path(path, "hipercow", "config"))
    if (file.exists(ret$path$config)) {
      ret$config <- load_configuration(ret$path$config)
    }
    ret$retry_map <- read_retry_map(ret$path$retry)
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
  path <- find_directory_descend("hipercow", start = path, limit = "/")
  if (length(path) == 0) {
    cli::cli_abort(
      c("Couldn't find hipercow root",
        i = "Perhaps you need to run 'hipercow_init'"))
  }
  path_description <- file.path(path, "hipercow", "DESCRIPTION")
  if (file.exists(path_description)) {
    d <- as.list(read.dcf(path_description)[1, ])
    if (identical(d$Package, "hipercow")) {
      cli::cli_abort(
        c("Found unlikely hipercow root",
          i = paste("Hi Rich or Wes. It looks like you forgot to add an",
                    "argument 'root = path' to whatever you're writing at the",
                    "moment."),
          i = "(If you're someone else seeing this, we're curious how)"))
    }
  }
  normalize_path(path)
}


check_old_path_format <- function(path) {
  contents <- dir(file.path(path, "hipercow", "tasks"))
  if (any(grepl("^[[:xdigit:]]{32}$", contents))) {
    cli::cli_abort(
      c("Your hipercow root is incompatible with this version of hipercow",
        i = paste(
          "You have tasks created with hipercow earlier than 0.3.0; we have",
          "changed the format in more recent versions to make it more",
          "efficient.  Please let Rich and Wes know and we'll help get you",
          "migrated")))
  }
}


migrate_0_3_0 <- function(path) {
  contents <- dir(file.path(path, "hipercow", "tasks"))
  for (id in grep("^[[:xdigit:]]{32}$", contents, value = TRUE)) {
    dest <- file.path(path, "hipercow", "tasks", substr(id, 1, 2),
                      substr(id, 3, nchar(id)))
    fs::dir_create(dirname(dest))
    fs::file_move(file.path(path, "hipercow", "tasks", id), dest)
  }
}
