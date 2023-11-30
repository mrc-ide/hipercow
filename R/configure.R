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
  root <- hermod_root(root)
  dr <- hermod_driver_load(driver)

  config <- withr::with_dir(root$path$root, dr$configure(...))

  fs::dir_create(root$path$config)
  if (is.null(root$config)) {
    root$config <- list()
  }

  saveRDS(config, file.path(root$path$config, paste0(driver, ".rds")))
  root$config[[driver]] <- config

  invisible()
}


##' Create a new hermod driver; this is intended to be used from other
##' packages, and rarely called directly. If you are trying to run
##' tasks on a cluster you do not need to call this!
##'
##' @param configure Function used to set core configuration for the
##'   driver.  This function will be called from the hermod root
##'   directory (so `getwd()` will report the correct path). It can
##'   take any arguments, do any calculation and then must return any
##'   R object that can be serialised.  The resulting configuration
##'   will be passed in as `config` to other driver functions.
##'
##' @param submit Submit a task to a cluster.  This is run after the
##'   task is created (either automatically or manually) and takes as
##'   arguments the task id, the configuration, the path to the root.
hermod_driver <- function(configure, submit) {
  structure(list(configure = configure,
                 submit = submit),
            class = "hermod_driver")
}


hermod_driver_load <- function(driver, call) {
  if (is.null(cache$drivers[[driver]])) {
    valid <- "windows"
    assert_scalar_character(driver)
    if (!(driver %in% valid)) {
      cli::cli_abort(c("Invalid driver '{driver}'",
                       i = "Valid choices are {squote(valid)}"),
                     call = call)
    }
    ns <- ensure_package(pkg)
    cache$drivers[[driver]] <- hermod_driver_create(driver, pkg, ns)
  }
  cache$drivers[[driver]]
}


## Users should never see these errors, we are in control of our own
## drivers.
hermod_driver_create <- function(name, pkg, ns) {
  target <- sprintf("hermod_driver_%s", name)
  if (!is.function(ns[[target]])) {
    cli::cli_abort("Expected a function '{target}' in package '{pkg}'")
  }
  result <- ns[[target]]()
  if (!inherits(result, "hermod_driver")) {
    cli::cli_abort(
      "'{pkg}:::{target}()' did not return an object of type 'hermod_driver''")
  }
  result
}


hermod_driver_select <- function(name, root, call = NULL) {
  valid <- names(root$config)
  if (is.null(name)) {
    if (length(valid) == 0) {
      cli::cli_abort("No hermod driver configured",
                     i = "Please run 'hermod_configure(\"{name}\")'")
    } else if (length(valid) > 1) {
      cli::cli_abort(
        "More than one hermod driver configured",
        i = "Please provide the argment {arg}",
        i = "Valid options are {squote(valid)}",
        arg = "driver", call = call)
    }
    name <- valid
  } else if (!(name %in% valid)) {
    cli::cli_abort(
      "Invalid value for '{arg}'",
      i = "Valid options are {squote(valid)}",
      arg = "driver", call = call)
  }
  name
}


hermod_driver_prepare <- function(driver, root, call) {
  root <- hermod_root(root)
  driver <- hermod_driver_select(driver, root, call)
  list(name = driver,
       driver = hermod_driver_load(driver, call),
       config = root$config[[driver]])
}
