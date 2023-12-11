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
  root <- hermod_root(root)

  assert_scalar_character(driver)
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
##'
##' @param status Fetch a task status. Takes a vector of ids and
##'   returns a vector of the same length of statuses.
##'
##' @param result Fetch a task result.  If needed, copies the result
##'   file into the current hermod root.  Assume that a result is
##'   available (i.e., we've already checked that the task status is
##'   terminal)
##'
##' @param provision Provision a library. Works with conan, and must
##'   accept `method`, `config`, `path_root` followed by `...` to pass
##'   through to `conan2::conan_configure`. It is expected this
##'   function will trigger running conan to provision a library.
##'
##' @export
hermod_driver <- function(configure, submit, status, result, cancel,
                          provision) {
  structure(list(configure = configure,
                 submit = submit,
                 status = status,
                 result = result,
                 cancel = cancel,
                 provision = provision),
            class = "hermod_driver")
}


hermod_driver_load <- function(driver, call) {
  if (is.null(cache$drivers[[driver]])) {
    valid <- "windows"
    assert_scalar_character(driver)
    if (!(driver %in% valid)) {
      cli::cli_abort(c("Invalid driver '{driver}'",
                       i = "Valid choice{? is/s are}: {squote(valid)}"),
                     call = call)
    }
    cache$drivers[[driver]] <- hermod_driver_create(driver)
  }
  cache$drivers[[driver]]
}


hermod_driver_create <- function(name) {
  pkg <- sprintf("hermod.%s", name)
  ns <- ensure_package(pkg)
  target <- sprintf("hermod_driver_%s", name)

  ## Users should never see these errors, we are in control of our own
  ## drivers; these just help us if we're writing new ones.
  stopifnot(is.function(ns[[target]]))
  result <- ns[[target]]()
  stopifnot(inherits(result, "hermod_driver"))
  result
}


hermod_driver_select <- function(name, root, call = NULL) {

  valid <- names(root$config)
  if (is.null(name)) {
    if (length(valid) == 0) {
      cli::cli_abort(c("No hermod driver configured",
                       i = "Please run 'hermod_configure()'"),
                     call = call)
    } else if (length(valid) > 1) {
      cli::cli_abort(c("More than one hermod driver configured",
                       i = "Please provide the argument 'driver'",
                       i = "Valid options are: {squote(valid)}"),
                     arg = "driver", call = call)
    }
    name <- valid
  } else {
    assert_scalar_character(name, name = "driver")
    if (!(name %in% valid)) {
      if (length(valid) == 0) {
        hint <- paste("No driver configured;",
                      "please run 'hermod_configure(\"{name}\")'")
      } else {
        hint <- "Valid option{? is/s are}: {squote(valid)}"
      }
      cli::cli_abort(
        c("Invalid value for 'driver': '{name}'",
          i = hint),
        arg = "driver", call = call)
    }
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
