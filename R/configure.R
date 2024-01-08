##' Configure your hipercow root.  `hipercow_configure` creates the
##' configuration and `hipercow_configuration` looks it up.
##'
##' @title Configure your hipercow root
##'
##' @param driver The hipercow driver; probably you want this to be
##'   `"windows"` as that is all we support at the moment!
##'
##' @param ... Arguments passed to your driver. We'll work out how to
##'   point you at appropriate documentation once it is written.
##'
##' @param root Hipercow root, usually best `NULL`
##'
##' @export
hipercow_configure <- function(driver, ..., root = NULL) {
  root <- hipercow_root(root)

  assert_scalar_character(driver)
  dr <- hipercow_driver_load(driver)
  config <- withr::with_dir(root$path$root, dr$configure(...))

  fs::dir_create(root$path$config)
  if (is.null(root$config)) {
    root$config <- list()
  }

  path_config <- file.path(root$path$config, paste0(driver, ".rds"))
  is_new <- !file.exists(path_config)
  is_changed <- saverds_if_different(config, path_config)
  root$config[[driver]] <- config

  if (is_new) {
    cli::cli_alert_success("Configured hipercow to use '{driver}'")
  } else if (is_changed) {
    cli::cli_alert_success("Updated configuration for '{driver}'")
  } else {
    cli::cli_alert_info("Configuration for '{driver}' unchanged")
  }

  invisible()
}


##' Create a new hipercow driver; this is intended to be used from other
##' packages, and rarely called directly. If you are trying to run
##' tasks on a cluster you do not need to call this!
##'
##' @param configure Function used to set core configuration for the
##'   driver.  This function will be called from the hipercow root
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
##' @param log Fetch the task log. Takes a single task id and an
##'   integer (the number of lines already known) and returns a
##'   character vector of new logs.  Return `NULL` (and not a zero
##'   length character vector) if a log is not available.
##'
##' @param result Fetch a task result.  If needed, copies the result
##'   file into the current hipercow root.  Assume that a result is
##'   available (i.e., we've already checked that the task status is
##'   terminal)
##'
##' @param cancel Cancel one or more tasks. Takes a vector of task
##'   ids, and requests that these tasks are cancelled, returning a
##'   list with elements `cancelled`: a logical vector the same length
##'   indicating if cancellation was successful, and `time_started`:
##'   the time that the task was started, or NA if the task was not
##'   yet started.
##'
##' @param provision Provision a library. Works with conan, and must
##'   accept `method`, `config`, `path_root` followed by `...` to pass
##'   through to `conan2::conan_configure`. It is expected this
##'   function will trigger running conan to provision a library.
##'
##' @export
hipercow_driver <- function(configure, submit, status, log, result, cancel,
                          provision) {
  structure(list(configure = configure,
                 submit = submit,
                 status = status,
                 log = log,
                 result = result,
                 cancel = cancel,
                 provision = provision),
            class = "hipercow_driver")
}


hipercow_driver_load <- function(driver, call) {
  if (!allow_load_drivers()) {
    cli::cli_abort(
      c("Trying to load a driver from code that should not do so",
        i = "This is a hipercow bug, please report, along with the traceback"),
      call = call)
  }
  if (is.null(cache$drivers[[driver]])) {
    valid <- "windows"
    assert_scalar_character(driver)
    if (!(driver %in% valid)) {
      cli::cli_abort(c("Invalid driver '{driver}'",
                       i = "Valid choice{? is/s are}: {squote(valid)}"),
                     call = call)
    }
    cache$drivers[[driver]] <- hipercow_driver_create(driver, call)
  }
  cache$drivers[[driver]]
}


hipercow_driver_create <- function(name, call = NULL) {
  pkg <- sprintf("hipercow.%s", name)
  ns <- ensure_package(pkg, call)
  target <- sprintf("hipercow_driver_%s", name)

  ## Users should never see these errors, we are in control of our own
  ## drivers; these just help us if we're writing new ones.
  stopifnot(is.function(ns[[target]]))
  result <- ns[[target]]()
  stopifnot(inherits(result, "hipercow_driver"))
  result
}


hipercow_driver_select <- function(name, root, call = NULL) {
  valid <- names(root$config)
  if (is.null(name)) {
    if (length(valid) == 0) {
      cli::cli_abort(c("No hipercow driver configured",
                       i = "Please run 'hipercow_configure()'"),
                     call = call)
    } else if (length(valid) > 1) {
      cli::cli_abort(c("More than one hipercow driver configured",
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
                      "please run 'hipercow_configure(\"{name}\")'")
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


hipercow_driver_prepare <- function(driver, root, call) {
  root <- hipercow_root(root)
  driver <- hipercow_driver_select(driver, root, call)
  list(name = driver,
       driver = hipercow_driver_load(driver, call),
       config = root$config[[driver]])
}


allow_load_drivers <- function() {
  if (is.null(cache$allow_load_drivers)) {
    cache$allow_load_drivers <- Sys.getenv("HIPERCOW_NO_DRIVERS", "0") != "1"
  }
  cache$allow_load_drivers
}
