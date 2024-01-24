##' Configure your hipercow root.  `hipercow_configure` creates the
##' configuration and `hipercow_configuration` looks it up
##'
##' # Windows
##'
##' Options supported by the `windows` driver:
##'
##' * `shares`: Information about shares (additional to the one
##'   mounted as your working directory) that should be made available
##'   to the cluster job. The use case here is where you need access
##'   to some files that are present on a shared drive and you will
##'   access these by absolute path (say `M:/gis/shapefiles/`) from
##'   your tasks.  You can provide a share as a `windows_path` object,
##'   or a list of such objects.  You will not typically need to use
##'   this option.
##'
##' * `r_version`: Control the R version used on the
##'   cluster. Typically hipercow will choose a version close to the
##'   one you are using to submit jobs, of the set available on the
##'   cluster. You can use this option to choose a specific version
##'   (e.g., pass "4.3.0" to select exactly that version).
##'
##' See `vignette("details")` for more information about these options.
##'
##' @title Configure your hipercow root
##'
##' @param driver The hipercow driver; probably you want this to be
##'   `"windows"` as that is all we support at the moment!
##'
##' @param ... Arguments passed to your driver; see Details for
##'   information about what is supported (this varies by driver).
##'
##' @param root Hipercow root, usually best `NULL`
##'
##' @seealso [hipercow_unconfigure], which removes a driver
##'
##' @export
##' @examplesIf FALSE
##' hipercow_configure("windows", r_version = "4.3.0")
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


##' Remove a driver configured by [hipercow_configure].  This will not
##' affect tasks already submitted with this driver, but will prevent
##' any future tasks being submitted with it.
##'
##' @title Remove a driver from a hipercow configuration
##'
##' @param driver The name of the driver to remove
##'
##' @inheritParams hipercow_configure
##'
##' @seealso [hipercow_configuration], which shows currently enabled drivers.
##'
##' @return Nothing, called for its side effects only.
##' @export
hipercow_unconfigure <- function(driver, root = NULL) {
  root <- hipercow_root(root)
  assert_scalar_character(driver)
  if (is.null(root$config[[driver]])) {
    cli::cli_alert_warning(
      "Did not remove configuration for '{driver}' as it was not enabled")
  } else {
    path_config <- file.path(root$path$config, paste0(driver, ".rds"))
    unlink(path_config)
    root$config[[driver]] <- NULL
    cli::cli_alert_success("Removed configuration for '{driver}'")
  }
}


##' Create a new hipercow driver; this is intended to be used from other
##' packages, and rarely called directly. If you are trying to run
##' tasks on a cluster you do not need to call this!
##'
##' @title Create a driver
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
##' @param info Fetch task info for a single task. May take longer
##'   than `status` and expected to retrieve the true status from the
##'   scheduler.
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
##' @param provision_run Provision a library. Works with conan, and
##'   must accept `args`, `config`, and `path_root`. The `args` should be
##'   injected into `conan2::conan_configure`. It is expected this
##'   function will trigger running conan to provision a library.  The
##'   return value is ignored, an error is thrown if the installation fails.
##'
##' @param provision_list List previous installations. Takes `args`
##'   and if non-`NULL` injects into `conan2::conan_configure` (as for
##'   `provision_run`) in order to build a hash. Runs
##'   `conan2::conan_list` returning its value.
##'
##' @param provision_compare Test if a library is current.  It is
##'   expected that this will call `conan2::conan_compare`
##'
##' @param keypair Return a keypair as a list with elements `pub` and
##'   `key`; the public key as a string and the private key as a path
##'   that will be accessible when the cluster runs, but with
##'   permissions that are open only to the user who submitted the
##'   task.
##'
##' @param cluster_info Return information about a particular cluster: its
##'   maximum core count, maximum memory, node list and queue names, used
##'   for validating `hipercow_resource` against that cluster.
##'
##' @export
hipercow_driver <- function(configure, submit, status, info, log, result,
                            cancel, provision_run, provision_list,
                            provision_compare, keypair, cluster_info) {
  structure(list(configure = configure,
                 submit = submit,
                 status = status,
                 info = info,
                 log = log,
                 result = result,
                 cancel = cancel,
                 provision_run = provision_run,
                 provision_list = provision_list,
                 provision_compare = provision_compare,
                 keypair = keypair,
                 cluster_info = cluster_info),
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
    cache$drivers[[driver]] <- hipercow_driver_create(driver, call)
  }
  cache$drivers[[driver]]
}


hipercow_driver_create <- function(driver, call = NULL) {
  assert_scalar_character(driver, call = call)
  if (driver == "example") {
    return(example_driver())
  }

  valid <- "windows"
  if (!(driver %in% valid)) {
    cli::cli_abort(c("Invalid driver '{driver}'",
                     i = "Valid choice{? is/s are}: {squote(valid)}"),
                   call = call)
  }

  pkg <- sprintf("hipercow.%s", driver)
  ns <- ensure_package(pkg, call)
  target <- sprintf("hipercow_driver_%s", driver)

  ## Users should never see these errors, we are in control of our own
  ## drivers; these just help us if we're writing new ones.
  stopifnot(is.function(ns[[target]]))
  result <- ns[[target]]()
  stopifnot(inherits(result, "hipercow_driver"))
  result
}


hipercow_driver_select <- function(name, root, call = NULL) {
  valid <- names(root$config)
  if (isFALSE(name) || (is.null(name) && length(valid) == 0)) {
    return(NULL)
  }

  arg <- "driver"
  if (is.null(name) || isTRUE(name)) {
    if (length(valid) == 0) {
      cli::cli_abort(
        c("No hipercow driver configured",
          i = "Please run 'hipercow_configure()' to configure a driver"),
        call = call)
    } else if (length(valid) > 1) {
      ## TODO: add some sort of default mechanism here.
      cli::cli_abort(
        c("'driver' not specified but multiple drivers are configured",
          i = "Please provide the argument '{arg}'",
          i = "Valid options are: {squote(valid)}",
          i = paste("If you have configured a driver you no longer want, you",
                    "can remove it using 'hipercow_unconfigure()', after which",
                    "the default behaviour will improve")),
        arg = arg, call = call)
    }
    name <- valid
  } else {
    assert_scalar_character(name, name = arg, call = call)
    if (!(name %in% valid)) {
      if (length(valid) == 0) {
        hint <- paste("No driver configured;",
                      "please run 'hipercow_configure(\"{name}\")'")
      } else {
        hint <- "Valid option{? is/s are}: {squote(valid)}"
      }
      cli::cli_abort(
        c("Invalid value for '{arg}': '{name}'",
          i = hint),
        arg = arg, call = call)
    }
  }
  name
}


hipercow_driver_prepare <- function(driver, root, call) {
  root <- hipercow_root(root)
  ## Force loading a driver here.
  if (is.null(driver) || isFALSE(driver)) {
    driver <- TRUE
  }
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
