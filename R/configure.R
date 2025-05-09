##' Configure your hipercow root.  `hipercow_configure` creates the
##' configuration and `hipercow_configuration` looks it up
##'
##' # DIDE Cluster - Windows nodes
##'
##' Options supported by the `dide-windows` driver:
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
##' * `redis_url`: Control the URL used to connect to Redis.  You can
##'   use this to use an alternative Redis host, which is unlikely
##'   unless we have suggested you do this.  The default (`NULL`) uses
##'   the Redis server on the headnode.
##'
##' See `vignette("details")` for more information about these options.
##'
##' @title Configure your hipercow root
##'
##' @param driver The hipercow driver; we support two at present:
##'   `"dide-windows"` and `"dide-linux"`.
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
##' hipercow_configure("dide-windows", r_version = "4.3.0")
hipercow_configure <- function(driver, ..., root = NULL) {
  root <- hipercow_root(root)

  assert_scalar_character(driver)
  if (driver == "windows") {
    cli::cli_abort(
      c("Please use 'dide-windows' for your driver, and not 'windows'",
        i = paste("We are in the process of commissioning a linux cluster",
                  "and support for using ICT's cluster, so we need to",
                  "disambiguate the name here")))
  }
  dr <- hipercow_driver_load(driver)
  config <- withr::with_dir(root$path$root, dr$configure(...))

  if (is.null(root$config)) {
    root$config <- list()
  }

  path <- configuration_path(root, driver)
  legacy_path <- configuration_path(root, driver, legacy = TRUE)

  fs::dir_create(fs::path_dir(path))
  saveRDS(config, path)

  migrated <- file.exists(legacy_path)
  if (migrated) {
    unlink(legacy_path)
  }

  is_new <- is.null(root$config[[driver]])
  is_changed <- !identical(config, root$config[[driver]])
  root$config[[driver]] <- config

  if (migrated) {
    cli::cli_alert_success("Migrated from legacy configuration for '{driver}'")
  } else if (is_new) {
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
    unlink(configuration_path(root, driver))
    unlink(configuration_path(root, driver, legacy = TRUE))
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
##' @param check_hello Run any preflight checks before launching a
##'   hello world task.  Return a validated resources list.
##'
##' @param cluster_info Return information about a particular cluster:
##'   its maximum core count, maximum memory, node list and queue
##'   names, used for validating [hipercow_resources] against that
##'   cluster.
##'
##' @param default_envvars Driver-specific default environment
##'   variables.  Drivers can use this to add environment variables
##'   that have a higher precedence than the hipercow defaults, but
##'   lower precedence than the `hipercow.default_envvars` option or
##'   the `envvars` argument to a task.
##'
##' @export
hipercow_driver <- function(configure, submit, status, info, log, result,
                            cancel, provision_run, provision_list,
                            provision_compare, keypair, check_hello,
                            cluster_info, default_envvars = NULL) {
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
                 check_hello = check_hello,
                 cluster_info = cluster_info,
                 default_envvars = default_envvars),
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

  drivers <- hipercow_drivers()

  valid <- names(drivers)
  if (!(driver %in% valid)) {
    cli::cli_abort(c("Invalid driver '{driver}'",
                     i = "Valid choice{? is/s are}: {squote(valid)}"),
                   call = call)
  }

  pkg <- drivers[[driver]][[1]]
  target <- drivers[[driver]][[2]]
  ns <- ensure_package(pkg, call = call)

  ## Users should never see these errors, we are in control of our own
  ## drivers; these just help us if we're writing new ones.
  stopifnot(is.function(ns[[target]]))
  result <- ns[[target]]()
  stopifnot(inherits(result, "hipercow_driver"))
  result
}


hipercow_driver_select <- function(name, required, root, call = NULL) {
  valid <- names(root$config)
  if (!required && (isFALSE(name) || (is.null(name) && length(valid) == 0))) {
    return(NULL)
  }

  arg <- "driver"
  if (isFALSE(name)) {
    cli::cli_abort(
      c("Invalid choice '{arg} = FALSE'; a driver is required here",
        i = paste("You have provided '{arg} = FALSE' to try and prevent",
                  "loading a driver, but to complete this action you need",
                  "a driver, so there's nothing I can do here")),
      arg = arg, call = call)
  }
  if (is.null(name) || isTRUE(name)) {
    if (length(valid) == 0) {
      cli::cli_abort(
        c("No hipercow driver configured",
          i = "Please run 'hipercow_configure()' to configure a driver"),
        call = call)

    } else if (setequal(valid, c("windows", "dide-windows"))) {

      # Where two drivers are found and they are `dide-windows` and `windows`,
      # then always use `dide-windows` as the other is dead. Could warn here,
      # but I think better to just silently do the right thing, since we've
      # already forced correct use in hipercow_configure

      valid <- "dide-windows"

    } else if (length(valid) > 1) {
      ## TODO (mrc-4980): add some sort of default mechanism here.
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
        hint <- paste("The '{name}' driver is not configured;",
                      "please run 'hipercow_configure(\"{name}\", ...)'")
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
  if (is.null(driver)) {
    cli::cli_abort(
      c("Trying to load a driver after deciding not to (a hipercow bug)",
        i = paste("Please let us know that you've seen this message along",
                  "with the traceback")))
  }
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


configuration_path <- function(root, driver, legacy = FALSE) {
  if (legacy) {
    file.path(root$path$config, paste0(driver, ".rds"))
  } else {
    file.path(root$path$config, hostname(), paste0(driver, ".rds"))
  }
}


load_configuration <- function(base) {
  ## TODO: for now we assume that config is saved/loaded by rds;
  ## that's not going to work once we get a polyglot root with
  ## python.  For now at least just load rds configuration.

  list_files <- function(p) {
    f <- dir(p, pattern = "\\.rds$", full.names = TRUE)
    set_names(f, sub("\\.rds$", "", basename(f)))
  }

  legacy_files <- list_files(base)
  files <- list_files(file.path(base, hostname()))

  # Only keep the legacy files for which no file exists at the new path.
  legacy_files <- legacy_files[!(names(legacy_files) %in% names(files))]
  n <- length(legacy_files)
  if (n > 0) {
    cli::cli_warn(
      c("!" = paste("Using legacy configuration for the",
                    "{squote(names(legacy_files))} driver{?s}"),
        "i" = paste("{cli::qty(n)}Call 'hipercow_configure()' to",
                    "re-configure the driver{?s}")))
  }

  lapply(c(files, legacy_files), readRDS)
}
