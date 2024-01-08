##' Provision a library. Normally we will do this automatically for
##' you, but with this function you can trigger provisioning.
##'
##' @title Provision cluster library
##'
##' @param method The provisioning method to use, defaulting to
##'   `NULL`, which indicates we should try and detect the best
##'   provisioning mechanism for you.
##'
##' @param ... Arguments passed through to conan. See docs that we
##'   need to write still.
##'
##' @param environment The name of the environment to provision (see
##'   [hipercow_environment_create] for details).
##'
##' @inheritParams task_submit
##'
##' @return Nothing
##'
##' @export
hipercow_provision <- function(method = NULL, ..., driver = NULL,
                               environment = "default", root = NULL) {
  ## TODO: here, if *no* driver is found that could be that we are
  ## running on the headnode, either by job submission or directly,
  ## and we'll need to handle that too.
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  env <- environment_load(environment, root, rlang::current_env())
  args <- list(method = method, environment = env, ...)

  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_run(args, dat$config, root$path$root)
  invisible()
}


##' List previous successful installations of this hipercow root.
##'
##' @title List installations
##'
##' @inheritParams hipercow_provision
##'
##' @return A [data.frame] with columns:
##'
##' * `name`: the name of the installation. This might be useful with
##'   `conan_compare`
##' * `time`: the time the installation was started
##' * `hash`: the installation hash
##' * `method`: the method used for the installation
##' * `args`: the arguments to the installation (as a list column)
##' * `current`: if using `hipercow_provision_check`, does this
##'   installation match the arguments provided?
##'
##' This object also has class `conan_list` so that it prints nicely,
##'   but you can drop this with `as.data.frame`.
##'
##' @export
hipercow_provision_list <- function(driver = NULL, root = NULL) {
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_list(NULL, dat$config, root$path$root)
}


##' @rdname hipercow_provision_list
##' @export
hipercow_provision_check <- function(method = NULL, ..., driver = NULL,
                                     environment = "default",
                                     root = NULL) {
  ## I don't think this is great, because it will perform poorly with
  ## multiple environments and requires a lot of care to get right.
  ##
  ## We might be interested in "have we ever provisioned?"  So if we
  ## returned a comparison of the different provisionings and if they
  ## match the hash that might be more useful?
  ##
  ## So
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  env <- environment_load(environment, root, rlang::current_env())
  args <- list(method = method, environment = env, ...)
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_list(args, dat$config, root$path$root)
}


##' Compare installations performed into your libraries by conan.
##'
##' @title Compare installations
##'
##' @param curr The previous installation to compare against. Can be a
##'   name (see [hipercow_provision_list] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th
##'   installation". The default value of 0 corresponds to the current
##'   installation.
##'
##' @param prev The previous installation to compare against. Can be a
##'   name (see [hipercow_provision_list] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th installation".
##'   The default of -1 indicates the previous installation. Must
##'   refer to an installation before `curr`. Use `NULL` or -Inf` if
##'   you want to compare against the empty installation.
##'
##' @inheritParams hipercow_provision
##'
##' @return An object of class `conan_compare`, which can be printed
##'   nicely.
##'
##' @export
hipercow_provision_compare <- function(curr = 0, prev = -1, driver = NULL,
                                       root = NULL) {
  root <- hipercow_root(root)
  ensure_package("conan2", rlang::current_env())
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision_compare(dat$config, root$path$root, curr, prev)
}
