##' Provision a library. Normally we will do this automatically for
##' you, but with this function you can trigger provisioning.
##'
##' @title Provision cluster library
##'
##' @param method The provisioning method to use, defaulting to
##'   `NULL`, which indicates autoprovision
##'
##' @param ... Arguments passed through to conan. See docs that we
##'   need to write still.
##'
##' @param environment The name of the environment to provision (see
##'   [hermod_environment_create] for details).
##'
##' @inheritParams hermod_task_submit
##'
##' @return Nothing
##'
##' @export
hermod_provision <- function(method = NULL, ..., driver = NULL,
                             environment = "default", root = NULL) {
  ## TODO: here, if *no* driver is found that could be that we are
  ## running on the headnode, either by job submission or directly,
  ## and we'll need to handle that too.
  root <- hermod_root(root)
  ensure_package("conan2", rlang::current_env())
  env <- environment_load(environment, root, rlang::current_env())
  dat <- hermod_driver_prepare(driver, root, rlang::current_env())
  dat$driver$provision(method, dat$config, root$path$root, env, ...)
  invisible()
}
