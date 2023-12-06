##' Provision a library. Normally we will do this automatically for
##' you, but with this function you can trigger provisioning.
##'
##' @title Provision cluster library
##'
##' @param ... Arguments passed through to conan. See Details.
##'
##' @inheritParams hermod_task_submit
##'
##' @return Nothing
##'
##' @export
hermod_provision <- function(method = NULL, ..., driver = NULL, root = NULL) {
  ## TODO: here, if *no* driver is found that could be that we are
  ## running on the headnode, either by job submission or directly,
  ## and we'll need to handle that too.
  root <- hermod_root(root)
  dat <- hermod_driver_prepare(driver, root, environment())
  dat$driver$provision(method, dat$config, root$path$root, ...)
  invisible()
}
