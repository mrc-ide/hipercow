##' Submit a task to a queue
##'
##' This is a lower-level function that you will not often need to call.
##'
##' @title Submit a task
##' @param id The task id
##'
##' @param ... Disallowed additional arguments, don't use.
##'
##' @param driver The name of the driver to use, or you can leave
##'   blank if only one is configured (this will be typical).
##'
##' @param root The hermod root
##'
##' @export
task_submit <- function(id, ..., driver = NULL, root = NULL) {
  if (...length() > 0) {
    cli::cli_abort("Additional arguments to 'task_submit' not allowed")
  }
  root <- hermod_root(root)

  ## This is a bit gross, could be tidied up later.
  dat <- hermod_driver_prepare(driver, root, environment())
  dat$driver$submit(id, dat$config, root$path$root)

  writeLines(dat$name, file.path(root$path$tasks, id, STATUS_SUBMITTED))
  root$cache$driver[[id]] <- dat$name
  invisible()
}
