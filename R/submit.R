hermod_task_submit <- function(id, ..., driver = NULL, root = NULL) {
  if (...length() > 0) {
    cli::cli_abort("Additional arguments to 'hermod_task_submit' not allowed")
  }
  root <- hermod_root(root)

  ## This is a bit gross, could be tidied up later.
  dat <- hermod_driver_prepare(driver, root, environment())
  dat$driver$submit(id, dat$config, root$path$root)

  writeLines(dat$name, file.path(root$path$tasks, id, DRIVER))
  file.create(file.path(root$path$tasks, id, STATUS_SUBMITTED))
  invisible()
}
