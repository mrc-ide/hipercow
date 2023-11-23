didehpc_task_submit <- function(id, root = NULL) {
  root <- hermod_root(root)
  ## TODO: cache this
  cl <- web_client$new(config$credentials, login = TRUE)
  ## path_batch <- file.path("//fi--didef3.dide.ic.ac.uk/tmp",
  ##                       "hermod-example/hermod/tasks", id,
  ##                       BATCH_RUN)
  res <- cl$submit(windows_path(path_batch), id, job_template)
  file.create(file.path(root$paths$tasks, id, STATUS_SUBMITTED))
  writeLines(res$id, file.path(root$paths$tasks, id, DIDE_ID))
  invisible()
}


didehpc_task_status <- function(id, check_dide = FALSE, root = NULL) {
  hermod_task_status(id)
  cl <- web_client$new(config$credentials, login = TRUE)
  ## Probably worth enumerating the ways we can be out of sync here. DIDE will alwys be ahead of disk, so no need to 
  
  ## ...get status here from dide
}


didehpc_client <- function(root) {
  if (is.null(root$didehpc_client)) {
    client <- web_client$new(config$credentials, login = TRUE)
    root$didehpc_client <- client
  }
  root$didehpc_client
}
