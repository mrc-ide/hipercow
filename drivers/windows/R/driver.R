hermod_driver_windows <- function() {
  hermod::hermod_driver(
    configure = windows_configure,
    submit = windows_submit,
    status = windows_status)
}


windows_submit <- function(id, config, path_root) {
  path_batch <- write_batch_task_run(id, config, path_root)

  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  dide_id <- client$submit(path_batch_unc, id, config$template)
  path_dide_id <- file.path(dirname(path_batch), DIDE_ID)
  writeLines(dide_id, path_dide_id)
  cache_dide_id(id, readLines(path_dide_id))
}


windows_status <- function(id, config, path_root) {
  dide_id <- vcapply(id, task_dide_id, path_root)

  client <- get_web_client()
  ## TODO: we need a bulk api here, this will be slow if there's more
  ## than one.
  vcapply(dide_id, client$status, USE.NAMES = FALSE)
}


task_dide_id <- function(id, path_root) {
  if (is.null(cache$task_dide_id[[id]])) {
    path <- file.path(path_root, "hermod", "tasks", id, DIDE_ID)
    cache_dide_id(id, readLines(path))
  }
  cache$task_dide_id[[id]]
}


cache_dide_id <- function(task_id, dide_id) {
  if (is.null(cache$task_dide_id)) {
    cache$task_dide_id <- list()
  }
  cache$task_dide_id[[task_id]] <- dide_id
}
