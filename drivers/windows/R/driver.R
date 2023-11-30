hermod_driver_windows <- function() {
  hermod::hermod_driver(
    configure = windows_configure,
    submit = windows_submit)
}


windows_submit <- function(id, config, path_root) {
  path_batch <- write_batch_task_run(id, config, path_root)

  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  dide_id <- client$submit(path_batch_unc, id, config$template)
  writeLines(dide_id, file.path(dirname(path_batch), DIDE_ID))
}
