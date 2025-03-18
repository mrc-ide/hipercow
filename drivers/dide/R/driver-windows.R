hipercow_driver_windows <- function() {
  windows_driver <- dide_driver_base()
  windows_driver$configure <- windows_configure
  windows_driver$submit <- windows_submit
  windows_driver$check_hello <- windows_check_hello
  windows_driver
}


windows_submit <- function(id, resources, config, path_root) {
  path_batch <- write_batch_task_run_windows(id, config, path_root)
  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path_slashes(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  dide_id <- client$submit(path_batch_unc, id, resources)
  path_dide_id <- file.path(dirname(path_batch), DIDE_ID)
  writeLines(dide_id, path_dide_id)
}


windows_check_hello <- function(config, path_root) {
  if (!dide_check(path_root)) {
    cli::cli_abort("Failed checks for using windows cluster; please see above")
  }
  resources <- hipercow::hipercow_resources_validate(NULL, "dide-windows",
                                                     path_root)
  resources$queue <- cluster_resources("windows")$build_queue
  resources
}
