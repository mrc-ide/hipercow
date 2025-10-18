hipercow_driver_linux <- function() {
  linux_driver <- dide_driver_base()
  linux_driver$configure <- linux_configure
  linux_driver$submit <- linux_submit
  linux_driver$check_hello <- linux_check_hello
  linux_driver
}

linux_submit <- function(id, resources, config, path_root) {
  # Convert local root to linux mount - eg convert something like
  # Q:/testcow or ~/home/net/dide/testcow to /mnt/homes/wrh1/testcow

  linux_root <- unc_to_linux_hpc_mount(prepare_path(path_root, config$shares))

  # Create run.sh and wrap_run.sh in the write place - this returns the
  # windows-style path to that, so we can write DIDE_ID below.

  res <- write_batch_task_run_linux(id, config, path_root)

  client <- get_web_client()
  dide_id <- client$submit(res$linux_path_to_wrap, id, resources)

  # Job submitted - write the DIDE ID.

  path_dide_id <- file.path(dirname(res$local_path_to_wrap), DIDE_ID)
  writeLines(dide_id, path_dide_id)
}

linux_check_hello <- function(config, path_root) {
  if (!dide_check(path_root)) {
    cli::cli_abort(paste("Failed checks for using linux on windows cluster;",
                         "please see above"))
  }
  resources <- hipercow::hipercow_resources_validate(NULL, "dide-linux",
                                                     path_root)
  resources$queue <- cluster_resources("linux")$build_queue
  resources
}
