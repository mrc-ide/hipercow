hipercow_driver_linux <- function() {
  linux_driver <- dide_driver_base()
  linux_driver$configure <- linux_configure
  linux_driver$submit <- linux_submit
  linux_driver$check_hello <- linux_check_hello
  linux_driver
}

linux_submit <- function(id, resources, config, path_root) {

  # Convert win root to linux mount - eg
  # Q:/testcow to /didehomes/wrh1/testcow

  linux_root <- unc_to_linux_hpc_mount(prepare_path(path_root, config$shares))

  # Create run.sh and wrap_run.sh in the write place - this returns the
  # windows-style path to that, so we can write DIDE_ID below.

  win_path_to_sh <- write_batch_task_run(id, config, path_root)

  # For the API submit call, we want /workdir:/didenames/wrh1/testcow
  # and the job to be relative to that - ./hipercow/tasks/etc

  path_sh_dat <- prepare_path(win_path_to_sh, config$shares)
  linux_rel_to_root <- gsub(paste0("^", linux_root), ".",
                            unc_to_linux_hpc_mount(path_sh_dat))
  client <- get_web_client()
  dide_id <- client$submit(linux_rel_to_root, id, resources,
                           workdir = linux_root)

  # Job submitted - write the DIDE ID.

  path_dide_id <- file.path(dirname(win_path_to_sh), DIDE_ID)
  writeLines(dide_id, path_dide_id)
}

linux_check_hello <- function(config, path_root) {
  if (!windows_check(path_root)) {
    cli::cli_abort(paste("Failed checks for using linux on windows cluster;",
                         "please see above"))
  }
  resources <- hipercow::hipercow_resources_validate(NULL, "dide-linux",
                                                     path_root)
  resources$queue <- cluster_resources("linux")$build_queue
  resources
}
