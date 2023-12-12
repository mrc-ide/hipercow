## windows-specific provisioning code, called from hermod
windows_provision <- function(method, config, path_root, environment, ...) {
  conan_config <- conan::conan_configure(
    method,
    path = path_root,
    path_lib = config$path_lib,
    path_bootstrap = config$path_bootstrap,
    environment = environment,
    ...)

  id <- ids::random_id()
  path <- file.path(path_root, "hermod", "provision", id, "conan.R")
  conan::conan_write(conan_config, path)

  path_batch <- write_batch_provision_script(id, config, path_root)

  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  template <- "BuildQueue"
  dide_id <- client$submit(path_batch_unc, sprintf("conan:%s", id), template)

  path_dide_id <- file.path(dirname(path_batch), DIDE_ID)
  writeLines(dide_id, path_dide_id)

  path_log <- file.path(dirname(path_batch), "log")

  status <- function() {
    switch(client$status_job(dide_id),
           "PENDING" = "waiting",
           "RUNNING" = "running",
           "COMPLETE" = "success",
           "failure") # "ERROR", "CANCELLED" or unknown status
  }

  conan::conan_watch(
    status,
    function() readlines_if_exists(path_log, warn = FALSE),
    show_log = conan_config$show_log,
    poll = conan_config$poll)
}
