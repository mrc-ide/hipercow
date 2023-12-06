## windows-specific provisioning code, called from hermod
windows_provision <- function(method, config, path_root, ...) {
  conan_config <- conan::conan_configure(
    method,
    path = path_root,
    path_lib = config$path_lib,
    path_bootstrap = config$path_bootstrap,
    ...)
  switch(conan_config$method,
         script = windows_provision_script(conan_config, config, path_root),
         cli::cli_abort("Unsupported provision method '{method}'"))
}


windows_provision_script <- function(conan_config, hermod_config, path_root) {
  id <- ids::random_id()

  path <- file.path(path_root, "hermod", "provision", id, "conan.R")
  conan::conan_write(conan_config, path)

  path_batch <- write_batch_provision_script(id, hermod_config, path_root)

  path_batch_dat <- prepare_path(path_batch, hermod_config$shares)
  path_batch_unc <- windows_path(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  template <- "BuildQueue"
  name <- sprintf("conan:%s", id)
  dide_id <- client$submit(path_batch_unc, id, template)

  path_dide_id <- file.path(dirname(path_batch), DIDE_ID)
  writeLines(dide_id, path_dide_id)

  path_log <- file.path(dirname(path_batch), "log")

  status <- function() {
    switch(client$status_job(dide_id),
           "PENDING" = "waiting",
           "RUNNING" = "running",
           "COMPLETE" = "success",
           "ERROR" = "failure",
           "CANCELLED" = "failure",
           NULL)
  }

  conan::conan_watch(
    status,
    function() readlines_if_exists(path_log, warn = FALSE),
    show_log = conan_config$show_log,
    poll = conan_config$poll)
}
