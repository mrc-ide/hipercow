## windows-specific provisioning code, called from hipercow
windows_provision <- function(method, config, path_root, environment, ...,
                              show_log = TRUE, poll = 1) {
  conan_config <- conan2::conan_configure(
    method,
    path = path_root,
    path_lib = config$path_lib,
    path_bootstrap = path_bootstrap(config),
    environment = environment,
    ...)

  id <- ids::random_id()
  path <- file.path(path_root, "hipercow", "provision", id, "conan.R")
  conan2::conan_write(conan_config, path)

  path_batch <- write_batch_provision_script(id, config, path_root)

  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path_slashes(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  template <- "BuildQueue"
  dide_id <- client$submit(path_batch_unc, sprintf("conan:%s", id), template)

  path_dide_id <- file.path(dirname(path_batch), DIDE_ID)
  writeLines(dide_id, path_dide_id)

  path_log <- file.path(dirname(path_batch), "log")

  get_status <- function() {
    switch(client$status_job(dide_id),
           "PENDING" = "waiting",
           "RUNNING" = "running",
           "COMPLETE" = "success",
           "failure") # "ERROR", "CANCELLED" or unknown status
  }
  get_log <- function() {
    readlines_if_exists(path_log, warn = FALSE)
  }

  res <- logwatch::logwatch(
    "installation",
    get_status,
    get_log,
    show_log = show_log,
    poll = poll)
  elapsed <- format(res$end - res$start, digits = 4)
  if (res$status == "success") {
    cli::cli_alert_success(
      "Installation script finished successfully in {elapsed}")
  } else {
    cli::cli_abort("Installation failed after {elapsed}")
  }
  res
}
