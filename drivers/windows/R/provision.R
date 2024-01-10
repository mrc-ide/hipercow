## windows-specific provisioning code, called from hipercow
windows_provision_run <- function(args, config, path_root) {
  show_log <- args$show_log %||% TRUE
  poll <- args$poll %||% 1
  args$show_log <- NULL
  args$poll <- NULL

  conan_config <- rlang::inject(conan2::conan_configure(
    !!!args,
    path = path_root,
    path_lib = config$path_lib,
    path_bootstrap = path_bootstrap(config)))

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

  res <- logwatch::logwatch(
    "installation",
    function() client$status_job(dide_id),
    function() readlines_if_exists(path_log, warn = FALSE),
    show_log = show_log,
    poll = poll,
    status_waiting = "submitted",
    status_running = "running")

  elapsed <- format(res$end - res$start, digits = 4)
  if (res$status == "success") {
    cli::cli_alert_success(
      "Installation script finished successfully in {elapsed}")
  } else {
    cli::cli_abort(
      "Installation failed after {elapsed} with status '{res$status}'")
  }
  res
}


windows_provision_list <- function(args, config, path_root) {
  if (is.null(args)) {
    hash <- NULL
  } else {
    hash <- conan_config <- rlang::inject(conan2::conan_configure(
              !!!args,
              path = path_root,
              path_lib = config$path_lib,
              path_bootstrap = path_bootstrap(config)))$hash
  }
  path_lib <- file.path(path_root, config$path_lib)
  conan2::conan_list(path_lib, hash)
}


windows_provision_compare <- function(config, path_root, curr, prev) {
  path_lib <- file.path(path_root, config$path_lib)
  conan2::conan_compare(path_lib, curr, prev)
}
