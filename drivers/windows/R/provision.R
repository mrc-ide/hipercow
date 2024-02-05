## windows-specific provisioning code, called from hipercow
windows_provision_run <- function(args, config, path_root) {
  show_log <- args$show_log %||% TRUE
  poll <- args$poll %||% 1
  args$show_log <- NULL
  args$poll <- NULL

  client <- get_web_client()
  check_running_before_install(client, path_root = path_root)

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

  res <- hipercow::hipercow_resources()
  res <- hipercow::hipercow_resources_validate(res, root = path_root)
  res$queue <- "BuildQueue"
  dide_id <- client$submit(path_batch_unc, sprintf("conan:%s", id), res)

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


windows_provision_compare <- function(curr, prev, config, path_root) {
  path_lib <- file.path(path_root, config$path_lib)
  conan2::conan_compare(path_lib, curr, prev)
}


check_running_before_install <- function(client, path_root,
                                         timeout = Inf, poll = 1,
                                         progress = NULL) {
  cli::cli_alert_info("Looking for active tasks before installation")
  dat <- client$status_user("*")
  ids <- dat$name[dat$status %in% c("submitted", "running") &
                  grepl("^[[:xdigit:]]{32}$", dat$name)]
  ids <- ids[file.exists(path_to_task_file(path_root, ids, NULL))]

  if (length(ids) == 0) {
    cli::cli_alert_success("No tasks running")
    return(TRUE)
  }
  cli::cli_alert_warning(
    "You have {length(ids)} current task{?s} queued or running")
  cli::cli_alert_info(paste(
    "Due to the way that windows handles file locking, if you install",
    "packages while they are in use, the installation will probably fail.",
    "Sometimes this can also leave your library in a confused state, with",
    "partially-installed but useless packages."),
    wrap = TRUE)
  cli::cli_alert_info("You have three courses of action here:")
  cli::cli_li(
    "{.strong Cancel}: Give up now and try again another time")
  cli::cli_li(paste(
    "{.strong Wait}: I can wait until {cli::qty(length(ids))}",
    "{?your task has/all your tasks have} completed, then start the",
    "installation. I won't notice though if you queue more tasks in",
    "the meantime"))
  cli::cli_li(paste(
    "{.strong Install} anyway: Let's see how it goes and pick up",
    "the pieces. Not recommended, but yolo."))

  action <- menu(c("cancel", "wait", "install"))

  if (action == "cancel") {
    cli::cli_abort("Installation cancelled, try again later")
  } else if (action == "wait") {
    bundle <- hipercow::hipercow_bundle_create(ids, validate = FALSE,
                                               root = path_root)
    cli::cli_alert_info("Waiting for your tasks to complete")
    hipercow::hipercow_bundle_wait(bundle, timeout = timeout, poll = poll,
                                   fail_early = TRUE, progress = progress,
                                   root = path_root)
    cli::cli_alert_success(
      "All tasks now finished, proceeding with installation")
  } else { # install
    cli::cli_alert_warning(
      "Trying the installation anyway, wish me luck.")
  }
}
