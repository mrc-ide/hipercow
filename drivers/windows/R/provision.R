prepare_provision_run <- function(args, check_running_tasks,
                                  config, path_root) {
  show_log <- args$show_log %||% TRUE
  poll <- args$poll %||% 1
  args$show_log <- NULL
  args$poll <- NULL
  client <- get_web_client()
  check_old_versions(r_versions(config$platform),
                     config$r_version,
                     getRversion())
  if (check_running_tasks) {
    check_running_before_install(client, path_root = path_root)
  }

  conan_config <- rlang::inject(conan2::conan_configure(
    !!!args,
    path = path_root,
    path_lib = config$path_lib,
    path_bootstrap = bootstrap_path_from_config(config)))

  id <- ids::random_id()
  path <- file.path(path_root, "hipercow", "provision", id, "conan.R")
  conan2::conan_write(conan_config, path)

  list(poll = poll, id = id, client = client, show_log = show_log)
}


prepare_provision_linux <- function(config, path_root, id) {
  path_to_sh <- write_batch_provision_script(id, config, path_root)
  linux_root <- unc_to_linux_hpc_mount(prepare_path(path_root, config$shares))
  path_dat <- prepare_path(path_to_sh, config$shares)
  rel_to_root <- gsub(paste0("^", linux_root), ".",
                      unc_to_linux_hpc_mount(path_dat))

  list(submit_path = rel_to_root,
       local_path = path_to_sh)
}

prepare_provision_windows <- function(config, path_root, id) {
  path_to_bat <- write_batch_provision_script(id, config, path_root)
  path_bat_dat <- prepare_path(path_to_bat, config$shares)
  path_to_submit <- windows_path_slashes(
    file.path(path_bat_dat$path_remote, path_bat_dat$rel))

  list(submit_path = path_to_submit,
       local_path = path_to_bat)
}

dide_provision_run <- function(args, check_running_tasks, config,
                               path_root) {
  prep <- prepare_provision_run(args, check_running_tasks, config, path_root)
  if (config$platform == "linux") {
    os_prov <- prepare_provision_linux(config, path_root, prep$id)
  } else {
    os_prov <- prepare_provision_windows(config, path_root, prep$id)
  }

  res <- hipercow::hipercow_resources()
  res <- hipercow::hipercow_resources_validate(res, root = path_root)
  res$queue <- cluster_resources(config$platform)$build_queue
  dide_id <- prep$client$submit(os_prov$submit_path,
                                sprintf("conan:%s", prep$id), res)

  path_dide_id <- file.path(dirname(os_prov$local_path), DIDE_ID)
  writeLines(dide_id, path_dide_id)

  path_log <- file.path(dirname(os_prov$local_path), "log")

  res <- logwatch::logwatch(
    "installation",
    function() prep$client$status_job(dide_id),
    function() readlines_if_exists(path_log, warn = FALSE),
    show_log = prep$show_log,
    poll = prep$poll,
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
              path_bootstrap = bootstrap_path_from_config(config)))$hash
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

  # To-do - the below should become an API call to do this efficiently
  # and just return the number of tasks queued/running, rather than
  # fetching them in this way.

  dat <- rbind(client$status_user("Queued"),
               client$status_user("Running"))
  ids <- dat$name[grepl("^[[:xdigit:]]{32}$", dat$name)]
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
