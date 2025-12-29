hipercow_driver_linux <- function() {
  hipercow::hipercow_driver(
    configure = linux_configure,
    submit = linux_submit,
    status = linux_status,
    info = linux_info,
    log = linux_log,
    result = linux_result,
    cancel = linux_cancel,
    provision_run = linux_provision_run,
    provision_list = linux_provision_list,
    provision_compare = linux_provision_compare,
    keypair = linux_keypair,
    check_hello = linux_check_hello,
    cluster_info = linux_cluster_info)
}


linux_submit <- function(id, resources, config, path_root) {
  path_batch <- write_batch_task_run(id, config, resources, path_root)
  scheduler_id <- do_linux_submit(path_batch, id, config, path_root)
  path_scheduler_id <- file.path(dirname(path_batch), SCHEDULER_ID)
  writeLines(dide_id, path_scheduler_id)
}


linux_status <- function(id, config, path_root) {
  path_started <- file.path(path_root, "hipercow", "tasks", id,
                            "status-running")
  ifelse(file.exists(path_started), "running", "submitted")
}


linux_info <- function(id, config, path_root) {
  path_scheduler_id <- file.path(
    path_root, "hipercow", "tasks", id, SCHEDULER_ID)
  scheduler_id <- readLines(path_scheduler_id)
  status <- do_linux_status(scheduler_id, config)
  list(status = status,
       time_started = time_started(id, path_root))
}


linux_result <- function(id, config, path_root) {
  ## Nothing to do here, but we might want to do something in the
  ## cases where the result is not found but the task has failed.
}


linux_log <- function(id, outer, config, path_root) {
  if (outer) {
    path_scheduler_id <- file.path(path_root, "hipercow", "tasks", id, SCHEDULER_ID)
    scheduler_id <- readLines(path_scheduler_id)
    ## TODO: read .o and .e files
    NULL
  } else {
    readlines_if_exists(file.path(path_root, "hipercow", "tasks", id, TASK_LOG))
  }
}


linux_cancel <- function(id, config, path_root) {
  path_scheduler_id <- file.path(path_root, "hipercow", "tasks", id, SCHEDULER_ID)
  scheduler_id <- vcapply(path_scheduler_id, readLines, USE.NAMES = FALSE)
  scheduler_id <- scheduler_id[order(as.integer(scheduler_id), decreasing = TRUE)]
  cancelled <- do_linux_cancel(scheduler_id, config)
  ## Times are awful:
  time_started <- rep(Sys.time(), length(id))
  time_started[] <- NA
  if (any(cancelled)) {
    time_started[cancelled] <- time_started(id[cancelled], path_root)
  }
  list(cancelled = cancelled, time_started = time_started)
}


linux_check_hello <- function(config, path_root) {
  if (!linux_check(path_root)) {
    cli::cli_abort("Failed checks for using linux cluster; please see above")
  }
  resources <- hipercow::hipercow_resources_validate(NULL, "linux", path_root)
  ## TODO: perhaps clusters could return a "fast" queue for use.
  resources
}


linux_keypair <- function(config, path_root) {
  cli::cli_abort("encryption not yet supported with the linux driver")
}


linux_provision <- function(args, config, path_root) {
  cli::cli_abort("provisioning is important but hard")
}


linux_provision_list <- function(args, config, path_root) {
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


linux_provision_compare <- function(curr, prev, config, path_root) {
  path_lib <- file.path(path_root, config$path_lib)
  conan2::conan_compare(path_lib, curr, prev)
}


time_started <- function(id, path_root) {
  path <- file.path(path_root, "hipercow", "tasks", id, "status-running")
  file.info(path, extra_cols = FALSE)$ctime
}
