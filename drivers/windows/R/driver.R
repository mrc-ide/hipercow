hipercow_driver_windows <- function() {
  hipercow::hipercow_driver(
    configure = windows_configure,
    submit = windows_submit,
    status = windows_status,
    info = windows_info,
    log = windows_log,
    result = windows_result,
    cancel = windows_cancel,
    provision_run = windows_provision_run,
    provision_list = windows_provision_list,
    provision_compare = windows_provision_compare,
    keypair = windows_keypair,
    check_hello = windows_check_hello,
    cluster_info = windows_cluster_info)
}


windows_submit <- function(id, resources, config, path_root) {
  path_batch <- write_batch_task_run(id, config, path_root)

  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path_slashes(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  dide_id <- client$submit(path_batch_unc, id, resources)
  path_dide_id <- file.path(dirname(path_batch), DIDE_ID)
  writeLines(dide_id, path_dide_id)
}


windows_status <- function(id, config, path_root) {
  path_started <- path_to_task_file(path_root, id, "status-running")
  ifelse(file.exists(path_started), "running", "submitted")
}


windows_info <- function(id, config, path_root) {
  client <- get_web_client()
  path_dide_id <- path_to_task_file(path_root, id, DIDE_ID)
  dide_id <- readLines(path_dide_id)
  ## TODO: we could query for the real time of death from the cluster
  ## here too.
  list(status = client$status_job(dide_id),
       time_started = time_started(id, path_root))
}


windows_result <- function(id, config, path_root) {
  ## Nothing to do here, but we might want to do something in the
  ## cases where the result is not found but the task has failed.
}


windows_log <- function(id, outer, config, path_root) {
  if (outer) {
    path_dide_id <- path_to_task_file(path_root, id, DIDE_ID)
    dide_id <- readLines(path_dide_id)
    client <- get_web_client()
    client$log(dide_id)
  } else {
    readlines_if_exists(path_to_task_file(path_root, id, TASK_LOG))
  }
}


windows_cancel <- function(id, config, path_root) {
  path_dide_id <- path_to_task_file(path_root, id, DIDE_ID)
  dide_id <- vcapply(path_dide_id, readLines, USE.NAMES = FALSE)
  dide_id <- dide_id[order(as.integer(dide_id), decreasing = TRUE)]
  client <- get_web_client()
  ## Cancel here returns a named vector of "OK", and will return
  ## "WRONG_STATE" if cancellation fails.
  res <- client$cancel(dide_id)
  cancelled <- unname(res == "OK")
  ## Times are awful:
  time_started <- rep(Sys.time(), length(id))
  time_started[] <- NA
  if (any(cancelled)) {
    time_started[cancelled] <- time_started(id[cancelled], path_root)
  }
  list(cancelled = cancelled, time_started = time_started)
}


windows_check_hello <- function(config, path_root) {
  if (!windows_check(path_root)) {
    cli::cli_abort("Failed checks for using windows cluster; please see above")
  }
  resources <- hipercow::hipercow_resources_validate(NULL, driver = "windows",
                                                     root = path_root)
  resources$queue <- "BuildQueue"
  resources
}


windows_keypair <- function(config, path_root) {
  username <- windows_username()
  tryCatch(
    pub <- keyring::key_get("hipercow/dide/pubkey", username = username),
    error = function(e) {
      cli::cli_abort(
        c("Did not find your DIDE public key",
          i = "Please run 'windows_keypair_generate()' to generate a keypair"),
        parent = e)
    })
  key <- sprintf("//fi--san03.dide.ic.ac.uk/homes/%s/.hipercow/key", username)
  list(pub = pub, key = key)
}


time_started <- function(id, path_root) {
  path <- path_to_task_file(path_root, id, "status-running")
  file.info(path, extra_cols = FALSE)$ctime
}


path_to_task_file <- function(path_root, id, file) {
  id_head <- substr(id, 1, 2)
  id_tail <- substr(id, 3, nchar(id))
  if (is.null(file)) {
    file.path(path_root, "hipercow", "tasks", id_head, id_tail)
  } else {
    file.path(path_root, "hipercow", "tasks", id_head, id_tail, file)
  }
}
