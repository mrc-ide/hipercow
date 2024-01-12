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
    keypair = windows_cluster_keypair,
    cluster_info = windows_cluster_info)
}


windows_submit <- function(id, config, path_root) {
  path_batch <- write_batch_task_run(id, config, path_root)

  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path_slashes(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  dide_id <- client$submit(path_batch_unc, id, config$template)
  path_dide_id <- file.path(dirname(path_batch), DIDE_ID)
  writeLines(dide_id, path_dide_id)
}


windows_status <- function(id, config, path_root) {
  ## TODO: what would be nice is to query the dide_id and then use the
  ## web client to fetch the *real* task status. That would avoid the
  ## "stuck at PENDING" issues people have seen.
  ##
  ## In order to do this efficiently we'll need a bulk API endpoint to
  ## hit, sending a number of ids all at once and getting back a
  ## vector of status.
  ##
  ## In the meantime, we'll just hit the disk because that is what the
  ## old version of the tools did, and it works fairly well in
  ## practice.
  status <- rep(NA_character_, length(id))
  check <- c("success" = "status-success",
             "failure" = "status-failure",
             "running" = "status-running")
  path <- file.path(path_root, "hipercow", "tasks", id)
  for (s in names(check)) {
    i <- is.na(status)
    if (any(j <- file.exists(file.path(path[i], check[[s]])))) {
      status[i][j] <- s
    }
  }
  status[is.na(status)] <- "submitted"
  status
}


windows_info <- function(id, config, path_root) {
  client <- get_web_client()
  path_dide_id <- file.path(path_root, "hipercow", "tasks", id, DIDE_ID)
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
    path_dide_id <- file.path(path_root, "hipercow", "tasks", id, DIDE_ID)
    dide_id <- readLines(path_dide_id)
    client <- get_web_client()
    client$log(dide_id)
  } else {
    readlines_if_exists(file.path(path_root, "hipercow", "tasks", id, TASK_LOG))
  }
}


windows_cancel <- function(id, config, path_root) {
  path_dide_id <- file.path(path_root, "hipercow", "tasks", id, DIDE_ID)
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
  path <- file.path(path_root, "hipercow", "tasks", id, "status-running")
  file.info(path, extra_cols = FALSE)$ctime
}
