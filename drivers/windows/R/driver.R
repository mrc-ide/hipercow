hipercow_driver_windows <- function() {
  hipercow::hipercow_driver(
    configure = windows_configure,
    submit = windows_submit,
    status = windows_status,
    log = windows_log,
    result = windows_result,
    cancel = windows_cancel,
    provision = windows_provision)
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
  unname(res == "OK")
}
