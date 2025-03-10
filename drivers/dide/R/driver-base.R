dide_driver_base <- function() {
  hipercow::hipercow_driver(

    # These are common between linux and windows drivers

    status = dide_status,
    info = dide_info,
    log = dide_log,
    result = dide_result,
    cancel = dide_cancel,
    provision_run = dide_provision_run,
    provision_list = dide_provision_list,
    provision_compare = dide_provision_compare,
    keypair = dide_keypair,
    cluster_info = dide_cluster_info ,
    default_envvars = DEFAULT_ENVVARS,

    # These are defined by the linux and windows drivers

    configure = NULL,
    submit = NULL,
    check_hello = NULL)
}


time_started <- function(id, path_root) {
  path <- path_to_task_file(path_root, id, "status-running")
  file.info(path, extra_cols = FALSE)$ctime
}


dide_status <- function(id, config, path_root) {
  path_started <- path_to_task_file(path_root, id, "status-running")
  ifelse(file.exists(path_started), "running", "submitted")
}


dide_info <- function(id, config, path_root) {
  client <- get_web_client()
  path_dide_id <- path_to_task_file(path_root, id, DIDE_ID)
  dide_id <- readLines(path_dide_id)
  ## TODO: we could query for the real time of death from the cluster
  ## here too.
  list(status = client$status_job(dide_id),
       time_started = time_started(id, path_root))
}


dide_log <- function(id, outer, config, path_root) {
  if (outer) {
    path_dide_id <- path_to_task_file(path_root, id, DIDE_ID)
    dide_id <- readLines(path_dide_id)
    client <- get_web_client()
    client$log(dide_id)
  } else {
    readlines_if_exists(path_to_task_file(path_root, id, TASK_LOG))
  }
}


dide_result <- function(id, config, path_root) {
  ## Nothing to do here, but we might want to do something in the
  ## cases where the result is not found but the task has failed.
}


dide_cancel <- function(id, config, path_root) {
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


dide_keypair <- function(config, path_root) {
  username <- dide_username()
  tryCatch(
    pub <- keyring::key_get("hipercow/dide/pubkey", username = username),
    error = function(e) {
      cli::cli_abort(
        c("Did not find your DIDE public key",
          i = "Please run 'dide_keypair_generate()' to generate a keypair"),
        parent = e)
    })
  key <- sprintf("//qdrive.dide.ic.ac.uk/homes/%s/.hipercow/key", username)
  list(pub = pub, key = key)
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
