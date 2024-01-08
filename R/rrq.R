hipercow_rrq_controller <- function(root = NULL) {
  root <- hipercow_root(root)
  con <- hipercow_redis_connection(root)
  rrq_id <- ...
  rrq::rrq_controller$new(rrq_id, con)
}


hipercow_rrq_submit_workers <- function(n = 1, resources = NULL, root = NULL) {
  rrq <- hipercow_rrq_controller(root)
  rrq$envir(rrq_context_loader(), notify = FALSE)
  cfg <- rrq::rrq_worker_config(timeout_idle = 600, queue = "default")
  rrq$worker_config_save("hipercow", cfg)
  base <- ids::adjective_animal()
  if (n == 1) {
    worker_ids <- base
    cli::cli_alert_info("Submitting 1 worker with name '{base}'")
  } else {
    worker_ids <- sprintf("%s_%d", base, seq_len(n))
    cli::cli_alert_info("Submitting {n} workers with base name '{base}'")
  }
  rrq_key_alive <- rrq::rrq_worker_expect(rrq, worker_ids)

  ## This is close, but probably won't work. We also need to configure
  ## the log directories; probably as hipercow/rrq
  task_id <- task_create_bulk(
    rrq::rrq_worker$new(rrq_id,
                        name_config = "hipercow",
                        worker_id = id)$loop(),
    list(id = worker_id))

  ## Once this is done the major change to be done is just getting a
  ## sensible chdir behaviour organised so that these can be used by
  ## orderly tasks (i.e., launch the workers against the root, then
  ## queue tasks with a relative path that we get from the orderly
  ## root)
  
  rrq::rrq_worker_wait(rrq, rrq_key_alive, timeout = timeout,
                       progress = progress)
}


hipercow_redis_connection <- function(root) {
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  if (is.null(dat$config$redis_host$external)) {
    cli::cli_abort("The driver '{dat$name}' does not support rrq workers")
  }
  redux::hiredis(dat$config$redis_host$external)
}
