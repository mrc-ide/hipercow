##' A helper used in running examples
##'
##' @title Example helper
##'
##' @param runner Start a runner?
##'
##' @param with_logging Run each task with logging; this is quite a
##'   bit slower.
##'
##' @export
##'
##' @keywords internal
hipercow_example_helper <- function(runner = TRUE, with_logging = FALSE) {
  message("(This example uses a special helper)")
  path <- tempfile()
  cache$drivers[["example"]] <- example_driver()
  suppressMessages(hipercow_init(path, driver = "example"))
  owd <- setwd(path)
  if (runner) {
    args <- list(path, with_logging)
    px <- callr::r_bg(example_runner, args, package = TRUE,
                      stdout = NULL, stderr = NULL)
  }
  set_cache_dir <- is.na(Sys.getenv("R_USER_CACHE_DIR", NA))
  if (set_cache_dir) {
    Sys.setenv(R_USER_CACHE_DIR = tempfile())
  }

  cleanup <- function() {
    message("(cleaning up and returning to original directory)")
    if (runner) {
      message("(stopping the runner)")
      px$kill()
    }
    setwd(owd)
    unlink(path, recursive = TRUE)
    if (set_cache_dir) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    }
    cache$drivers[["example"]] <- NULL
  }
  cleanup
}


example_driver <- function() {
  hipercow_driver(
    configure = example_configure,
    submit = example_submit,
    status = example_status,
    info = example_info,
    log = example_log,
    result = example_result,
    cancel = example_cancel,
    provision_run = example_provision_run,
    provision_list = example_provision_list,
    provision_compare = example_provision_compare,
    keypair = example_keypair,
    cluster_info = example_cluster_info)
}


example_configure <- function() {
  path_queue <- file.path(getwd(), "hipercow", "example.queue")
  if (!file.exists(path_queue)) {
    file.create(path_queue)
  }
  list()
}


example_submit <- function(id, resources, config, path_root) {
  append_lines(id, file.path(path_root, "hipercow", "example.queue"))
}


example_status <- function(id, config, path_root) {
  path_started <- file.path(path_root, "hipercow", "tasks", id,
                            "status-running")
  ifelse(file.exists(path_started), "running", "submitted")
}


example_info <- function(id, config, path_root) {
  path_started <- file.path(path_root, "hipercow", "tasks", id,
                            "status-running")
  time_started <- file.info(path_started)$ctime
  list(status = example_status(id, config, path_root),
       time_started = time_started)
}


example_log <- function(id, outer, config, path_root) {
  path <- file.path(path_root, "hipercow", "tasks", id,
                    if (outer) "log.outer" else "log")
  if (file.exists(path)) readLines(path) else NULL
}


example_result <- function(id, config, path_root) {
}


example_cancel <- function(id, config, path_root) {
  queue <- file.path(path_root, "hipercow", "example.queue")
  queued <- readLines(queue)
  writeLines(setdiff(queued, id), queue)
  cancelled <- id %in% queued
  time_started <- rep(NA, length(id))
  if (any(cancelled)) {
    time_started[cancelled] <-
      file.info(file.path(path_root, "hipercow", "tasks",
                          id[cancelled], "status-running"))$ctime
  }
  list(cancelled = cancelled, time_started = time_started)
}


example_provision_run <- function(args, config, path_root) {
  conan_config <- rlang::inject(conan2::conan_configure(
    !!!args,
    path = path_root,
    path_lib = file.path("hipercow", "lib"),
    path_bootstrap = .libPaths()[[1]]))
  withr::with_dir(path_root,
                  conan2::conan_run(conan_config, show_log = TRUE))
}


example_provision_list <- function(args, config, path_root) {
  if (is.null(args)) {
    hash <- NULL
  } else {
    hash <- rlang::inject(conan2::conan_configure(
      !!!args,
      path = path_root,
      path_lib = file.path("hipercow", "lib"),
      path_bootstrap = .libPaths()[[1]]))$hash
  }
  path_lib <- file.path(path_root, "hipercow", "lib")
  conan2::conan_list(path_lib, hash)
}


example_provision_compare <- function(curr, prev, config, path_root) {
  path_lib <- file.path(path_root, "hipercow", "lib")
  conan2::conan_compare(path_lib, curr, prev)
}


example_keypair <- function(config, path_root) {
  path_key <- file.path(path_root, "hipercow", "example", "key")
  if (file.exists(path_key)) {
    key <- openssl::read_key(path_key)
  } else {
    fs::dir_create(dirname(path_key))
    key <- openssl::rsa_keygen()
    openssl::write_pem(key, path_key)
  }
  list(pub = openssl::write_ssh(as.list(key)$pubkey),
       key = path_key)
}


example_cluster_info <- function(config, path_root) {
  list(max_ram = 1, max_cores = 1, queues = "default",
       nodes = "default", default_queue = "default")
}


example_runner <- function(path, with_logging, poll = 0.1) {
  withr::local_dir(path)
  withr::local_envvar(HIPERCOW_NO_DRIVERS = "1")
  path_queue <- file.path("hipercow", "example.queue")
  repeat {
    ids <- readLines(path_queue)
    if (length(ids) > 0) {
      writeLines(ids[-1], path_queue)
      if (with_logging) {
        example_run_with_logging(ids[[1]])
      } else {
        example_run(ids[[1]])
      }
    }
    Sys.sleep(poll)
  }
}

example_run <- function(id) {
  env <- new.env(parent = topenv())
  task_eval(id, envir = env, verbose = FALSE)
}

example_run_with_logging <- function(id) {
  path_task <- file.path("hipercow", "tasks", id)
  path_log <- file.path(path_task, "log")
  path_log_outer <- file.path(path_task, "log.outer")
  writeLines(sprintf("Running task %s", id), path_log_outer)
  callr::r(function(id) hipercow::task_eval(id, verbose = TRUE), list(id = id),
           stdout = path_log, stderr = path_log)
  append_lines(sprintf("Finished task %s", id), path_log_outer)
}
