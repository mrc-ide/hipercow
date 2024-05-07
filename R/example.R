##' A helper used in running examples and docs.  This function will
##' change your working directory into a new temporary hipercow root,
##' and start a worker process that will quietly run tasks.  This is
##' not intended for people to use outside of running examples!
##'
##' @title Example helper
##'
##' @param runner Start a runner?  If `TRUE` (the default) we start a
##'   background process with `callr::r_bg` that will pick tasks off a
##'   queue and run them.  Or pass an integer, and start more than one
##'   runner.
##'
##' @param with_logging Run each task with logging; this is quite a
##'   bit slower, but enables examples that use [task_log_show] etc.
##'   Only has an effect if `runner` is `TRUE`.
##'
##' @param new_directory Create new empty (temporary) directory?  If
##'   `FALSE` then we just use the current directory.  This is used in
##'   the vignettes where the directory is set already.
##'
##' @param initialise Initialise? If `FALSE` then no initialisation
##'   is done.  This is intended for examples that will use
##'   [hipercow_init()] later.
##'
##' @return A function that can be called (with no arguments) to
##'   return to the original working directory and clean up all files
##'   created for the example.
##'
##' @export
##' @keywords internal
hipercow_example_helper <- function(runner = TRUE,
                                    with_logging = FALSE,
                                    new_directory = TRUE,
                                    initialise = TRUE) {
  cli::cli_alert_info("This example uses a special helper")
  if (new_directory) {
    path <- tempfile()
  } else {
    path <- getwd()
  }
  if (initialise) {
    suppressMessages(hipercow_init(path, driver = "example"))
  }
  if (new_directory) {
    owd <- setwd(normalize_path(path))
  }

  if (is.logical(runner)) {
    runner <- as.integer(runner)
  }
  if (runner > 0) {
    args <- list(path, with_logging)
    px <- lapply(seq_len(runner), function(i) {
      callr::r_bg(example_runner, args, package = TRUE,
                  stdout = NULL, stderr = NULL)
    })
  }

  ## Set required envvar for pkgdepends (see conan2's setup.R which
  ## does the same for the test suite).
  set_cache_dir <- is.na(Sys.getenv("R_USER_CACHE_DIR", NA))
  if (set_cache_dir) {
    Sys.setenv(R_USER_CACHE_DIR = tempfile())
  }

  cleanup <- function() {
    cli::cli_alert_info("Cleaning up example")
    if (runner > 0) {
      for (p in px) {
        p$kill()
      }
    }
    if (new_directory) {
      setwd(owd)
      unlink(path, recursive = TRUE)
    }
    if (set_cache_dir) {
      Sys.unsetenv("R_USER_CACHE_DIR")
    }
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
    check_hello = example_check_hello,
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
  id_head <- substr(id, 1, 2)
  id_tail <- substr(id, 3, nchar(id))
  path_started <- file.path(path_root, "hipercow", "tasks", id_head, id_tail,
                            "status-running")
  ifelse(file.exists(path_started), "running", "submitted")
}


example_info <- function(id, config, path_root) {
  id_head <- substr(id, 1, 2)
  id_tail <- substr(id, 3, nchar(id))
  path_started <- file.path(path_root, "hipercow", "tasks", id_head, id_tail,
                            "status-running")
  time_started <- file.info(path_started)$ctime
  list(status = example_status(id, config, path_root),
       time_started = time_started)
}


example_log <- function(id, outer, config, path_root) {
  id_head <- substr(id, 1, 2)
  id_tail <- substr(id, 3, nchar(id))
  path <- file.path(path_root, "hipercow", "tasks", id_head, id_tail,
                    if (outer) "log.outer" else "log")
  if (file.exists(path)) readLines(path) else NULL
}


example_result <- function(id, config, path_root) {
}


example_cancel <- function(id, config, path_root) {
  ## TODO: find any running statuses, and then kill them
  queue <- file.path(path_root, "hipercow", "example.queue")
  queued <- readLines(queue)
  writeLines(setdiff(queued, id), queue)
  cancelled <- id %in% queued
  time_started <- rep(NA, length(id))
  if (any(cancelled)) {
    id_head <- substr(id[cancelled], 1, 2)
    id_tail <- substr(id[cancelled], 3, nchar(id))
    time_started[cancelled] <-
      file.info(file.path(path_root, "hipercow", "tasks",
                          id_head, id_tail, "status-running"))$ctime
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


example_check_hello <- function(config, path_root) {
}


example_cluster_info <- function(config, path_root) {
  resources <- list(max_ram = 4, max_cores = 4,
                    queues = c("alltasks", "bigmem", "fast"),
                    nodes = c("node-1", "node-2", "gpu-3", "gpu-4"),
                    default_queue = "alltasks")
  redis_url <- "127.0.0.1:6379"
  r_versions <- getRversion()
  list(resources = resources, r_versions = r_versions, redis_url = redis_url)
}


example_runner <- function(path, with_logging, poll = 0.1) {
  ## No coverage on this; it's an infinite loop!
  ## nocov start
  repeat {
    example_step(path, with_logging, poll)
  }
  ## nocov end
}


example_step <- function(path, with_logging, poll) {
  path_queue <- file.path(path, "hipercow", "example.queue")
  ids <- readlines_if_exists(path_queue)
  if (length(ids) > 0) {
    writeLines(ids[-1], path_queue)
    id <- ids[[1]]
    withr::local_dir(path)
    withr::local_envvar(HIPERCOW_NO_DRIVERS = "1",
                        HIPERCOW_CORES = example_cores(id))
    if (with_logging) {
      example_run_with_logging(id)
    } else {
      example_run(id)
    }
  } else {
    Sys.sleep(poll)
  }
}

example_run <- function(id) {
  env <- new.env(parent = topenv())
  task_eval(id, envir = env, verbose = rlang::is_interactive())
}

example_run_with_logging <- function(id) {
  id_head <- substr(id, 1, 2)
  id_tail <- substr(id, 3, nchar(id))
  path_task <- file.path("hipercow", "tasks", id_head, id_tail)
  path_log <- file.path(path_task, "log")
  path_log_outer <- file.path(path_task, "log.outer")
  writeLines(sprintf("Running task %s", id), path_log_outer)
  ## This changes to r_bg, then get the pid and write it, then block.
  callr::r(function(id) hipercow::task_eval(id, verbose = TRUE), list(id = id),
           stdout = path_log, stderr = path_log)
  append_lines(sprintf("Finished task %s", id), path_log_outer)
}

example_cores <- function(id) {
  id_head <- substr(id, 1, 2)
  id_tail <- substr(id, 3, nchar(id))
  resources <- readRDS(file.path("hipercow", "tasks", id_head, id_tail,
                                 RESOURCES))
  if (resources$cores == Inf) {
    example_cluster_info(NULL, getwd())$resources$max_cores
  } else {
    resources$cores
  }
}
