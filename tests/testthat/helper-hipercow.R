init_quietly <- function(...) {
  suppressMessages(hipercow_init(...))
}


mock_pkg <- function() {
  list(
    make_configuration = function(...) {
      list(...)
    })
}


elsewhere_driver <- function() {
  hipercow_driver(
    configure = elsewhere_configure,
    submit = elsewhere_submit,
    status = elsewhere_status,
    info = elsewhere_info,
    log = elsewhere_log,
    result = elsewhere_result,
    cancel = elsewhere_cancel,
    provision_run = elsewhere_provision_run,
    provision_list = elsewhere_provision_list,
    provision_compare = elsewhere_provision_compare,
    keypair = elsewhere_keypair,
    check_hello = elsewhere_check_hello,
    cluster_info = elsewhere_cluster_info)
}


elsewhere_configure <- function(path, action = "queue") {
  if (!fs::dir_exists(file.path(path, "hipercow"))) {
    stop("Invalid path for 'elesewhere'; does not contain hipercow root")
  }
  list(path = path, action = action)
}


elsewhere_submit <- function(id, resources, config, path_root) {
  path <- config$path
  src <- path_to_task_file(path_root, id, "data")
  dest <- path_to_task_file(path, id, "data")
  fs::dir_create(dirname(dest))
  fs::file_copy(src, dest)
  if (config$action == "queue") {
    queue <- file.path(path, "elsewhere.queue")
    if (!file.exists(queue)) {
      file.create(queue)
    }
    ids <- c(readLines(queue), id)
    writeLines(ids, queue)
  } else if (config$action == "immediate") {
    hipercow::task_eval(id, envir = new.env(parent = topenv()),
                        root = path_root)
  }
}


elsewhere_status <- function(id, config, path_root) {
  status <- task_status(id, root = config$path)
  status[is.na(status) | status == "created"] <- "submitted"
  path_info_src <- path_to_task_file(config$path, id, "info")
  path_info_dst <- path_to_task_file(path_root, id, "info")
  i <- file.exists(path_info_src) & !file.exists(path_info_dst)
  if (any(i)) {
    file.copy(path_info_src[i], path_info_dst[i])
  }
  status
}


elsewhere_info <- function(id, config, path_root) {
  list(status = elsewhere_status(id, config, path_root))
}


elsewhere_log <- function(id, outer, config, path_root) {
  path <- path_to_task_file(config$path, id, "elsewhere_log")
  if (outer) {
    return("outer log")
  }
  if (!file.exists(path)) {
    NULL
  } else if (outer) {
    "outer log"
  } else {
    readLines(path)
  }
}


elsewhere_result <- function(id, config, path_root) {
  src <- path_to_task_file(config$path, id, "result")
  dst <- path_to_task_file(path_root, id, "result")
  file.copy(src, dst)
}


elsewhere_cancel <- function(id, config, path_root) {
  queue <- file.path(config$path, "elsewhere.queue")
  if (file.exists(queue)) {
    queued <- readLines(queue)
    writeLines(setdiff(queued, id), queue)
  } else {
    queued <- character()
  }
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


elsewhere_provision_run <- function(args, check_running_tasks, platform,
                                    config, path_root) {
  show_log <- args$show_log %||% FALSE
  args$show_log <- NULL
  path_bootstrap <- find_library_with("pkgdepends")
  conan_config <- rlang::inject(conan2::conan_configure(
    !!!args,
    path = path_root,
    path_lib = file.path("hipercow", "lib"),
    path_bootstrap = path_bootstrap))
  stopifnot(conan_config$method == "script")
  path_there <- config$path
  stopifnot(
    file.copy(file.path(path_root, conan_config$script),
              file.path(path_there, conan_config$script),
              overwrite = TRUE))

  withr::with_dir(path_there,
                  conan2::conan_run(conan_config, show_log = show_log))
}



elsewhere_provision_list <- function(args, config, path_root) {
  if (is.null(args)) {
    hash <- NULL
  } else {
    hash <- rlang::inject(conan2::conan_configure(
      !!!args,
      path = path_root,
      path_lib = file.path("hipercow", "lib"),
      path_bootstrap = .libPaths()[[1]]))$hash
  }
  path_lib <- file.path(config$path, "hipercow", "lib")
  conan2::conan_list(path_lib, hash)
}


elsewhere_provision_compare <- function(curr, prev, config, path_root) {
  path_lib <- file.path(config$path, "hipercow", "lib")
  conan2::conan_compare(path_lib, curr, prev)
}


elsewhere_keypair <- function(config, path_root) {
  path_key <- file.path(path_root, "hipercow", "elsewhere", "key")
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


elsewhere_check_hello <- function(config, path_root) {
}


elsewhere_register <- function() {
  ## This side-steps the logic in hipercow_driver_load(); after
  ## elsewhere_register() has been called, then `hipercow_driver_load`
  ## will fetch the correct driver without dealing with the package
  ## logic
  cache$drivers[["elsewhere"]] <- elsewhere_driver()
}


clear_cached_drivers <- function() {
  if (!is.null(cache$drivers)) {
    rm(list = "drivers", envir = cache)
  }
}

clear_cached_roots <- function() {
  if (!is.null(cache$roots)) {
    rm(list = "roots", envir = cache)
  }
}

elsewhere_cluster_info <- function(config, platform, path_root) {
  resources <- list(max_ram = 16, max_cores = 8, queues = c("Aldi", "Tesco"),
                    nodes = c("kevin", "stuart"), default_queue = "Aldi")
  redis_url <- NULL
  r_versions <- getRversion()
  list(resources = resources, r_versions = r_versions, redis_url = redis_url)
}


## Little helper for tests now that we no longer have the raw path to
## work with.
path_to_task_file <- function(path_root, id, file = NULL) {
  if (is.null(file)) {
    file.path(path_root, "hipercow", "tasks", path_task_split(id))
  } else {
    file.path(path_root, "hipercow", "tasks", path_task_split(id), file)
  }
}


has_redis <- function() {
  if (is.null(cache$has_redis)) {
    cache$has_redis <- tryCatch({
      redux::hiredis()$PING()
      TRUE
    },
    error = function(e) FALSE)
  }
  cache$has_redis
}


skip_if_no_redis <- function() {
  skip_on_cran()
  testthat::skip_if_not(has_redis(), "redis not available")
}


launch_example_workers <- function(path, n = 1, envir = parent.frame()) {
  args <- list(path = path, with_logging = FALSE)
  px <- lapply(seq_len(n), function(i) {
    callr::r_bg(example_runner, args, package = TRUE,
                stdout = NULL, stderr = NULL)
  })
  withr::defer({
    for (p in px) {
      p$kill()
    }
  }, envir)
  invisible(px)
}
