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
    cluster_info = elsewhere_cluster_info)
}


elsewhere_configure <- function(path, action = "queue") {
  if (!fs::dir_exists(file.path(path, "hipercow"))) {
    stop("Invalid path for 'elesewhere'; does not contain hipercow root")
  }
  list(path = path, action = action)
}


elsewhere_submit <- function(id, config, path_root) {
  path <- config$path
  src <- file.path(path_root, "hipercow", "tasks", id, "expr")
  dest <- file.path(path, "hipercow", "tasks", id, "expr")
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
  path_info_src <- file.path(config$path, "hipercow", "tasks", id, "info")
  path_info_dst <- file.path(path_root, "hipercow", "tasks", id, "info")
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
  path <- file.path(config$path, "hipercow", "tasks", id, "elsewhere_log")
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
  src <- file.path(config$path, "hipercow", "tasks", id, "result")
  dst <- file.path(path_root, "hipercow", "tasks", id, "result")
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
    time_started[cancelled] <-
      file.info(file.path(path_root, "hipercow", "tasks",
                          id[cancelled], "status-running"))$ctime
  }
  list(cancelled = cancelled, time_started = time_started)
}


elsewhere_provision_run <- function(args, config, path_root) {
  show_log <- args$show_log %||% FALSE
  args$show_log <- NULL
  conan_config <- rlang::inject(conan2::conan_configure(
    !!!args,
    path = path_root,
    path_lib = file.path("hipercow", "lib"),
    path_bootstrap = .libPaths()[[1]]))
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


elsewhere_register <- function() {
  ## This side-steps the logic in hipercow_driver_load(); after
  ## elsewhere_register() has been called, then `hipercow_driver_load`
  ## will fetch the correct driver without dealing with the package
  ## logic
  cache$drivers[["elsewhere"]] <- elsewhere_driver()
}


clear_drivers <- function() {
  if (!is.null(cache$drivers)) {
    rm(list = "drivers", envir = cache)
  }
}

elsewhere_cluster_info <- function(config, path_root) {
  list(max_ram = 16, max_cores = 8, queues = c("Aldi", "Tesco"),
       nodes = c("kevin", "stuart"))
}
