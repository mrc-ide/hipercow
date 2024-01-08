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
    log = elsewhere_log,
    result = elsewhere_result,
    cancel = elsewhere_cancel,
    provision = elsewhere_provision)
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
  path_info <- file.path(config$path, "hipercow", "tasks", id, "info")
  info <- lapply(path_info, readrds_if_exists)
  list(status = status, info = info)
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
    file.create(
      file.path(config$path, "hipercow", "tasks", intersect(id, queued),
                "status-cancelled"))
  } else {
    queued <- character()
  }
  id %in% queued
}


elsewhere_provision <- function(method, config, path_root, environment, ...,
                                show_log = FALSE) {
  conan_config <- conan2::conan_configure(
    method,
    path = path_root,
    path_lib = file.path("hipercow", "lib"),
    path_bootstrap = .libPaths()[[1]],
    environment = environment,
    ...)
  stopifnot(conan_config$method == "script")
  path_there <- config$path
  stopifnot(
    file.copy(file.path(path_root, conan_config$script),
              file.path(path_there, conan_config$script),
              overwrite = TRUE))
  withr::with_dir(path_there,
                  conan2::conan_run(conan_config, show_log = show_log))
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
