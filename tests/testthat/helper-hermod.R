init_quietly <- function(...) {
  suppressMessages(hermod_init(...))
}


mock_pkg <- function() {
  list(
    make_configuration = function(...) {
      list(...)
    })
}


elsewhere_driver <- function() {
  hermod_driver(
    configure = elsewhere_configure,
    submit = elsewhere_submit,
    status = elsewhere_status,
    result = elsewhere_result,
    cancel = elsewhere_cancel,
    provision = elsewhere_provision)
}


elsewhere_configure <- function(path, immediate = FALSE) {
  if (!file.exists(file.path(path, "hermod.json"))) {
    stop("Invalid path for 'elesewhere'; does not contain hermod root")
  }
  list(path = path, immediate = immediate)
}


elsewhere_submit <- function(id, config, path_root) {
  path <- config$path
  src <- file.path(path_root, "hermod", "tasks", id, "expr")
  dest <- file.path(path, "hermod", "tasks", id, "expr")
  fs::dir_create(dirname(dest))
  fs::file_copy(src, dest)
  if (config$immediate) {
    px <- withr::with_dir(
      config$path,
      callr::r_bg(function(id) hermod::hermod_task_eval(id), list(id),
                  cleanup = FALSE))
    writeLines(as.character(px$get_pid()), file.path(dirname(dest), "pid"))
  } else {
    queue <- file.path(path, "elsewhere.queue")
    if (!file.exists(queue)) {
      file.create(queue)
    }
    ids <- c(readLines(queue), id)
    writeLines(ids, queue)
  }}


elsewhere_status <- function(id, config, path_root) {
  status <- hermod_task_status(id, root = config$path)
  status[is.na(status)] <- "submitted"
  if (config$immediate) {
    path_pid <- file.path(config$path, "hermod", "tasks", id, "pid")
    pids <- as.integer(vcapply(path_pid, readLines))
    status[pids %in% ps::ps_pids()] <- "started"
  }
  status
}


elsewhere_result <- function(id, config, path_root) {
  src <- file.path(config$path, "hermod", "tasks", id, "result")
  dst <- file.path(path_root, "hermod", "tasks", id, "result")
  file.copy(src, dst)
}


elsewhere_cancel <- function(id, config, path_root) {
  killed <- rep(FALSE, length(id))
  if (config$immediate) {
    path_pid <- file.path(config$path, "hermod", "tasks", id, "pid")
    for (i in seq_along(id)) {
      h <- tryCatch(ps::ps_handle(as.integer(readLines(path_pid[[i]]))),
                    error = function(e) NULL)
      if (ps::ps_is_running(h)) {
        ps::ps_kill(h)
        killed[[i]] <- TRUE
      }
    }
  }
  killed
}


elsewhere_provision <- function(method, config, path_root, environment, ...) {
  conan_config <- conan2::conan_configure(
    method,
    path = path_root,
    path_lib = file.path("hermod", "lib"),
    path_bootstrap = .libPaths()[[1]],
    environment = environment,
    ...)
  stopifnot(conan_config$method == "script")
  path_there <- config$path
  stopifnot(
    file.copy(file.path(path_root, conan_config$script),
              file.path(path_there, conan_config$script),
              overwrite = TRUE))
  withr::with_dir(path_there, conan2::conan_run(conan_config))
}


elsewhere_register <- function() {
  ## This side-steps the logic in hermod_driver_load(); after
  ## elsewhere_register() has been called, then `hermod_driver_load`
  ## will fetch the correct driver without dealing with the package
  ## logic
  cache$drivers[["elsewhere"]] <- elsewhere_driver()
}


clear_drivers <- function() {
  if (!is.null(cache$drivers)) {
    rm(list = "drivers", envir = cache)
  }
}


wait_until_status <- function(id, wanted, root = NULL, time = 5, poll = 0.05) {
  if (length(wanted) == 1 && wanted == "terminal") {
    wanted <- c("success", "failure", "cancelled")
  }
  t_end <- Sys.time() + time
  repeat {
    status <- hermod_task_status(id, root = root)
    if (status %in% wanted) {
      return(status)
    }
    if (Sys.time() > t_end) {
      stop("Status did not change in time")
    }
    Sys.sleep(poll)
  }
}
