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
    provision = elsewhere_provision)
}


elsewhere_configure <- function(path) {
  if (!file.exists(file.path(path, "hermod.json"))) {
    stop("Invalid path for 'elesewhere'; does not contain hermod root")
  }
  list(path = path)
}


elsewhere_submit <- function(id, config, path_root) {
  path <- config$path
  src <- file.path(path_root, "hermod", "tasks", id, "expr")
  dest <- file.path(path, "hermod", "tasks", id, "expr")
  fs::dir_create(dirname(dest))
  fs::file_copy(src, dest)
  queue <- file.path(path, "elsewhere.queue")
  if (!file.exists(queue)) {
    file.create(queue)
  }
  ids <- c(readLines(queue), id)
  writeLines(ids, queue)
}


elsewhere_status <- function(id, config, path_root) {
  ## Once we rework this to use callr, this might hit the process id?
  status <- hermod_task_status(id, root = config$path)
  ## this is really the worst we can do:
  status[is.na(status)] <- "submitted"
  status
}


elsewhere_provision <- function(method, config, path_root, ...) {
  conan_config <- conan::conan_configure(
    method,
    path = path_root,
    path_lib = file.path("hermod", "lib"),
    path_bootstrap = .libPaths()[[1]],
    ...)
  stopifnot(conan_config$method == "script")
  stopifnot(
    file.copy(file.path(path_root, conan_config$script),
              file.path(config$path, conan_config$script),
              overwrite = TRUE))
  withr::with_dir(config$path, conan::conan_run(conan_config))
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
