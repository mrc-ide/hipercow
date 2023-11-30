##' Create an explicit task. Explicit tasks are the simplest sort of
##' task in hermod and do nothing magic. They accept an R expression
##' (from `quote` or friends) and possibly a set of variables to
##' export from the global environment.  This can then be run on a
##' cluster by loading your variables and running your expression.  If
##' your expression depends on packages being *attached* then you
##' should pass a vector of package names too.
##'
##' @title Create explicit task
##'
##' @param expr Unevaluated expression object, e.g., from `quote`
##'
##' @param export Optional character vector of names of objects to
##'   export into the evaluating environment
##'
##' @param envir Environment in which to find variables for `export`
##'
##' @param packages Optional character vector of packages to attach
##'   before the expression is run. Likely needed for things like
##'   ggplot and dplyr which make use of exported functions.
##'
##' @inheritParams hermod_task_eval
##'
##' @return A task id, a string of hex characters. Use this to
##'   interact with the task.
##'
##' @export
hermod_task_create_explicit <- function(expr, export = NULL,
                                        envir = .GlobalEnv, packages = NULL,
                                        root = NULL) {
  root <- hermod_root(root)
  id <- ids::random_id()
  dest <- file.path(root$path$tasks, id)
  dir.create(dest, FALSE, TRUE)
  if (is.null(export)) {
    locals <- NULL
  } else {
    locals <- set_names(lapply(export, get, envir = envir), export)
  }
  path <- relative_workdir(root$path$root)
  data <- list(type = "explicit",
               id = id,
               expr = expr,
               locals = locals,
               path = path,
               packages = packages)
  saveRDS(data, file.path(dest, EXPR))
  file.create(file.path(dest, STATUS_CREATED))
  id
}


##' Run a task that has been created by a `hermod_task_create_*`
##' function, e.g., [hermod_task_create_explicit()]
##'
##' @title Run a task
##'
##' @param id The task identifier
##'
##' @param envir An environment in which to evaluate the
##'   expression. For non-testing purposes, generally ignore this, the
##'   global environment will be likely the expected environment.
##'
##' @param root A hermod root, or path to it. If `NULL` we search up
##'   your directory tree.
##'
##' @return Boolean indicating success (`TRUE`) or failure (`FALSE`)
##' @export
hermod_task_eval <- function(id, envir = .GlobalEnv, root = NULL) {
  root <- hermod_root(root)
  path <- file.path(root$path$tasks, id)
  if (file.exists(file.path(path, STATUS_STARTED))) {
    ## TODO: we could report more about when it was started?
    cli::cli_abort("Task '{id}' has already been started")
  }
  file.create(file.path(path, STATUS_STARTED))
  data <- readRDS(file.path(path, EXPR))

  top <- rlang::current_env() # not quite right, but better than nothing
  local <- new.env(parent = emptyenv())
  withr::local_dir(file.path(root$path$root, data$path))
  result <- rlang::try_fetch(
    switch(
      data$type,
      explicit = task_eval_explicit(data, envir, root),
      cli::cli_abort("Tried to evaluate unknown type of task {data$type}")),
    error = function(e) {
      if (is.null(e$trace)) {
        e$trace <- rlang::trace_back(top)
      }
      local$error <- e
      NULL
    })

  success <- is.null(local$error)
  if (success) {
    status <- STATUS_SUCCESS
  } else {
    result <- local$error
    status <- STATUS_FAILURE
  }
  saveRDS(result, file.path(path, RESULT))
  file.create(file.path(path, status))

  success
}


##' Get the status of a task. See Details for the lifecycle.
##'
##' A task passes through a lifecycle:
##'
##' * `created`
##' * `submitted`
##' * `started`
##' * `success`, `failure`, `cancelled`
##'
##' These occur in increasing order and the result of this function is
##' the furtherst through this list.
##'
##' Later, we will introduce other types to cope with tasks that have
##' been retried, or that are blocked on dependencies (or have become
##' impossible).
##'
##' @title Get task status
##'
##' @param id The task identifier
##'
##' @inheritParams hermod_task_eval
##'
##' @return A string with the task status
##' @export
hermod_task_status <- function(id, root = NULL) {
  root <- hermod_root(root)
  path <- file.path(root$path$tasks, id)
  re <- "^status-"
  status <- dir(path, pattern = re)
  status <- max(c(match(status, STATUS), 0))
  if (status == 0) "missing" else sub(re, "", STATUS[status])
}


##' Get the task result. This might be an error if the task has failed.
##'
##' @title Get task result
##'
##' @inheritParams hermod_task_status
##'
##' @return The value of the queued expression
##' @export
hermod_task_result <- function(id, root = NULL) {
  root <- hermod_root(root)
  path <- file.path(root$path$tasks, id)
  path_result <- file.path(path, RESULT)
  if (!file.exists(path_result)) {
    status <- hermod_task_status(id, root)
    cli::cli_abort("Result for task '{id}' not available, status is '{status}'")
  }
  readRDS(path_result)
}


task_eval_explicit <- function(data, envir, root) {
  for (p in data$packages) {
    library(p, character.only = TRUE)
  }
  if (!is.null(data$locals)) {
    list2env(data$locals, envir)
  }
  eval(data$expr, envir)
}


relative_workdir <- function(root_path, call = NULL) {
  workdir <- normalize_path(getwd())
  if (!fs::path_has_parent(workdir, root_path)) {
    cli::cli_abort(
      c("Working directory is not a subdirectory of the hermod root",
        i = "Working: {workdir}",
        i = "Hermod: {root_path}"),
      call = call)
  }
  as.character(fs::path_rel(workdir, root_path))
}
