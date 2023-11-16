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
  data <- list(type = "explicit",
               id = id,
               expr = expr,
               locals = locals,
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
  result <- switch(
    data$type,
    explicit = task_eval_explicit(data, envir, root),
    cli::cli_abort("Tried to evaluate unknown type of task {data$type}"))
  saveRDS(result$value, file.path(path, RESULT))
  file.create(
    file.path(path, if (result$success) STATUS_SUCCESS else STATUS_FAILURE))
  result$success
}


hermod_task_status <- function(id, root = NULL) {
  root <- hermod_root(root)
  path <- file.path(root$path$tasks, id)
  re <- "^status-"
  status <- dir(path, pattern = re)
  status <- max(c(match(status, STATUS), 0))
  if (status == 0) "missing" else sub(re, "", STATUS[status])
}


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
  tryCatch({
    result <- eval(data$expr, envir)
    list(success = TRUE, value = result)
  }, error = function(e) list(success = FALSE, value = e))
}
