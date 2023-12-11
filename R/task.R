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
##' @param envir Local R environment in which to find variables for
##'   `export`
##'
##' @param environment Name of the hermod environment to evaluate the
##'   task within.
##'
##' @inheritParams hermod_task_eval
##'
##' @return A task id, a string of hex characters. Use this to
##'   interact with the task.
##'
##' @export
hermod_task_create_explicit <- function(expr, export = NULL, envir = .GlobalEnv,
                                        environment = "default", root = NULL) {
  root <- hermod_root(root)
  locals <- task_locals(export, envir)

  ensure_environment_exists(environment, root, rlang::current_env())
  path <- relative_workdir(root$path$root)

  id <- ids::random_id()
  dest <- file.path(root$path$tasks, id)
  dir.create(dest, FALSE, TRUE)

  data <- list(type = "explicit",
               id = id,
               expr = expr,
               locals = locals,
               path = path,
               environment = environment)
  saveRDS(data, file.path(dest, EXPR))
  file.create(file.path(dest, STATUS_CREATED))
  id
}


##' Create a task based on an expression. This is similar to
##' [hermod_task_create_explicit] except more magic, and is closer to
##' the interface that we expect people will use.
##'
##' @title Create a task based on an expression
##'
##' @param expr The expression, does not need quoting.
##'
##' @inheritParams hermod_task_create_explicit
##'
##' @inherit hermod_task_create_explicit return
##' @export
hermod_task_create_expression <- function(expr, environment = "default",
                                          root = NULL) {
  root <- hermod_root(root)

  quo <- rlang::enquo(expr)
  if (rlang::quo_is_symbol(quo)) {
    sym <- rlang::as_name(rlang::quo_get_expr(quo))
    envir <- rlang::caller_env() # or is it rlang::quo_get_expr(quo) perhaps?
    if (!rlang::env_has(envir, sym, inherit = TRUE)) {
      cli::cli_abort("Could not find expression '{sym}'")
    }
    expr <- rlang::env_get(envir, sym, inherit = TRUE)
    if (!rlang::is_call(expr)) {
      cli::cli_abort(c(
        "Expected 'expr' to be a function call",
        i = paste("You passed a symbol '{sym}', but that turned out to be",
                  "an object of type {typeof(expr)} and not a call")))
    }
  } else {
    if (!rlang::quo_is_call(quo)) {
      cli::cli_abort("Expected 'expr' to be a function call")
    }
    envir <- rlang::quo_get_env(quo)
    expr <- rlang::quo_get_expr(quo)
  }

  if (rlang::is_call(expr, "quote")) {
    given <- rlang::expr_deparse(expr)
    alt <- rlang::expr_deparse(expr[[2]])
    cli::cli_abort(
      c("You have an extra layer of quote() around 'expr'",
        i = "You passed '{given}' but probably meant to pass '{alt}'"))
  }

  locals <- task_locals(all.vars(expr), envir)

  ensure_environment_exists(environment, root, rlang::current_env())
  path <- relative_workdir(root$path$root)

  id <- ids::random_id()
  dest <- file.path(root$path$tasks, id)
  dir.create(dest, FALSE, TRUE)

  data <- list(type = "expression",
               id = id,
               expr = expr,
               locals = locals,
               path = path,
               environment = environment)
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

  result <- rlang::try_fetch({
    environment_apply(data$environment, envir, root, top)
    withr::local_dir(file.path(root$path$root, data$path))
    switch(
      data$type,
      explicit = task_eval_explicit(data, envir, root),
      expression = task_eval_expression(data, envir, root),
      cli::cli_abort("Tried to evaluate unknown type of task '{data$type}'"))
  }, error = function(e) {
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
##' @return A string with the task status. Tasks that do not exist
##'   will have a status of `NA`.
##'
##' @export
hermod_task_status <- function(id, root = NULL) {
  ## This function is fairly complicated because we try to do as
  ## little work as possible; querying the network file system is
  ## fairly expensive and we assume that hitting the underlying driver
  ## might be costly too (either an http request for windows or a ssh
  ## exec on linux). We also need this to be vectorised over a number
  ## of tasks so that we can take advantage of using a single request
  ## to the remote driver.
  ##
  ## Once this works, we can probably split it into several pieces,
  ## but that could just make it harder to follow?
  root <- hermod_root(root)
  assert_character(id)
  if (length(id) == 0) {
    return(character(0))
  }

  status <- rep(NA_character_, length(id))

  ## Fastest possible exit; we know that this task has a terminal
  ## status so we return it from the cache
  i <- match(id, names(root$cache$task_status_terminal))
  j <- !is.na(i)
  status[j] <- unname(root$cache$task_status_terminal[i[j]])
  i <- is.na(status)
  if (!any(i)) {
    return(status)
  }

  terminal <- c(success = STATUS_SUCCESS,
                failure = STATUS_FAILURE,
                cancelled = STATUS_CANCELLED)

  ## Next, check to see if we have a terminal status for each
  ## task. This will be the case (with the above exit being missed) in
  ## a different session, or if the status was written by a different
  ## session.
  path <- file.path(root$path$tasks, id)
  for (s in names(terminal)) {
    if (any(j <- file.exists(file.path(path[i], terminal[[s]])))) {
      status[i][j] <- s
      i <- is.na(status)
    }
  }

  if (any(i)) {
    task_driver <- vcapply(id[i], hermod_task_driver, root = root)
    for (driver in unique(na_omit(task_driver))) {
      dat <- hermod_driver_prepare(driver, root, rlang::current_env())
      j <- task_driver == driver
      status_ij <- dat$driver$status(id[i][j], dat$config, root$path$root)
      for (s in names(terminal)) {
        if (any(k <- !is.na(status_ij) & status_ij == s)) {
          file.create(file.path(path[i][j][k], terminal[[s]]))
        }
      }
      status[i][j] <- status_ij
    }

    ## Final set were not submitted; these must be on disk only and we
    ## know that they are not in a terminal state:
    i <- is.na(status)
    if (any(i)) {
      for (s in c(STATUS_STARTED, STATUS_CREATED)) {
        if (any(j <- file.exists(file.path(path[i], s)))) {
          status[i][j] <- sub("status-", "", s)
          i <- is.na(status)
        }
      }
    }
  }

  ## Finally, we update the terminal status cache, will make things
  ## faster next time around.
  i <- setdiff(id[status %in% names(terminal)],
               names(root$cache$task_status_terminal))
  root$cache$task_status_terminal[i] <- status[match(i, id)]

  status
}


##' Return the driver used by a task. This can't be changed once set.
##'
##' @title Return driver used by a task
##'
##' @inheritParams hermod_task_status
##'
##' @return A string, `NA` if no driver used.
##'
##' @export
hermod_task_driver <- function(id, root = NULL) {
  root <- hermod_root(root)
  if (id %in% names(root$cache$task_driver)) {
    return(root$cache$task_driver[[id]])
  }

  path <- file.path(root$path$tasks, id, STATUS_SUBMITTED)
  if (!file.exists(path)) {
    return(NA_character_)
  }

  driver <- readLines(path)
  root$cache$task_driver[[id]] <- driver

  driver
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
    task_driver <- vcapply(id, hermod_task_driver, root = root)
    if (is.na(task_driver) || !(status %in% c("success", "failure"))) {
      cli::cli_abort(
        "Result for task '{id}' not available, status is '{status}'")
    }
    dat <- hermod_driver_prepare(task_driver, root, environment())
    dat$driver$result(id, dat$config, root$path$root)
  }
  readRDS(path_result)
}


##' Cancel one or more tasks
##'
##' @title Cancel tasks
##'
##' @param id The id or ids to cancel
##'
##' @inheritParams hermod_task_status
##'
##' @return A logical vector the same length as `id` indicating if the
##'   task was cancelled. This will be `FALSE` if the job was already
##'   completed, not running, etc.
##'
##' @export
hermod_task_cancel <- function(id, root = NULL) {
  root <- hermod_root(root)
  result <- rep(FALSE, length(id))
  status <- hermod_task_status(id, root)
  i <- status %in% c("submitted", "running")
  if (any(i)) {
    task_driver <- vcapply(id, hermod_task_driver, root = root)
    for (driver in unique(na_omit(task_driver))) {
      dat <- hermod_driver_prepare(task_driver, root, environment())
      j <- task_driver == driver
      result[i][j] <- dat$driver$cancel(id[i][j], dat$config, root$path$root)
    }
    file.create(file.path(root$path$tasks, id[result], STATUS_CANCELLED))
  }
  result
}


task_eval_explicit <- function(data, envir, root) {
  if (!is.null(data$locals)) {
    list2env(data$locals, envir)
  }
  eval(data$expr, envir)
}


task_eval_expression <- function(data, envir, root) {
  rlang::env_bind(envir, !!!data$locals)
  ## It's possible that we need to use rlang::eval_tidy() here, see
  ## the help page for an example.  It does depend on how much we want
  ## to export though.
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


task_locals <- function(names, envir) {
  if (length(names) == 0) {
    NULL
  } else {
    rlang::env_get_list(envir, names, inherit = TRUE, last = topenv())
  }
}
