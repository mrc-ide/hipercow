##' Create an explicit task. Explicit tasks are the simplest sort of
##' task in hipercow and do nothing magic. They accept an R expression
##' (from `quote` or friends) and possibly a set of variables to
##' export from the global environment.  This can then be run on a
##' cluster by loading your variables and running your expression.  If
##' your expression depends on packages being *attached* then you
##' should pass a vector of package names too.  This function may
##' disappear, and is used by us to think about the package, it's not
##' designed to really be used.
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
##' @param environment Name of the hipercow environment to evaluate the
##'   task within.
##'
##' @param submit Control over task submission. This will expand over
##'   time once we support specifying resources. The most simple
##'   interface is to use `TRUE` here to automatically submit a task,
##'   using your default configuration, or `FALSE` to prevent
##'   submission.  The default `NULL` will submit a task if a driver
##'   is configured.
##'
##' @inheritParams task_eval
##'
##' @return A task id, a string of hex characters. Use this to
##'   interact with the task.
##'
##' @export
task_create_explicit <- function(expr, export = NULL, envir = .GlobalEnv,
                                 environment = "default", submit = NULL,
                                 root = NULL) {
  root <- hipercow_root(root)

  variables <- task_variables(export, envir, environment, root,
                              rlang::current_env())

  path <- relative_workdir(root$path$root)

  id <- ids::random_id()
  dest <- file.path(root$path$tasks, id)
  dir.create(dest, FALSE, TRUE)

  data <- list(type = "explicit",
               id = id,
               expr = expr,
               variables = variables,
               path = path,
               environment = environment)
  saveRDS(data, file.path(dest, EXPR))
  file.create(file.path(dest, STATUS_CREATED))

  task_submit_maybe(id, submit, root, rlang::current_env())

  id
}


##' Create a task based on an expression. This is similar to
##' [task_create_explicit] except more magic, and is closer to
##' the interface that we expect people will use.
##'
##' @title Create a task based on an expression
##'
##' @param expr The expression, does not need quoting.
##'
##' @inheritParams task_create_explicit
##'
##' @inherit task_create_explicit return
##' @export
task_create_expr <- function(expr, environment = "default", submit = NULL,
                             root = NULL) {
  root <- hipercow_root(root)

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

  variables <- task_variables(all.vars(expr), envir, environment, root,
                              rlang::current_env())
  path <- relative_workdir(root$path$root)

  id <- ids::random_id()
  dest <- file.path(root$path$tasks, id)
  dir.create(dest, FALSE, TRUE)

  data <- list(type = "expression",
               id = id,
               expr = expr,
               variables = variables,
               path = path,
               environment = environment)
  saveRDS(data, file.path(dest, EXPR))
  file.create(file.path(dest, STATUS_CREATED))

  task_submit_maybe(id, submit, root, rlang::current_env())

  id
}


##' Run a task that has been created by a `task_create_*` function,
##' e.g., [task_create_explicit()], [task_create_expr()]. Generally
##' users should not run this function directly.
##'
##' @title Run a task
##'
##' @param id The task identifier
##'
##' @param envir An environment in which to evaluate the
##'   expression. For non-testing purposes, generally ignore this, the
##'   global environment will be likely the expected environment.
##'
##' @param root A hipercow root, or path to it. If `NULL` we search up
##'   your directory tree.
##'
##' @return Boolean indicating success (`TRUE`) or failure (`FALSE`)
##' @export
task_eval <- function(id, envir = .GlobalEnv, root = NULL) {
  root <- hipercow_root(root)
  path <- file.path(root$path$tasks, id)
  status <- task_status(id, root = root)
  if (status %in% c("running", "success", "failure", "cancelled")) {
    cli::cli_abort("Can't start task '{id}', which has status '{status}'")
  }
  file.create(file.path(path, STATUS_RUNNING))
  data <- readRDS(file.path(path, EXPR))

  top <- rlang::current_env() # not quite right, but better than nothing
  local <- new.env(parent = emptyenv())

  result <- rlang::try_fetch({
    environment_apply(data$environment, envir, root, top)
    check_globals(data$variables$globals, envir, top)
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
##' * `running`
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
##' @inheritParams task_eval
##'
##' @return A string with the task status. Tasks that do not exist
##'   will have a status of `NA`.
##'
##' @export
task_status <- function(id, root = NULL) {
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
  root <- hipercow_root(root)
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
    task_driver <- vcapply(id[i], task_get_driver, root = root)
    for (driver in unique(na_omit(task_driver))) {
      dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
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
      for (s in c(STATUS_RUNNING, STATUS_CREATED)) {
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


task_get_driver <- function(id, root = NULL) {
  root <- hipercow_root(root)
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
##' @inheritParams task_status
##'
##' @return The value of the queued expression
##' @export
task_result <- function(id, root = NULL) {
  root <- hipercow_root(root)
  path <- file.path(root$path$tasks, id)
  path_result <- file.path(path, RESULT)
  if (!file.exists(path_result)) {
    status <- task_status(id, root)
    task_driver <- vcapply(id, task_get_driver, root = root)
    if (is.na(task_driver) || !(status %in% c("success", "failure"))) {
      cli::cli_abort(
        "Result for task '{id}' not available, status is '{status}'")
    }
    dat <- hipercow_driver_prepare(task_driver, root, environment())
    dat$driver$result(id, dat$config, root$path$root)
  }
  readRDS(path_result)
}


##' Get the task log, if the task has produced one.  Tasks run by the
##' `windows` driver will generally produce a log.  A log might be
##' quite long, and you might want to print it to screen in its
##' entirity (`task_log_show`), return it as character vector
##' (`task_log_value`).
##'
##' @title Get task result
##'
##' @inheritParams task_status
##'
##' @return The value of the queued expression
##'
##' @rdname task_log
##' @export
task_log_show <- function(id, root = NULL) {
  result <- task_log_fetch(id, root)
  if (is.null(result)) {
    cli::cli_alert_danger("No logs for task '{id}' (yet?)")
  } else if (length(result) == 0) {
    cli::cli_alert_danger("Empty logs for task '{id}' (so far?)")
  } else {
    cat(paste0(result, "\n", collapse = ""))
  }
}


##' @rdname task_log
##' @export
task_log_value <- function(id, root = NULL) {
  task_log_fetch(id, root)
}


task_log_fetch <- function(id, root) {
  driver <- task_get_driver(id, root = root)
  dat <- hipercow_driver_prepare(driver, root, environment())
  dat$driver$log(id, dat$config, root$path$root)
}


final_status_to_logical <- function(status) {
  switch(status,
         submitted = NA,
         running = NA,
         success = TRUE,
         failure = FALSE,
         cancelled = FALSE,
         cli::cli_abort("Unhandled status '{status}'"))
}


##' Wait for a single task to complete.
##'
##' @title Wait for a task to complete
##'
##' @inheritParams task_status
##'
##' @param timeout The time to wait for the task to complete. The
##'   default is to wait forever.
##'
##' @param poll The interval to request an update; shorter values here
##'   will return more quickly but will flail around a bit more.  The
##'   default is to check every second which means you wait for up to
##'   a second longer than needed - for long running jobs you could
##'   set this longer if you wanted.
##'
##' @param progress Logical value, indicating if a progress indicator
##'   should be used. The default `NULL` uses the option
##'   `hipercow.progress`, and if unset displays a progress bar in an
##'   interactive session.
##'
##' @return Logical value, `TRUE` if the task completed successfully,
##'   `FALSE` otherwise.
##'
##' @export
task_wait <- function(id, timeout = Inf, poll = 1, progress = NULL,
                      root = NULL) {
  root <- hipercow_root(root)
  path <- file.path(root$path$tasks, id)
  status <- task_status(id, root = root)

  if (status == "created") {
    cli::cli_abort(
      c("Cannot wait on '{id}', which has status '{status}'",
        i = "You need to submit this task to wait on it"))
  }

  value <- final_status_to_logical(status)
  progress <- show_progress(progress)
  if (progress && is.na(value)) {
    cli::cli_progress_bar(
      format = paste("{cli::pb_spin} Waiting for task '{id}' |",
                     "{status} | {cli::pb_elapsed}"))
  }

  t_end <- Sys.time() + timeout
  repeat {
    if (!is.na(value)) {
      break
    }
    if (Sys.time() > t_end) {
      cli::cli_abort("Task '{id}' did not complete in time")
    }
    Sys.sleep(poll)
    if (progress) {
      cli::cli_progress_update()
    }
    status <- task_status(id, root = root)
    value <- final_status_to_logical(status)
  }

  value
}


##' Cancel one or more tasks
##'
##' @title Cancel tasks
##'
##' @param id The task id or task ids to cancel
##'
##' @inheritParams task_status
##'
##' @return A logical vector the same length as `id` indicating if the
##'   task was cancelled. This will be `FALSE` if the job was already
##'   completed, not running, etc.
##'
##' @export
task_cancel <- function(id, root = NULL) {
  root <- hipercow_root(root)
  cancelled <- rep(FALSE, length(id))
  status <- task_status(id, root)
  eligible <- status %in% c("submitted", "running")
  if (any(eligible)) {
    task_driver <- vcapply(id, task_get_driver, root = root)
    for (driver in unique(na_omit(task_driver))) {
      dat <- hipercow_driver_prepare(task_driver, root, environment())
      j <- task_driver == driver
      cancelled[eligible][j] <-
        dat$driver$cancel(id[eligible][j], dat$config, root$path$root)
    }
    file.create(file.path(root$path$tasks, id[cancelled], STATUS_CANCELLED))
  }
  task_cancel_report(id, status, cancelled, eligible)
  cancelled
}


## This is surprisingly disgusting.
task_cancel_report <- function(id, status, cancelled, eligible) {
  n <- length(id)
  if (n == 1) {
    if (cancelled) {
      cli::cli_alert_success("Successfully cancelled '{id}'")
    } else if (!eligible) {
      cli::cli_alert_warning(
        "Did not try to cancel '{id}' as it had status '{status}'")
    } else {
      cli::cli_alert_danger(
        "Did not manage to cancel '{id}' which had status '{status}'")
    }
  } else if (n > 1) {
    m <- sum(eligible)
    if (all(cancelled)) {
      cli::cli_alert_success("Successfully cancelled {n} tasks")
    } else if (!any(eligible)) {
      cli::cli_alert_warning(
        "Did not try to cancel any of {n} tasks as none were eligible")
    } else if (all(cancelled[eligible])) {
      cli::cli_alert_success(
        paste("Successfully cancelled {cli::qty(m)}{?the/all} {m}",
              "eligible {cli::qty(m)}task{?s}",
              "(of the {n} requested)"))
    } else if (!any(cancelled[eligible])) {
      cli::cli_alert_danger(
        paste("Failed to cancel {cli::qty(m)}{?the/all} {m}",
              "eligible {cli::qty(m)}task{?s}",
              "(of the {n} requested)"))
    } else { # some cancelled, some not
      k <- sum(cancelled[eligible])
      cli::cli_alert_warning(
        paste("Cancelled {k} of {m} eligible {cli::qty(m)}task{?s}",
              "(of the {n} requested)"))
    }
  }
}


task_eval_explicit <- function(data, envir, root) {
  if (!is.null(data$variables$locals)) {
    list2env(data$variables$locals, envir)
  }
  eval(data$expr, envir)
}


task_eval_expression <- function(data, envir, root) {
  rlang::env_bind(envir, !!!data$variables$locals)
  ## It's possible that we need to use rlang::eval_tidy() here, see
  ## the help page for an example.  It does depend on how much we want
  ## to export though.
  eval(data$expr, envir)
}


relative_workdir <- function(root_path, call = NULL) {
  workdir <- normalize_path(getwd())
  if (!fs::path_has_parent(workdir, root_path)) {
    cli::cli_abort(
      c("Working directory is not a subdirectory of the hipercow root",
        i = "Working: {workdir}",
        i = "Hipercow: {root_path}"),
      call = call)
  }
  as.character(fs::path_rel(workdir, root_path))
}


task_variables <- function(names, envir, environment, root, call = NULL) {
  if (length(names) == 0) {
    ensure_environment_exists(environment, root, rlang::current_env())
    NULL
  } else {
    in_environment <- environment_load(environment, root, call)$globals
    nms_globals <- intersect(names, in_environment)
    nms_locals <- setdiff(names, nms_globals)

    locals <- rlang::env_get_list(envir, nms_locals, inherit = TRUE,
                                  last = topenv())
    check_locals_size(locals, call = call)
    
    validate_globals <- getOption("hipercow.validate_globals", FALSE)
    if (validate_globals && length(nms_globals) > 0) {
      globals <- rlang::env_get_list(envir, nms_globals, inherit = TRUE,
                                     last = topenv())
      globals <- vcapply(globals, rlang::hash)
    } else {
      globals <- NULL
    }

    list(locals = locals, globals = globals)
  }
}


task_submit_maybe <- function(id, submit, root, call) {
  if (!is.null(submit)) {
    ## Could also allow character here soon.
    assert_scalar_logical(submit, call = call)
  }
  has_config <- length(root$config) > 0
  if (isFALSE(submit) || (!has_config && is.null(submit))) {
    return(FALSE)
  }
  if (!has_config) {
    cli::cli_abort(
      c("Can't submit task because no driver configured",
        i = "Run 'hipercow::hipercow_configure()' to configure a driver"),
      call = call)
  }
  if (length(root$config) == 1) {
    driver <- names(root$config)
  } else {
    cli::cli_abort("Can't cope with more than one driver configured yet",
                   call = call)
  }
  task_submit(id, driver = driver, root = root)
  TRUE
}


show_progress <- function(progress, call = NULL) {
  if (is.null(progress)) {
    getOption("hipercow.progress", interactive())
  } else {
    assert_scalar_logical(progress, call = call)
    progress
  }
}


check_locals_size <- function(locals, call = NULL) {
  if (length(locals) == 0) {
    return()
  }
  max_size <- getOption("hipercow.max_size_local", 1e6)
  if (!is.finite(max_size)) {
    return()
  }
  size <- vnapply(locals, object.size)
  err <- names(locals)[size > max_size]
  if (length(err) > 0) {
    max_size_bytes <- format_bytes(max_size)
    cli::cli_abort(
      c("Object{?s} too large to save with task: {squote(err)}",
        x = "Objects saved with a hipercow task can only be {max_size_bytes}",
        i = paste("You can increase the limit by increasing the value of",
                  "the option 'hipercow.max_size_local', even using 'Inf' to",
                  "disable this check entirely"),
        i = paste("Better again, if create large objects from your 'sources'",
                  "argument to your environment, and then advertise this",
                  "using the 'globals' argument")),
      call = call)
  }
}
