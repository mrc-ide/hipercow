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
##' the furthest through this list.
##'
##' Later, we will introduce other types to cope with tasks that have
##' been retried, or that are blocked on dependencies (or have become
##' impossible).
##'
##' @title Get task status
##'
##' @param id The task identifier
##'
##' @param follow Logical, indicating if we should follow any retried
##'   tasks.
##'
##' @inheritParams task_eval
##'
##' @return A string with the task status. Tasks that do not exist
##'   will have a status of `NA`.
##'
##' @export
task_status <- function(id, follow = TRUE, root = NULL) {
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

  if (follow) {
    id <- follow_retry_map(id, root)
  }

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

  if (any(i) && allow_load_drivers()) {
    task_driver <- vcapply(id[i], task_get_driver, root = root)
    for (driver in unique(na_omit(task_driver))) {
      j <- task_driver == driver
      status[i][j] <- task_status_for_driver(id[i][j], driver, root)
    }
    i <- is.na(status)
  }

  if (any(i)) {
    for (s in c(STATUS_RUNNING, STATUS_SUBMITTED)) {
      if (any(j <- file.exists(file.path(path[i], s)))) {
        status[i][j] <- sub("status-", "", s)
        i <- is.na(status)
      }
    }
  }

  ## Does this task even exist?
  if (any(i)) {
    if (any(j <- file.exists(file.path(path[i], EXPR)))) {
      status[i][j] <- "created"
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


task_status_for_driver <- function(id, driver, root) {
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  status <- dat$driver$status(id, dat$config, root$path$root)
  terminal <- c(success = STATUS_SUCCESS,
                failure = STATUS_FAILURE,
                cancelled = STATUS_CANCELLED)
  is_terminal <- status %in% names(terminal)
  if (any(is_terminal)) {
    file_create_if_not_exists(file.path(
      root$path$tasks, id[is_terminal], terminal[status[is_terminal]]))
  }
  status
}


##' Get the task result. This might be an error if the task has failed.
##'
##' @title Get task result
##'
##' @inheritParams task_status
##'
##' @return The value of the queued expression
##' @export
task_result <- function(id, follow = TRUE, root = NULL) {
  root <- hipercow_root(root)
  if (follow) {
    id <- follow_retry_map(id, root)
  }
  path <- file.path(root$path$tasks, id)
  path_result <- file.path(path, RESULT)
  if (!file.exists(path_result)) {
    status <- task_status(id, follow = FALSE, root = root)
    if (allow_load_drivers()) {
      task_driver <- task_get_driver(id, root = root)
    } else {
      task_driver <- NA
    }
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
##' entirety (`task_log_show`), or return it as character vector
##' (`task_log_value`).
##'
##' The function `task_log_watch` has similar semantics to
##' [task_wait] but does not error on timeout, and always displays a
##' log.
##'
##' @title Get task log
##'
##' @param outer Logical, indicating if we should request the "outer"
##'   logs; these are logs from the underlying HPC software before it
##'   hands off to hipercow.
##'
##' @inheritParams task_status
##'
##' @return Depending on the function:
##'
##' * `task_log_show` returns the log value contents invisibly, but
##'   primarily displays the log contents on the console as a side
##'   effect
##' * `task_log_value` returns a character of log contents
##' * `task_log_watch` returns the status converted to logical (as
##'   for [task_wait])
##'
##' @rdname task_log
##' @export
task_log_show <- function(id, follow = TRUE, outer = FALSE, root = NULL) {
  root <- hipercow_root(root)
  result <- task_log_fetch(id, follow, outer, root)
  if (is.null(result)) {
    cli::cli_alert_danger("No logs for task '{id}' (yet?)")
  } else if (length(result) == 0) {
    cli::cli_alert_danger("Empty logs for task '{id}' (so far?)")
  } else {
    cat(paste0(result, "\n", collapse = ""))
  }
  invisible(result)
}


##' @rdname task_log
##' @export
task_log_value <- function(id, follow = TRUE, outer = FALSE, root = NULL) {
  root <- hipercow_root(root)
  task_log_fetch(id, follow, outer, root)
}


##' @rdname task_log
##'
##' @inheritParams task_wait
##'
##' @inheritParams logwatch::logwatch
##'
##' @export
task_log_watch <- function(id, follow = TRUE, poll = 1, skip = 0, timeout = Inf,
                           progress = NULL, root = NULL) {
  root <- hipercow_root(root)
  if (follow) {
    id <- follow_retry_map(id, root)
  }

  ## As in task_log_fetch; no need to do this each time around:
  driver <- task_get_driver(id, root = root)
  if (is.na(driver)) {
    cli::cli_abort(
      c("Cannot watch logs of task '{id}', which not been submitted",
        i = "You need to submit this task to watch its logs"))
  }
  dat <- hipercow_driver_prepare(driver, root, environment())

  ensure_package("logwatch")
  res <- logwatch::logwatch(
    "task",
    get_status = function() task_status(id, follow = FALSE, root = root),
    get_log = function() dat$driver$log(id, FALSE, dat$config, root$path$root),
    status_waiting = "submitted",
    status_running = "running",
    show_log = TRUE,
    show_spinner = show_progress(progress, call),
    timeout = timeout,
    poll = poll)

  final_status_to_logical(res$status)
}


task_log_fetch <- function(id, follow, outer, root) {
  if (follow) {
    id <- follow_retry_map(id, root)
  }
  driver <- task_get_driver(id, root = root)
  dat <- hipercow_driver_prepare(driver, root, environment())
  dat$driver$log(id, outer, dat$config, root$path$root)
}


final_status_to_logical <- function(status) {
  switch(status,
         submitted = NA,
         running = NA,
         timeout = NA, # from logwatch
         interrupt = NA, # from logwatch
         # Terminal status
         success = TRUE,
         failure = FALSE,
         cancelled = FALSE,
         # Catch future bugs
         cli::cli_abort("Unhandled status '{status}'"))
}


##' Wait for a single task to complete.  This function is very similar
##' to [task_log_watch], except that it errors if the job does not
##' complete (so that it can be used easily to ensure a task has
##' completed) and does not return any logs.
##'
##' The progress spinners here come from the cli package and will
##' respond to cli's options. In particular `cli.progress_clear` and
##' `cli.progress_show_after`.
##'
##' @title Wait for a task to complete
##'
##' @inheritParams task_status
##'
##' @param timeout The time to wait for the task to complete. The
##'   default is to wait forever.
##'
##' @param progress Logical value, indicating if a progress spinner
##'   should be used. The default `NULL` uses the option
##'   `hipercow.progress`, and if unset displays a progress bar in an
##'   interactive session.
##'
##' @inheritParams logwatch::logwatch
##'
##' @return Logical value, `TRUE` if the task completed successfully,
##'   `FALSE` otherwise.
##'
##' @export
task_wait <- function(id, follow = TRUE, timeout = Inf, poll = 1,
                      progress = NULL, root = NULL) {
  root <- hipercow_root(root)
  if (follow) {
    id <- follow_retry_map(id, root)
  }
  status <- task_status(id, follow = FALSE, root = root)

  if (status == "created") {
    cli::cli_abort(
      c("Cannot wait on task '{id}', which has not been submitted",
        i = "You need to submit this task to wait on it"))
  }

  value <- final_status_to_logical(status)
  if (is.na(value)) {
    ensure_package("logwatch")
    res <- logwatch::logwatch(
      sprintf("task '%s'", id),
      function() task_status(id, follow = FALSE, root = root),
      function() NULL,
      show_log = FALSE,
      show_spinner = show_progress(progress, call),
      poll = poll,
      timeout = timeout,
      status_waiting = "submitted")

    status <- res$status
    value <- final_status_to_logical(status)
    if (is.na(value)) {
      cli::cli_abort("Task '{id}' did not complete in time (status: {status})")
    }
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
task_cancel <- function(id, root = NULL) { # TODO: why no follow?
  root <- hipercow_root(root)
  cancelled <- rep(FALSE, length(id))
  status <- task_status(id, follow = FALSE, root = root)
  eligible <- status %in% c("submitted", "running")
  if (any(eligible)) {
    task_driver <- vcapply(id, task_get_driver, root = root)
    for (driver in unique(na_omit(task_driver))) {
      i <- task_driver == driver
      cancelled[eligible][i] <-
        task_cancel_for_driver(id[eligible][i], driver, root)
    }
  }
  task_cancel_report(id, status, cancelled, eligible)
  cancelled
}


task_cancel_for_driver <- function(id, driver, root) {
  dat <- hipercow_driver_prepare(driver, root, environment())
  res <- dat$driver$cancel(id, dat$config, root$path$root)

  is_cancelled <- res$cancelled
  if (any(is_cancelled)) {
    time_cancelled <- Sys.time()
    file.create(file.path(root$path$tasks, id[is_cancelled], STATUS_CANCELLED))
    for (i in which(is_cancelled)) {
      times <- c(
        created = readRDS(file.path(root$path$tasks, id[i], EXPR))$time,
        started = res$time_started[[i]],
        finished = time_cancelled)
      info <- list(status = "cancelled", times = times,
                   cpu = NULL, memory = NULL)
      saveRDS(info, file.path(root$path$tasks, id[i], INFO))
    }
  }

  is_cancelled
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
