##' Create a bundle of tasks.  This is simply a collection of tasks
##' that relate together in some way, and we provide some helper
##' functions for working with them that save you writing lots of
##' loops.  Each bundle has a name, which will be randomly generated
##' if you don't provide one, and a set of task ids.
##'
##' @title Create task bundle
##'
##' @param ids A character vector of task ids
##'
##' @param validate Logical, indicating if we should check that the
##'   task ids exist.  We always check that the task ids are
##'   plausible.
##'
##' @param name A string, the name for the bundle.  If not given, then
##'   a random nmae is generated.
##'
##' @param overwrite Logical, indicating that we should overwrite any
##'   existing bundle with the same name.
##'
##' @inheritParams task_eval
##'
##' @return A task bundle object
##' @export
hipercow_bundle_create <- function(ids, name = NULL, validate = TRUE,
                                   overwrite = TRUE, root = NULL) {
  root <- hipercow_root(root)
  if (length(ids) == 0) {
    cli::cli_abort("Can't make a bundle with no tasks")
  }
  assert_character(ids)
  ok <- grepl("^[[:alnum:]]{32}$", ids)
  if (any(!ok)) {
    cli::cli_abort(
      "All entries in 'ids' must be valid ids (32 character hex strings)")
  }
  if (is.null(name)) {
    name <- ids::adjective_animal()
  } else {
    assert_scalar_character(name)
  }
  assert_scalar_logical(validate)
  assert_scalar_logical(overwrite)
  if (validate) {
    ok <- file.exists(file.path(root$path$tasks, ids))
    if (!all(ok)) {
      cli::cli_abort(
        "Can't include tasks in bundle that don't exist, validate is 'TRUE'")
    }
  }
  dest <- file.path(root$path$bundles, name)
  if (!overwrite && file.exists(dest)) {
    cli::cli_abort("Bundle '{name}' exists and overwrite is FALSE")
  }
  fs::dir_create(root$path$bundles)
  writeLines(ids, dest)
  cli::cli_alert_success("Created bundle '{name}' with {length(ids)} task{?s}")
  new_bundle(name, ids)
}


##' Load an existing saved bundle
##'
##' @title Load existing bundle
##'
##' @param name Name of the bundle to load
##'
##' @inheritParams hipercow_bundle_create
##'
##' @return A `hipercow_bundle` object
##'
##' @export
hipercow_bundle_load <- function(name, root = NULL) {
  root <- hipercow_root(root)
  assert_scalar_character(name)
  path <- file.path(root$path$bundles, name)
  if (!file.exists(path)) {
    cli::cli_abort(
      c("No such bundle '{name}'",
        i = "Consider 'hipercow_bundle_list()' to see existing bundles"))
  }
  ids <- readLines(path)
  new_bundle(name, ids)
}


##' List existing bundles
##'
##' @title List existing bundles
##'
##' @inheritParams hipercow_bundle_create
##'
##' @return A [data.frame] with columns `name` and `time`, ordered by
##'   time (most recent first)
##'
##' @export
hipercow_bundle_list <- function(root = NULL) {
  root <- hipercow_root(root)
  nms <- dir(root$path$bundles)
  time <- file.info(file.path(root$path$bundles, nms))$mtime
  i <- order(time, decreasing = TRUE)
  data.frame(name = nms[i], time = time[i])
}


##' Delete one or more hipercow task bundles.  Note that this does not
##' delete the underlying tasks, which is not yet supported.
##'
##' @title Delete task bundles
##'
##' @param name Character vectors of names to delete
##'
##' @inheritParams hipercow_bundle_create
##'
##' @return Nothing, called for its side effect
##' @export
hipercow_bundle_delete <- function(name, root = NULL) {
  root <- hipercow_root(root)
  assert_character(name)
  unlink(file.path(root$path$bundles, name))
}


##' Cancel all tasks in a bundle.  This wraps [task_cancel] for all
##' the ids.
##'
##' @title Cancel bundle tasks
##'
##' @inheritParams hipercow_bundle_status
##'
##' @return A logical vector the same length as `id` indicating if the
##'   task was cancelled. This will be `FALSE` if the job was already
##'   completed, not running, etc.
##'
##' @export
hipercow_bundle_cancel <- function(bundle, follow = TRUE, root = NULL) {
  root <- hipercow_root(root)
  bundle <- check_bundle(bundle, root, rlang::current_env())
  task_cancel(bundle$ids, follow = follow, root = root)
}


##' Fetch status for all tasks in a bundle.
##'
##' @title Bundle status
##'
##' @param bundle Either a `hipercow_bundle` object, or the name of a
##'   bundle.
##'
##' @param reduce Reduce the status across all tasks in the bundle.
##'   This means we return a single value with the "worst" status
##'   across the bundle.  We only return `success` if *all* tasks have
##'   succeeded, and will return `failed` if any task has failed.
##'
##' @inheritParams task_status
##'
##' @return A character vector the same length as the number of tasks
##'   in the bundle, or length 1 if `reduce` is `TRUE`.
##'
##' @export
hipercow_bundle_status <- function(bundle, reduce = FALSE, follow = TRUE,
                                   root = NULL) {
  root <- hipercow_root(root)
  bundle <- check_bundle(bundle, root, rlang::current_env())
  status <- task_status(bundle$ids, follow = follow, root = root)
  if (reduce) status_reduce(status, "status") else status
}


##' Fetch all bundle results
##'
##' @title Fetch bundle results
##'
##' @inheritParams hipercow_bundle_status
##'
##' @return An unnamed list, with each element being the result for
##'   each a task in the bundle, in the same order.
##'
##' @export
hipercow_bundle_result <- function(bundle, follow = TRUE, root = NULL) {
  root <- hipercow_root(root)
  here <- rlang::current_env()
  bundle <- check_bundle(bundle, root, here)
  lapply(bundle$ids, function(id) {
    tryCatch(
      task_result(id, follow = follow, root = root),
      error = function(e) {
        cli::cli_abort(
          paste("Can't fetch results for bundle '{bundle$name}' due",
                "to error fetching result for '{id}'"),
          parent = e, call = here)
      })
  })

}


##' Wait for tasks in a bundle to complete.  This is the
##' generalisation of [task_wait] for a bundle.
##'
##' @title Wait for a bundle to complete
##'
##' @param fail_early Logical, indicating if we should fail as soon as
##'   the first task has failed.  In this case, the other running
##'   tasks continue running, but we return and indicate that the
##'   final result will not succeed.  If `fail_early = FALSE` we keep
##'   running until all tasks have passed or failed, even though we
##'   know we will return `FALSE`; but upon return
##'   `hipercow_bundle_result()` can be called and all results/errors
##'   returned.
##'
##' @inheritParams hipercow_bundle_status
##' @inheritParams task_wait
##'
##' @return A scalar logical value; `TRUE` if _all_ tasks complete
##'   successfully and `FALSE` otherwise
##'
##' @export
hipercow_bundle_wait <- function(bundle, follow = TRUE, timeout = Inf, poll = 1,
                                 fail_early = TRUE, progress = NULL,
                                 root = NULL) {
  root <- hipercow_root(root)
  bundle <- check_bundle(bundle, root, rlang::current_env())
  name <- bundle$name
  ids <- bundle$ids
  if (follow) {
    ids <- follow_retry_map(ids, root)
  }

  get_status <- function() {
    status_reduce(hipercow_bundle_status(bundle, follow = FALSE, root = root),
                  if (fail_early) "wait-fail-early" else "wait-fail-late")
  }

  status <- get_status()
  if (status == "created") {
    cli::cli_abort(
      c("Cannot wait on bundle '{name}', which has unsubmitted tasks",
        i = "You need to submit these tasks to wait on this bundle"))
  }

  value <- final_status_to_logical(status)
  if (any(is.na(value))) {
    ensure_package("logwatch")
    res <- logwatch::logwatch(
      sprintf("bundle '%s'", name),
      get_status,
      function() NULL,
      show_log = FALSE,
      show_spinner = show_progress(progress, call),
      poll = poll,
      timeout = timeout,
      status_waiting = "submitted")
    value <- final_status_to_logical(res$status)
    if (is.na(value)) {
      cli::cli_abort(
        "Bundle '{name}' did not complete in time")
    }
  }
  value
}


##' Fetch logs from tasks in a bundle.
##'
##' @title Fetch bundle logs
##'
##' @inheritParams hipercow_bundle_status
##' @inheritParams task_log_value
##'
##' @return A list with each element being the logs for the
##'   corresponding element in the bundle.
##'
##' @export
hipercow_bundle_log_value <- function(bundle, follow = TRUE, outer = FALSE,
                                      root = NULL) {
  root <- hipercow_root(root)
  bundle <- check_bundle(bundle, root, rlang::current_env())
  lapply(bundle$ids, task_log_value, outer = outer, root = root)
}


##' Retry tasks in a bundle.  This has slightly different semantics to
##' [task_retry()], which errors if a retry is not possible.  Here, we
##' anticipate that much of the time you will be interested in
##' retrying some fraction of your bundle and so don't need to wait
##' until all tasks have finished in order to retry failed tasks.
##'
##' @title Retry task bundle
##'
##' @param if_status_in Optionally, a character vector of task
##'   statuses for which we should retry tasks.  For example, pass
##'   `if_status_in = c("cancelled", "failure")` to retry cancelled
##'   and failed tasks.  Can only be terminal statuses (`cancelled`,
##'   `failure`, `success`).
##'
##' @inheritParams hipercow_bundle_status
##' @inheritParams task_retry
##'
##' @return Invisibly, a logical vector, indicating which of the tasks
##'   within the bundle were retried. This means that it's not
##'   immediately obvious how you can get the new id back from the
##'   tasks, but typically that is unimportant, as all bundle
##'   functions follow retries by default.
##'
##' @export
hipercow_bundle_retry <- function(bundle, if_status_in = NULL, submit = NULL,
                                  root = NULL) {
  root <- hipercow_root(root)
  bundle <- check_bundle(bundle, root, rlang::current_env())
  terminal <- c("cancelled", "failure", "success")
  if (is.null(if_status_in)) {
    if_status_in <- terminal
  } else {
    assert_character(if_status_in)
    err <- setdiff(if_status_in, terminal)
    if (length(err) > 0) {
      cli::cli_abort("Invalid value{?s} for 'if_status_in': {squote(err)}")
    }
  }
  status <- hipercow_bundle_status(bundle, follow = TRUE, root = root)
  i <- status %in% if_status_in
  if (any(i)) {
    cli::cli_alert_info("Retrying {sum(i)} / {length(i)} task{?s}")
    ids <- vcapply(bundle$ids[i], task_retry, submit = FALSE, root = root)
    task_submit_maybe(ids, submit, root, rlang::current_env())
  } else {
    t <- table(status)
    summary <- sprintf("%d %s", unname(t), names(t))
    cli::cli_abort("No tasks eligible for retry: {summary}")
  }
  invisible(i)
}


new_bundle <- function(name, ids) {
  structure(list(name = name, ids = ids), class = "hipercow_bundle")
}


check_bundle <- function(bundle, root, call = NULL) {
  if (inherits(bundle, "hipercow_bundle")) {
    return(bundle)
  }
  if (!rlang::is_scalar_character(bundle)) {
    cli::cli_abort(
      c("Invalid value for 'bundle'",
        i = "Expected a 'hipercow_bundle' or a string with a bundle name"),
      call = call)
  }
  hipercow_bundle_load(bundle, root)
}


##' @export
print.hipercow_bundle <- function(x, ...) {
  cli::cli_bullets(c(
    ">" = "<hipercow_bundle '{x$name}' with {length(x$ids)} task{?s}>"))
  invisible(x)
}


status_reduce <- function(x, flavour) {
  if (flavour == "status") {
    order <- c("created", "failure", "cancelled", "running", "submitted",
               "success")
  } else if (flavour == "wait-fail-early") {
    order <- c("created", "failure", "cancelled", "running", "submitted",
               "success")
  } else if (flavour == "wait-fail-late") {
    order <- c("created", "running", "submitted",
               "failure", "cancelled", "success")
  }
  order[min(match(x, order))]
}
