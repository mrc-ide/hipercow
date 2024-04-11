##' Purge (delete) hipercow tasks.  This is a destructive operation
##' that cannot be undone and can have unintended consequences!
##' However, if you are running short of space and don't want to just
##' delete everything and start again (which is our general
##' recommendation), this function provides a mechanism for cleaning
##' up tasks that you no longer need.
##'
##' Most of the arguments describe *filters* over your tasks.  We
##' delete the intersection of these filters (not the union), and you
##' must provide at least one filter.  So to delete all tasks that
##' were created more than a week ago you could write:
##'
##' ```
##' hipercow_purge(created_before = as.difftime(1, units = "weeks"))
##' ```
##'
##' but to restrict this to only tasks that have *also failed* you
##' could write
##'
##' ```
##' hipercow_purge(created_before = "1 week", with_status = "failed")
##' ```
##'
##' # Consequences of deletion
##'
##' A nonexhaustive list:
##'
##' * If you delete a task that is part of a [task_retry] chain, then
##'   all tasks (both upstream and downstream in that chain) are
##'   deleted
##' * Once we support task dependencies (mrc-4797), deleting tasks
##'   will mark any unrun dependent task as impossible, or perhaps
##'   delete it too, or prevent you from deleting the task; we've not
##'   decided yet
##' * You may have a bundle that references a task that you delete, in
##'   which case the bundle will not behave as expected.  As a result
##'   we delete all bundles that reference a deleted task
##' * Deleted bundles or deleted tasks that you hold identifiers to
##'   before deletion will not behave as expected, with tasks reported
##'   missing.  Restarting your session is probably the safest thing
##'   to do after purging.
##' * We can't prevent race conditions, so if you are purging tasks at
##'   the same time you are also retrying tasks that you will purge,
##'   you'll create tasks that we might not want to allow, and these
##'   tasks will fail in peculiar ways.
##'
##' @title Purge tasks
##'
##' @param task_ids A character vector of task identifiers.  Typically
##'   if you provide this you will not provide any other filters.
##'
##' @param finished_before A date, time, or [difftime] object
##'   representing the time or time ago that a task finished (here,
##'   the job might have finished for any reason; successfully or
##'   unsuccessfully unless you also provide the `with_status`
##'   argument).  Everything prior to this will be deleted.
##'
##' @param in_bundle A character vector of bundle names. Wild cards
##'   are supported using shell (glob) syntax, rather than regular
##'   expression syntax.  So use `data_*` to match all bundles that
##'   start with `data_` (see [utils::glob2rx] for details).  It is an error
##'   if *no* bundles are matched, but not an error if any individual
##'   pattern does not match.
##'
##' @param with_status A character vector of statuses to match.  We
##'   only purge tasks that match these statuses.  Valid statuses to
##'   use are `created`, `success`, `failure` and `cancelled` (note
##'   you cannot select tasks with status of `submitted` or `running`;
##'   use [task_cancel] for these first).
##'
##' @inheritParams task_eval
##'
##' @return A character vector of deleted identifiers, invisibly.
##'
##' @export
##' @examples
##' cleanup <- hipercow_example_helper()
##'
##' # Here are some tasks that have finished running:
##' bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5),
##'                                 bundle_name = "mybundle")
##' hipercow_bundle_wait(bundle)
##'
##' # Purge all tasks contained in any bundle starting with "my":
##' hipercow_purge(in_bundle = "my*")
##'
##' cleanup()
hipercow_purge <- function(task_ids = NULL,
                           finished_before = NULL,
                           in_bundle = NULL,
                           with_status = NULL,
                           root = NULL) {
  root <- hipercow_root(root)
  ids <- purge_select_ids(task_ids, finished_before, in_bundle, with_status,
                          root, rlang::current_env())
  if (length(ids) == 0) {
    cli::cli_alert_danger("Nothing to purge")
    return(invisible(character()))
  }

  if (is.null(with_status)) {
    status <- task_status(ids, root = root, follow = FALSE)
    err <- status %in% c("submitted", "running")
    if (any(err)) {
      cli::cli_abort("Can't purge; some tasks are submitted or running")
    }
  }

  if (NROW(root$retry_map) > 0) {
    extra <- setdiff(unlist(lapply(ids, retry_chain, root)), ids)
    if (length(extra > 0)) {
      status <- task_status(extra, root = root, follow = FALSE)
      err <- status %in% c("submitted", "running")
      if (any(err)) {
        cli::cli_abort(
          "Can't purge; some tasks have submitted or running retries")
      }
      cli::cli_alert_warning(
        "Also purging {length(extra)} task{?s} from retry chains")
    }
    ids <- c(ids, extra)
  }

  cli::cli_alert_info("Purging {length(ids)} task{?s}")
  unlink(path_task(root$path$tasks, ids), recursive = TRUE)

  nms <- dir(root$path$bundles)
  contents <- lapply(nms, hipercow_bundle_load, root)
  to_delete <- vlapply(contents, function(x) any(ids %in% x$ids))
  if (any(to_delete)) {
    cli::cli_alert_info("Deleting {sum(to_delete)} task bundle{?s}")
    unlink(file.path(root$path$bundles, nms[to_delete]))
  } else {
    cli::cli_alert_info("No task bundles need deleting")
  }

  invisible(ids)
}


purge_select_ids <- function(task_ids, finished_before, in_bundle, with_status,
                             root, call) {
  no_filter <- is.null(task_ids) && is.null(finished_before) &&
    is.null(in_bundle) && is.null(with_status)
  if (no_filter) {
    cli::cli_abort(
      c("No filter selected",
        i = paste("One of the arguments 'task_ids', 'finished_before' etc",
                  "must be used")),
      call = call)
  }

  task_ids <- purge_select_by_id(task_ids, root, call)
  task_ids <- purge_select_by_bundle(task_ids, in_bundle, root, call)
  task_ids <- purge_select_by_status(task_ids, with_status, root, call)
  task_ids <- purge_select_by_time(task_ids, finished_before, root, call)
  task_ids
}


purge_select_by_id <- function(task_ids, root, call) {
  if (!is.null(task_ids)) {
    assert_character(task_ids, call = call)
    ## Check that these tasks exist, error otherwise
    task_missing <- !fs::dir_exists(path_task(root$path$tasks, task_ids))
    if (any(task_missing)) {
      cli::cli_abort(
        "{sum(task_missing)} / {length(task_missing)} task{?s} do not exist",
        arg = "task_ids", call = call)
    }
  }
  task_ids
}


purge_select_by_bundle <- function(task_ids, in_bundle, root, call) {
  if (is.null(in_bundle)) {
    return(task_ids)
  }

  bundles <- unlist_character(lapply(utils::glob2rx(in_bundle), function(pattern) {
    dir(root$path$bundles, pattern = pattern, full.names = TRUE)
  }))
  bundle_ids <- unlist_character(lapply(bundles, readLines))

  if (is.null(task_ids)) bundle_ids else intersect(bundle_ids, task_ids)
}


purge_select_by_status <- function(task_ids, with_status, root, call) {
  if (is.null(with_status)) {
    return(task_ids)
  }
  valid <- c("created", "success", "failure", "cancelled")
  invalid <- setdiff(with_status, valid)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "Cannot purge tasks with status{?es}: {squote(invalid)}",
      arg = "with_status", call = call)
  }

  if (!is.null(task_ids)) {
    i <- task_status(task_ids, root = root, follow = FALSE) %in% with_status
    return(task_ids[i])
  }

  ## This is actually quite hard to do efficiently, and might
  ## benefit from a progress bar.  If one of the statues that we're
  ## interesed in is *not* 'created' we can probaly just look for
  ## either the terminal status marker or info and work back to the
  ## status fairly efficiently.  If 'created' is in the list, then
  ## we need to list every task unfortunately and check the status.
  path <- fs::path_split(
    dirname(dir(root$path$tasks, pattern = utils::glob2rx(DATA), 
            recursive = TRUE)))
  ids <- vcapply(path[lengths(path) == 2], paste0, collapse = "")
  task_ids <- if (is.null(task_ids)) ids else intersect(ids, task_ids)

  task_ids[task_status(task_ids, root = root) %in% with_status]
}


purge_select_by_time <- function(task_ids, finished_before, root, call) {
  if (is.null(finished_before)) {
    return(task_ids)
  }
  finished_before <- finished_before_to_time(finished_before, call)
  if (is.null(task_ids)) {
    path <- fs::path_split(
      dirname(dir(root$path$tasks, pattern = utils::glob2rx(INFO), 
              recursive = TRUE)))
    task_ids <- vcapply(path[lengths(path) == 2], paste0, collapse = "")
    path_info <- file.path(path_task(root$path$tasks, task_ids), INFO)
  } else {
    path_info <- file.path(path_task(root$path$tasks, task_ids), INFO)
    i <- file.exists(path_info)
    task_ids <- task_ids[i]
    path_info <- path_info[i]
  }
  time_finished <- unlist_times(
    lapply(path_info, function(p) readRDS(p)$times[["finished"]]))
  task_ids[time_finished < finished_before]
}


finished_before_to_time <- function(finished_before, call = call,
                                    now = Sys.time()) {
  assert_scalar(finished_before, call = call)
  if (inherits(finished_before, c("Date", "POSIXt"))) {
    return(finished_before)
  }
  if (inherits(finished_before, "difftime")) {
    return(now - abs(finished_before))
  }
  if (is.character(finished_before)) {
    re <- "^([0-9]) *(h(ours?)?|d(ays?)?|w(eeks?)?)$"
    if (!grepl(re, finished_before)) {
      cli::cli_abort(
        c("Invalid string input for 'finished_before': {finished_before}",
          i = "Should match '<n> hours|days|weeks'"),
        arg = "finished_before", call = call)
    }
    unit <- c(h = 3600, d = 86400, w = 604800)
    offset <- as.integer(sub(re, "\\1", finished_before)) *
      unit[[substr(sub(re, "\\2", finished_before), 1, 1)]]
    return(now - offset)
  }
  cli::cli_abort("Unsupported input for 'finished_before'",
                 arg = "finished_before", call = call)
}
