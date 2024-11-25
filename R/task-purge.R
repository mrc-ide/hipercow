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
##' A non-exhaustive list:
##'
##' * If you delete a task that is part of a [task_retry] chain, then
##'   all tasks (both upstream and downstream in that chain) are
##'   deleted
##' * Once we support task dependencies (mrc-4797), deleting tasks
##'   will mark any not-yet-run dependent task as impossible, or perhaps
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
##' @param dry_run If TRUE, report what would have been done, but
##'   no changes will be made.
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
                           dry_run = FALSE,
                           root = NULL) {
  root <- hipercow_root(root)

  if (!is.null(with_status)) {
    valid <- c("created", "success", "failure", "cancelled")
    invalid <- setdiff(with_status, valid)
    if (length(invalid) > 0) {
      cli::cli_abort(
        "Cannot purge tasks with status{?es}: {squote(invalid)}",
        arg = "with_status")
    }
  }

  ids <- task_select_ids(task_ids, finished_before, in_bundle, with_status,
                         allow_no_filter = FALSE, root = root)

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
  maybe_unlink(path_task(root$path$tasks, ids), recursive = TRUE,
               dry_run = dry_run)

  nms <- dir(root$path$bundles)
  contents <- lapply(nms, hipercow_bundle_load, root)
  to_delete <- vlapply(contents, function(x) any(ids %in% x$ids))
  if (any(to_delete)) {
    cli::cli_alert_info("Deleting {sum(to_delete)} task bundle{?s}")
    maybe_unlink(file.path(root$path$bundles, nms[to_delete]),
                 dry_run = dry_run)
  } else {
    cli::cli_alert_info("No task bundles need deleting")
  }

  invisible(ids)
}
