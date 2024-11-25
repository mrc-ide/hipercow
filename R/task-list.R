##' List hipercow tasks.  This is rarely what you want to, but can be
##' useful in some situations.  We get requests for this function
##' periodically, so here it is!  Other, better, functions may be
##' available (see Details).  Please be aware that this function can
##' be quite slow, and do not overuse it in scripts (please do not,
##' for example, call this repeatedly in a loop - talk to us if you
##' are tempted to do this).
##'
##' Sometimes, better functions are available for you:
##'
##' * If you want to list tasks in order to delete them, you might
##'   prefer [task_purge()]
##'
##' * If you want to list tasks in a bundle, you should use
##'   [hipercow_bundle_list()] to find the bundle and
##'   [hipercow_bundle_load()] to load it (or use other bundle
##'   functions)
##'
##' Once you have listed tasks with `task_list()`
##' 
##' @title List tasks
##'
##' @param task_ids A character vector of task identifiers.  Typically
##'   if you provide this you will not provide any other filters.
##'
##' @param finished_before A date, time, or [difftime] object
##'   representing the time or time ago that a task finished (here,
##'   the job might have finished for any reason; successfully or
##'   unsuccessfully unless you also provide the `with_status`
##'   argument).
##'
##' @param in_bundle A character vector of bundle names. Wild cards
##'   are supported using shell (glob) syntax, rather than regular
##'   expression syntax.  So use `data_*` to match all bundles that
##'   start with `data_` (see [utils::glob2rx] for details).  It is an
##'   error if *no* bundles are matched, but not an error if any
##'   individual pattern does not match.
##'
##' @param with_status A character vector of statuses to match.  We
##'   only purge tasks that match these statuses.  Valid statuses to
##'   use are `created`, `submitted, `running`, `success`, `failure`
##'   and `cancelled`.
##' 
##' @return A character vector.  You may want to then pull this vector
##'   of ids into a bundle (e.g., [hipercow_bundle_create]).  The
##'   order is arbitrary and does not reflect anything in your tasks.
##'
##' @export
task_list <- function(task_ids = NULL,
                      finished_before = NULL,
                      in_bundle = NULL,
                      with_status = NULL,
                      root = NULL) {
  root <- hipercow_root(root)
  task_select_ids(task_ids, finished_before, in_bundle, with_status,
                  allow_no_filter = TRUE, root = root)
}


task_select_ids <- function(task_ids, finished_before, in_bundle, with_status,
                            allow_no_filter, root, call = parent.frame()) {
  no_filter <- is.null(task_ids) && is.null(finished_before) &&
    is.null(in_bundle) && is.null(with_status)
  if (no_filter) {
    if (allow_no_filter) {
      return(task_select_all(root))
    }
    cli::cli_abort(
      c("No filter selected",
        i = paste("One of the arguments 'task_ids', 'finished_before' etc",
                  "must be used")),
      call = call)
  }

  task_ids <- task_select_by_id(task_ids, root, call)
  task_ids <- task_select_by_bundle(task_ids, in_bundle, root, call)
  task_ids <- task_select_by_status(task_ids, with_status, root, call)
  task_ids <- task_select_by_time(task_ids, finished_before, root, call)
  task_ids
}


task_select_by_id <- function(task_ids, root, call) {
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


task_select_by_bundle <- function(task_ids, in_bundle, root, call) {
  if (is.null(in_bundle)) {
    return(task_ids)
  }

  fn <- function(pattern) {
    dir(root$path$bundles, pattern = pattern, full.names = TRUE)    
  }
  bundles <- unlist_character(lapply(utils::glob2rx(in_bundle), fn))
  bundle_ids <- unlist_character(lapply(bundles, readLines))

  if (is.null(task_ids)) bundle_ids else intersect(bundle_ids, task_ids)
}


task_select_by_status <- function(task_ids, with_status, root, call) {
  if (is.null(with_status)) {
    return(task_ids)
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


task_select_by_time <- function(task_ids, finished_before, root, call) {
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


task_select_all <- function(root) {
  unlist_character(lapply(dir(root$path$tasks), function(p) {
    paste0(p, dir(file.path(root$path$tasks, p)))
  }))
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
