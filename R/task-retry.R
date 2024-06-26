##' Retry one or more tasks.  This creates a new task that copies the
##' work of the old one.  Most of the time this is transparent.  We'll
##' document this in the "advanced" vignette once it's written.
##'
##' This ends up being a little more complicated than ideal in order
##' to keep things relatively fast, while keeping our usual guarantees
##' about race conditions etc.  Basically; retrying is the only way a
##' task can move out of a terminal state but it still does not modify
##' the existing task.  Instead, we keep a separate register of
##' whether a task has been retried or not.  Each time we retry we
##' write into this register.  When you query about the status etc of
##' a task you can then add a `follow` argument to control whether or
##' not we check the register.  We assume that you never call this in
##' parallel; if you do then retries may be lost.  You can run
##' `task_retry(NULL)` to refresh the cached copy of the retry map if
##' you need to.
##'
##' @title Retry a task
##'
##' @param id The identifier or identifiers of tasks to retry.
##'
##' @inheritParams task_create_explicit
##'
##' @export
##' @return New identifiers for the retried tasks
##' @examples
##' cleanup <- hipercow_example_helper()
##'
##' # For demonstration, we just generate random numbers as then it's
##' # more obvious that things have been rerun:
##' id1 <- task_create_expr(runif(1))
##' task_wait(id1)
##' task_result(id1)
##'
##' # Now retry the task and get the retried result:
##' id2 <- task_retry(id1)
##' task_wait(id2)
##' task_result(id2)
##'
##' # After a retry, both the original and derived tasks know about
##' # each other:
##' task_info(id1)
##' task_info(id2)
##'
##' # By default every task will "follow" and access the most recent
##' # task in the chain:
##' task_result(id1) == task_result(id2)
##'
##' # You can prevent this by passing follow = FALSE to get the value
##' # of this particular attempt:
##' task_result(id1, follow = FALSE)
##'
##' # Tasks can be retried as many times as needed, creating a
##' # chain. It does not matter which task you retry as we always
##' # follow all the way to the end of the chain before retrying:
##' id3 <- task_retry(id1)
##' task_info(id1, follow = FALSE)
##' task_info(id3)
##'
##' cleanup()
task_retry <- function(id, driver = NULL, resources = NULL, root = NULL) {
  root <- hipercow_root(root)
  driver <- driver_before_create(driver, root, rlang::current_env())

  # More thinking to do on what resources should be for a retry
  # including: what about parallel
  resources <- resources_validate(resources, driver, root)

  id_real <- follow_retry_map(id, root)
  status <- task_status(id_real, follow = FALSE, root = root)
  err <- !(status %in% c("success", "failure", "cancelled"))
  if (any(err)) {
    n <- sum(err)
    i <- utils::head(which(err), 5)
    details <- set_names(sprintf("%s: %s", id_real[i], status[i]), "x")
    if (length(i) < n) {
      details <- c(details, c(i = "...and {n - length(i)} other task{?s}"))
    }
    cli::cli_abort(c("{n} {?task does/tasks do} not have terminal status",
                     details))
  }
  id_base <- base_retry_map(id_real, root)
  id_new <- vcapply(seq_along(id), function(i) {
    task_create(root, "retry", NULL, NULL, NULL, NULL,
                parent = id_real[[i]], base = id_base[[i]],
                inherit_envvars = id_base[[i]])
  })

  update_retry_map(id_new, id_real, id_base, root)

  task_submit_maybe(id_new, driver, resources, root, rlang::current_env())

  id_new
}


read_retry_map <- function(path) {
  if (file.exists(path)) {
    ret <- utils::read.csv(path, header = FALSE)
    names(ret) <- c("id", "parent", "base")
    ret
  } else {
    data.frame(id = character(), parent = character(), base = character())
  }
}


update_retry_map <- function(id, parent, base, root) {
  if (length(id) > 0) {
    append_lines(sprintf("%s,%s,%s", id, parent, base),
                 root$path$retry)
  }
  root$retry_map <- read_retry_map(root$path$retry)
}


## Lots of ways to optimise this if it becomes a timesink. Probably
## worth special case if length(id) == 1, possibly worth a lookup we
## invalidate with each map read?
follow_retry_map <- function(id, root) {
  map <- root$retry_map
  if (nrow(map) == 0 || length(id) == 0) {
    ## If noone has done a retry, early exit here
    return(id)
  }

  ret <- id
  while (!all(is.na(id))) {
    id <- map$id[match(id, map$parent)]
    i <- !is.na(id)
    ret[i] <- id[i]
  }
  ret
}


retry_chain <- function(id, root) {
  map <- root$retry_map
  if (nrow(map) == 0) {
    ## If noone has done a retry, early exit here
    return(NULL)
  }
  i <- match(id, map$id)
  if (!is.na(i)) {
    id <- map$base[i]
  }
  n <- sum(map$base == id)
  if (n == 0) {
    return(NULL)
  }
  ret <- rep(NA_character_, 1 + n)
  ret[[1]] <- id
  for (i in seq_len(n)) {
    id <- map$id[map$parent == id]
    ret[[i + 1]] <- id
  }
  ret
}


base_retry_map <- function(id, root) {
  map <- root$retry_map
  if (nrow(map) == 0 || length(id) == 0) {
    return(id)
  }
  i <- match(id, map$id)
  ret <- map$base[i]
  j <- is.na(i)
  ret[j] <- id[j]
  ret
}
