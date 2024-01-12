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


##' Delete one or more hipercow task bundles
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


new_bundle <- function(name, ids) {
  structure(list(name = name, ids = ids), class = "hipercow_bundle")
}


##' @export
print.hipercow_bundle <- function(x, ...) {
  cli::cli_bullets(c(
    ">" = "<hipercow_bundle '{x$name}' with {length(x$ids)} task{?s}>"))
  invisible(x)
}
