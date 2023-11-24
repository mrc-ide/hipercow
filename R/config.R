##' Configure your hermod root.
##'
##' @title Configure your hermod root
##'
##' @param shares Network shares in addition to your working
##'   directory. You only need to add this if you want to access some
##'   files from a network location other than the one that you are
##'   working in; e.g., you have large files that you want to read on
##'   your `P:`, you could map that here, and then refer to
##'   `P:/myfiles/` in your job.
##'
##' @param r_version A string, or `numeric_version` object, describing
##'   the R version required.  Not all R versions are known to be
##'   supported, so this will check against a list of installed R
##'   versions for the cluster you are using.  If omitted then: if
##'   your R version matches a version on the cluster that will be
##'   used, or the oldest cluster version that is newer than yours, or
##'   the most recent cluster version.
##'
##' @param root Hermod root, usually best `NULL`
##'
##' @export
##' @author Richard Fitzjohn
hermod_configure <- function(shares = NULL, r_version = NULL, root = NULL) {
  ## For now,
  ## * only one cluster; wpia-hn - this is all that works as of soon.
  ## * if users need the temp drive, they add that as a share, same as any other
  ## * match the r version automatically, as before
  ## * templates we will think about carefully later, new cluster does not
  ##   even have job templates
  ## * wholenode/parallel/cores etc - we will think about this later too,
  ##   all in one go on parallelism.
  ##
  ## We'll store everything here, works by reference. New calls to
  ## configure overwrite, they do not increment.
  root <- hermod_root(root)

  config <- hermod_config_create(root$path$root, shares, r_version)
  fs::dir_create(dirname(root$path$config))
  saveRDS(config, file.path(root$path$config))
  root$config <- config

  invisible(config)
}


hermod_config_create <- function(path, shares, r_version) {
  config <- list(
    cluster = "wpia-hn",
    template = "AllNodes",
    shares = dide_cluster_paths(shares, path),
    r_version = select_r_version(r_version))
  class(config) <- "hermod_config"
  config
}


hermod_config <- function(root = NULL) {
  config <- hermod_root(root)$config
  if (is.null(config)) {
    cli::cli_abort(
      c("This hermod root is not configured",
        i = "Please run 'hermod_configure()'"))
  }
  config
}


select_r_version <- function(r_version, ours = getRversion()) {
  if (is.null(r_version)) {
    valid <- r_versions()
    if (ours %in% valid) {
      r_version <- numeric_version(ours)
    } else {
      i <- valid > ours
      j <- if (any(i)) which(i)[[1L]] else length(valid)
      r_version <- valid[[j]]
    }
  } else {
    if (is.character(r_version)) {
      r_version <- numeric_version(r_version)
    }
    if (!(r_version %in% r_versions())) {
      stop("Unsupported R version: ", as.character(r_version))
    }
  }
  r_version
}
