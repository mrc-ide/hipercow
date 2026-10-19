dide_path <- function(path_local, path_remote, drive_remote, call = NULL) {
  assert_scalar_character(path_local)
  assert_scalar_character(path_remote)
  assert_scalar_character(drive_remote)

  if (!grepl("^[A-Za-z]:$", drive_remote)) {
    cli::cli_abort(
      "drive_remote must be of the form 'X:' (but was '{drive_remote}')",
      call = call)
  }

  if (grepl("^[A-Za-z]:$", path_local)) {
    path_local <- paste0(path_local, "/")
  }

  if ((!grepl("^\\\\\\\\(.*)$", path_remote)) &&
      (!grepl("^//(.*)$", path_remote))) {
    cli::cli_abort(
      c("path_remote must be a network path.",
        i = "Network paths should start with // or \\\\\\\\",
        i = "Please check {path_remote}"),
      call = call)
  }

  path_remote <- clean_path_remote(path_remote)

  if (!file.exists(path_local)) {
    cli::cli_abort(
      c("Local mount point does not exist.",
        i = "Please check the mount point: {path_local}."),
      call = call)
  }

  if (drive_remote == "I:") {
    cli::cli_abort(
      c("You cannot use I: on a cluster job.",
        i = "I: is reserved for internal use.",
        i = "Please map {path_remote} to a different drive letter."),
      call = call)
  }

  ret <- list(
    path_remote = path_remote,
    path_local = clean_path_local(path_local),
    drive_remote = drive_remote)
  class(ret) <- "dide_path"

  ret
}


##' @export
as.character.dide_path <- function(x, ...) {
  if (is.null(x$rel)) {
    sprintf("(local) %s => %s => %s (remote)",
            x$path_local, x$path_remote, x$drive_remote)
  } else {
    sprintf("[rel: %s] (local) %s => %s => %s (remote)",
            x$rel, x$path_local, x$path_remote, x$drive_remote)
  }
}


##' @export
print.dide_path <- function(x, ...) {
  cat(paste0("<path mapping>: ", as.character(x), "\n"))
  invisible(x)
}


remote_path <- function(x, shares, platform) {
  x <- prepare_path(x, shares)
  if (platform == "linux") {
    unc_to_linux_hpc_mount(x)
  } else {
    file.path(x$drive_remote, x$rel)
  }
}


clean_path_local <- function(path) {
  clean_path(normalize_path(path))
}


# So here, we are cleaning a remote path, which should be a FQDN
# (hence, this only really applies to Windows, which can use FQDNs
# like paths). See zzz.R for the DIDE_SERVERS constants.

clean_path_remote <- function(path) {
  ## Make FQDN
  bits <- strsplit(clean_path(path), "/")[[1]]

  if (bits[3] %in% DIDE_SERVERS$from) {
    bits[3] <- DIDE_SERVERS$to[DIDE_SERVERS$from == bits[3]]
  } else {
    cli::cli_abort("Remote path {bits[3]} not found")
  }

  bits[3] <- paste0(bits[3], ".dide.ic.ac.uk")
  paste0(bits, collapse = "\\")
}


## TODO: This is a terrible name.
##
## This path converts a local path into a network path mapping
prepare_path <- function(path, mappings, error = TRUE) {
  if (!file.exists(path)) {
    stop("path does not exist: ", path)
  }
  for (m in mappings) {
    if (fs::path_has_parent(path, m$path_local)) {
      m$rel <- as.character(fs::path_rel(path, m$path_local))
      return(m)
    }
  }
  if (error) {
    cli::cli_abort("did not find network mapping for path '{path}'")
  } else {
    NULL
  }
}
