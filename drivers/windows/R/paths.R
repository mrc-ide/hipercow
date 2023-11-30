##' Describe a path mapping for use when setting up jobs on the cluster.
##' @title Describe a path mapping
##'
##' @param name Name of this map.  Can be anything at all, and is used
##'   for information purposes only.
##'
##' @param path_local The point where the drive is attached locally.
##'   On Windows this will be something like "Q:/", on Mac something
##'   like "/Volumes/mountname", and on Linux it could be anything at
##'   all, depending on what you used when you mounted it (or what is
##'   written in `/etc/fstab`)
##'
##' @param path_remote The network path for this drive.  It
##'   will look something like `\\\\fi--didef3.dide.ic.ac.uk\\tmp\\`.
##'   Unfortunately backslashes are really hard to get right here and
##'   you will need to use twice as many as you expect (so *four*
##'   backslashes at the beginning and then two for each separator).
##'   If this makes you feel bad know that you are not alone:
##'   https://xkcd.com/1638 -- alternatively you may use forward
##'   slashes in place of backslashes (e.g. `//fi--didef3.dide.ic.ac.uk/tmp`)
##'
##' @param drive_remote The place to mount the drive on the cluster.
##'   We're probably going to mount things at Q: and T: already so
##'   don't use those.  And things like C: are likely to be used.
##'   Perhaps there are some guidelines for this somewhere?
##'
##' @export
##' @author Rich FitzJohn
path_mapping <- function(name, path_local, path_remote, drive_remote) {
  assert_scalar_character(name)
  assert_scalar_character(path_local)
  assert_scalar_character(path_remote)
  assert_scalar_character(drive_remote)

  if (!grepl("^[A-Za-z]:$", drive_remote)) {
    stop(sprintf("drive_remote must be of the form 'X:' (but was '%s')",
                 drive_remote))
  }

  if (grepl("^[A-Za-z]:$", path_local)) {
    path_local <- paste0(path_local, "/")
  }

  path_remote <- clean_path_remote(path_remote)
  if (!grepl("^\\\\\\\\(.*)$", path_remote)) {
    stop("path_remote must be a network path, starting with // or \\\\\\\\")
  }

  if (!file.exists(path_local)) {
    stop("Local mount point does not exist: ", path_local)
  }

  ret <- list(
    name = name,
    path_remote = path_remote,
    path_local = clean_path_local(path_local),
    drive_remote = drive_remote)
  class(ret) <- "path_mapping"

  ret
}


##' @export
as.character.path_mapping <- function(x, ...) {
  if (is.null(x$rel)) {
    sprintf("(local) %s => %s => %s (remote)",
            x$path_local, x$path_remote, x$drive_remote)
  } else {
    sprintf("[rel: %s] (local) %s => %s => %s (remote)",
            x$rel, x$path_local, x$path_remote, x$drive_remote)
  }
}


##' @export
print.path_mapping <- function(x, ...) {
  cat(paste0("<path mapping>: ", as.character(x), "\n"))
  invisible(x)
}


remote_path <- function(x, shares) {
  x <- prepare_path(x, shares)
  windows_path(file.path(x$path_remote, x$rel, fsep = "/"))
}


clean_path_local <- function(path) {
  clean_path(normalize_path(path))
}


clean_path_remote <- function(path) {
  ## Make FQDN
  bits <- strsplit(clean_path(path), "/")[[1]]

  ## Catch varieties of wpia-hn, as we need to add .hpc in domain
  wpia_hn <- c("wpia-hn", "wpia-hn.dide.ic.ac.uk", "wpia-hn.dide.local")
  if (bits[3] %in% wpia_hn) {
    bits[3] <- "wpia-hn.hpc"
  }

  ## This contains... empty, empty, server-name, share, dir ...
  ## So server_name should always be index 3.
  ## Remove .dide.local if we find it.

  if (grepl(".dide.local", bits[3], ignore.case = TRUE)) {
    bits[3] <- sub(".dide.local", "", bits[3], ignore.case = TRUE)
  }

  ## Add .dide.ic.ac.uk if it's not there.
  if (!grepl(".dide.ic.ac.uk", bits[3], ignore.case = TRUE)) {
    bits[3] <- paste0(bits[3], ".dide.ic.ac.uk")
  }

  ## re_assemble
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
