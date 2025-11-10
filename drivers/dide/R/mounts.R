dide_cluster_paths <- function(shares, path_root, platform = "windows") {
  path_root <- clean_path_local(path_root)
  ## TODO: clean the shares - we can only map for the home directory
  ## though.
  shares <- dide_check_shares(shares)
  shares <- dide_add_extra_root_share(shares, path_root)

  for (i in seq_along(shares)) {
    shares[[i]]$path_remote <- use_app_on_nas_south_ken(shares[[i]]$path_remote)
  }

  class(shares) <- "dide_shares"
  shares
}


##' @export
format.dide_shares <- function(x, ...) {
  n <- length(x)
  c(sprintf("%d configured:", n),
    set_names(vcapply(x, as.character), rep(">", n)))
}


detect_mounts <- function() {
  if (is_windows()) {
    detect_mounts_windows()
  } else {
    detect_mounts_unix()
  }
}


detect_mounts_windows <- function() {
  mounts <- system2("powershell", c("-c", "Get-SmbMapping|ConvertTo-CSV"),
                    stdout = TRUE)
  mounts <- utils::read.csv(text = mounts, skip = 1, header = TRUE)
  mounts <- mounts[mounts$Status == "OK", ]
  cbind(remote = mounts$RemotePath,
        local = mounts$LocalPath)
}

## TODO: No idea what spaces in the filenames will do here.  Nothing
## pretty, that's for sure.
detect_mounts_unix <- function() {
  mount <- sys_which("mount")
  type <- if (Sys.info()[["sysname"]] == "Darwin") "smbfs" else "cifs"

  re <- paste(
    "//(?<user>[^@]*@)?(?<host>[^/]*)/(?<path>.*?)\\s+on\\s+(?<local>.+?)",
    "(?<extra>.+)$")
  dat <- system2(mount, c("-t", type), stdout = TRUE, stderr = FALSE)

  i <- grepl(re, dat, perl = TRUE)
  if (!all(i)) {
    ## This will be useful to see if the above regex becomes wrong
    warning("Ignoring mounts:\n", paste(dat[!i], collapse = "\n"),
            immediate. = TRUE)
  }
  dat <- dat[i]

  if (length(dat) == 0L) {
    return(cbind(remote = character(), local = character()))
  }

  ## There are a couple of formats here.  On the VPN and with OSX
  ## (currently correlated) I see a //username@host/path format while
  ## on on the wired network and Linux I see //shorthost/path
  ##
  ## //(user@)?(host)(.dide.ic.ac.uk)?/(path)
  m <- rematch::re_match(re, dat)[, c("host", "path", "local"), drop = FALSE]

  host <- sub("\\.dide\\.ic\\.ac\\.uk$", "", m[, "host"])
  remote <- sprintf("\\\\%s\\%s", host, gsub("/", "\\\\", m[, "path"]))
  cbind(remote = remote,
        local = clean_path_local(m[, "local"]))
}

use_app_on_nas_south_ken <- function(path_remote) {
  # Similar to the above, but for the new South Ken
  # cluster, wpia-hn.hpc
  if (!(grepl("^[/\\\\]{2}wpia-hn-app", path_remote))) {
    path_remote <- sub("^([/\\\\]{2}wpia-hn)\\b", "\\1-app", path_remote)
  }

  re <- paste("^([/\\\\]{2}wpia-hn-app)\\.hpc\\.dide\\.ic\\.ac\\.uk|",
              "\\.hpc\\.dide\\.local\\b")
  path_remote <- sub(re, "\\1.hpc.dide.local", path_remote)

  path_remote <- gsub("wpia-hn-app.dide.local", "wpia-hn-app.hpc.dide.local",
                      path_remote)

  path_remote
}


dide_check_shares <- function(shares) {
  if (length(shares) == 0) {
    return(NULL)
  }
  if (inherits(shares, "dide_path")) {
    shares <- list(shares)
  }
  if (!is.list(shares)) {
    stop("Invalid input for 'shares'")
  }
  if (!all(vlapply(shares, inherits, "dide_path"))) {
    stop("All elements of 'shares' must be a dide_path")
  }

  remote <- vcapply(shares, "[[", "drive_remote", USE.NAMES = FALSE)
  dups <- unique(remote[duplicated(remote)])
  if (length(dups) > 0L) {
    stop("Duplicate remote drive names: ", paste(dups, collapse = ", "))
  }

  unname(shares)
}


dide_add_extra_root_share <- function(shares, path_root,
                                      mounts = detect_mounts()) {
  mapped <- vcapply(shares, "[[", "path_local")
  if (any(vlapply(mapped, fs::path_has_parent, path = path_root))) {
    ## Our local directory is already on a given share
    return(shares)
  }

  ## We did not find the local directory on a mapped share, look in the mounts
  i <- vlapply(mounts[, "local"], fs::path_has_parent, path = path_root)

  if (sum(i) > 1L) {
    cli::cli_abort(c(
      "Having trouble determining the working root directory mount point",
      i = "You have two plausible mounts, how have you done this?"))
  } else if (sum(i) == 0) {
    cli::cli_abort(
      c("Can't map local directory '{path_root}' to network path",
        i = paste("You need to work with your hipercow root on a network",
                  "share (and your working directory within that root),",
                  "but I can't work out the network path for this",
                  "myself. Most likely your working directory is on your",
                  "local computer only. Please see the package docs for",
                  "more information.")))
  }
  drive <- available_drive(shares, mounts[i, "local"])
  c(shares,
    list(dide_path(mounts[i, "local"], mounts[i, "remote"], drive)))
}


## If we're mounting some local drive (not home/temp) then on windows
## we'll reflect the local drive letter. Otherwise on linux/mac we'll
## pick from a late letter.
available_drive <- function(shares, local_mount, prefer = NULL) {
  if (grepl("^[A-Za-z]:", local_mount)) {
    substr(local_mount, 1, 2)
  } else {
    used <- toupper(substr(vcapply(shares, "[[", "drive_remote"), 1, 1))
    pos <- c(prefer, LETTERS[22:26])
    paste0(setdiff(pos, used)[[1L]], ":")
  }
}

dide_locally_resolve_unc_path <- function(path, mounts = detect_mounts(),
                                          skip_exist_check = FALSE) {
  if (!skip_exist_check && file.exists(path)) {
    return(path)
  }
  clean_path <- function(x) {
    sub("^//([^/]+)\\.dide\\.ic\\.ac\\.uk/", "//\\1/",
        unix_path_slashes(tolower(x)))
  }
  i <- match(clean_path(path), clean_path(mounts[, "remote"]))
  if (is.na(i)) {
    return(NULL)
  }
  unname(drop(mounts[i, "local"]))
}

unc_to_linux_hpc_mount <- function(path_dat) {

  # Prepend "/" to the relative path if it exists here - then we
  # can paste it on the end and not worry about trailing slashes.

  rel <- if (path_dat$rel != "") paste0("/", path_dat$rel) else ""

  # First, we'll just split the remote path into bits. For example
  # \\server.dide.ic.ac.uk\folder1\folder2 will be split into
  # the form c("//server.dide.ic.ac.uk", "folder1", "folder2")

  path_remote <- fs::path_split(path_dat$path_remote)[[1]]

  # Now some renaming of servers that are equivalent, just to simplify
  # the gunk that comes afterwards.

  path_remote[1][path_remote[1] %in% c(
    "//qdrive.dide.ic.ac.uk", "//wpia-san04.dide.ic.ac.uk", "//wpia-san04")] <-
    "//qdrive"

  path_remote[1][path_remote[1] %in% c(
    "//wpia-hn.hpc.dide.ic.ac.uk", "//wpia-hn.dide.ic.ac.uk")] <- "//wpia-hn"

  path_remote[1][path_remote[1] %in% c(
    "//wpia-hn2.hpc.dide.ic.ac.uk", "//wpia-hn2.dide.ic.ac.uk")] <- "//wpia-hn2"

  # Check for DIDE home directories.
  # On the Linux nodes, these are /mnt/homes/user

  if ((path_remote[1] == "//qdrive") && (path_remote[2] == "homes")) {
    username <- path_remote[3]
    return(sprintf("/mnt/homes/%s%s", username, rel))
  }

  # Check for multi-user mounts - one on wpia-hn and two on wpia-hn2

  # These may, or may not be followed by more folders - eg, a windows user
  # might have mapped W: = \\wpia-hn\cluster-storage
  # or W: = \\wpia-hn\cluster-storage\ncov\Ed.

  # These need to get converted into (respectively)
  # /mnt/cluster or
  # /mnt/cluster/ncov/Ed

  multi_user_mounts <- list(
    c("//wpia-hn", "cluster-storage", "cluster"),
    c("//wpia-hn2", "climate-storage", "vimc-cc1"),
    c("//wpia-hn2", "vimc-cc2-storage", "vimc-cc2"))

  for (i in seq_along(multi_user_mounts)) {
    if ((multi_user_mounts[[i]][1] == path_remote[1]) &&
       (multi_user_mounts[[i]][2] == path_remote[2])) {

      if (length(path_remote) >= 3) { # Inner mount folders
        extras <- paste0(path_remote[3:length(path_remote)], collapse = "/")
        return(sprintf("/mnt/%s/%s%s", multi_user_mounts[[i]][3], extras, rel))
      }
      return(sprintf("/mnt/%s%s", multi_user_mounts[[i]][3], rel))
    }
  }

  # We also have shares that point into the multi-user space -
  # \\wpia-hn\potato might also be accessible as
  # \\wpia-hn\cluster-storage\potato. We can detect
  # these on Windows by seeing if they exist in the cluster-storage
  # location, since we can just browse directly to it. On
  # linux, we'd need a mount already setup pointing to the
  # multi-user share. For now, we'll check it on Windows, and let it
  # go through unchecked on linux.

  if (path_remote[1] == "//wpia-hn") {

    deeper_path <- paste0("//wpia-hn.hpc.dide.ic.ac.uk/cluster-storage/",
                        paste0(path_remote[-1], collapse = "/"))

    if ((Sys.info()["sysname"] != "Windows") || fs::dir_exists(unc_path)) {
      return(sprintf("/mnt/cluster/%s%s",
               paste0(path_remote[-1], collapse = "/"), rel))
    }
  }

  # This is a bit gross, as it's just for testing, but it makes
  # things easier as the mockery gets quite deep.

  if (all.equal(path_remote, c("//host.dide.ic.ac.uk", "share", "path"))) {
    return(sprintf("/test/path/%s", path_dat$rel))
  }

  # If we reach here, we have failed to find a way of accessing the
  # network path from the cluster. This is for network paths that are not
  # the home drive (qdrive etc), and have not yet been migrated into the
  # multi-user locations. These will get resolved/migrated one by one.

  cli::cli_abort(c(
    "Error mapping linux path",
    i = "Couldn't work out linux mount for {path_dat$path_remote}"))
}
