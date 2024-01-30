dide_cluster_paths <- function(shares, path_root) {
  path_root <- clean_path_local(path_root)
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
  windir <- Sys.getenv("WINDIR", "C:\\Windows")
  methods <- c("csv",
               paste0(windir, "\\System32\\wbem\\en-US\\csv"),
               paste0(windir, "\\System32\\wbem\\en-GB\\csv"))

  for (meth in methods) {
    res <- wmic_call(meth)
    if (res$success) {
      return(res$result)
    }
  }

  stop("Could not determine windows mounts using wmic\n", res$result)
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


## Windows support:
wmic_call <- function(formatstr) {
  ## ordinarily we'd use system2 but that writes a string that can't
  ## be parsed under Rgui due to odd encoding.
  ## https://stackoverflow.com/q/61067574
  ## Using system() does not seem to suffer the same problem
  cmd <- sprintf('wmic netuse list brief /format:"%s"', formatstr)
  res <- tryCatch(
    list(success = TRUE,
         result = wmic_parse(system_intern_check(cmd))),
    error = function(e) list(success = FALSE, result = e$message))
}


wmic_parse <- function(x) {
  tmp <- tempfile()
  writeLines(x, tmp)
  on.exit(unlink(tmp))
  dat <- utils::read.csv(tmp, stringsAsFactors = FALSE)
  expected <- c("RemoteName", "LocalName", "ConnectionState")
  msg <- setdiff(expected, names(dat))
  if (length(msg) > 0) {
    stop("Failed to find expected names in wmic output: ",
         paste(msg, collapse = ", "))
  }
  dat <- dat[dat$ConnectionState %in% "Connected", ]
  cbind(remote = dat$RemoteName, local = dat$LocalName)
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
  if (inherits(shares, "windows_path")) {
    shares <- list(shares)
  }
  if (!is.list(shares)) {
    stop("Invalid input for 'shares'")
  }
  if (!all(vlapply(shares, inherits, "windows_path"))) {
    stop("All elements of 'shares' must be a windows_path")
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
    list(windows_path(mounts[i, "local"], mounts[i, "remote"], drive)))
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


dide_locally_resolve_unc_path <- function(path, mounts = detect_mounts()) {
  if (file.exists(path)) {
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
