## This function will detect home, temp and if the current working
## directory is not in one of those then continue on to detect the cwd
## too.
dide_cluster_paths <- function(shares, workdir = getwd()) {
  shares <- dide_check_shares(shares)
  shares <- dide_add_extra_workdir_share(shares, workdir)

  for (i in seq_along(shares)) {
    shares[[i]]$path_remote <- use_app_on_nas_south_ken(shares[[i]]$path_remote)
  }

  list(shares = shares, workdir = prepare_path(workdir, shares))
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
  cbind(remote = remote, local = m[, "local"])
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
  expected <- c("RemoteName", "LocalName")
  msg <- setdiff(expected, names(dat))
  if (length(msg) > 0) {
    stop("Failed to find expected names in wmic output: ",
         paste(msg, collapse = ", "))
  }
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
  if (inherits(shares, "path_mapping")) {
    shares <- set_names(list(shares), shares$name)
  }
  if (!is.list(shares)) {
    stop("Invalid input for 'shares'")
  }
  if (!all(vlapply(shares, inherits, "path_mapping"))) {
    stop("All elements of 'shares' must be a path_mapping")
  }

  remote <- vcapply(shares, "[[", "drive_remote", USE.NAMES = FALSE)
  dups <- unique(remote[duplicated(remote)])
  if (length(dups) > 0L) {
    stop("Duplicate remote drive names: ", paste(dups, collapse = ", "))
  }

  shares
}


dide_add_extra_workdir_share <- function(shares, workdir) {
  mapped <- vcapply(shares, "[[", "path_local")
  if (any(vlapply(mapped, fs::path_has_parent, path = workdir))) {
    ## Our local directory is already on a given share
    return(shares)
  }

  ## We did not find the local directory on a mapped share, look in the mounts
  mounts <- detect_mounts()
  i <- (nzchar(mounts[, "local"]) &
        vlapply(tolower(mounts[, "local"]), string_starts_with, x = workdir))
  if (sum(i) > 1L) {
    cli::cli_abort(c(
      "Having trouble determining the working directory mount point"),
      i = "You have two plausible mounts, how have you done this?")
  } else if (sum(i) == 0) {
    ## TODO: regular point of early confusion, point to our docs.
    cli::cli_abort(
      c("Running out of place: {workdir} is not on a network share",
        i = "You need to use a network share, not a path on your computer"))
  }
  drive <- available_drive(shares, mounts[i, "local"])
  workdir <- path_mapping("workdir", mounts[i, "local"],
                          mounts[i, "remote"], drive)
  c(shares, list(workdir))
}


## If we're mounting some local drive (not home/temp) then on windows
## we'll reflect the local drive letter. Otherwise on linux/mac we'll
## pick from a late letter.
available_drive <- function(shares, local_mount, prefer = NULL) {
  if (grepl("^[A-Za-z]:", local_mount)) {
    local_mount
  } else {
    used <- toupper(substr(vcapply(shares, "[[", "drive_remote"), 1, 1))
    pos <- c(prefer, LETTERS[22:26])
    paste0(setdiff(pos, used)[[1L]], ":")
  }
}
