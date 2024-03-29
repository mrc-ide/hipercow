windows_configure <- function(shares = NULL, r_version = NULL) {
  path <- getwd()
  r_version <- select_r_version(r_version)
  r_version_str <- version_string(r_version, ".")
  path_lib <- file.path("hipercow", "lib", "windows", r_version_str)
  stopifnot(fs::dir_exists(file.path(path, "hipercow")))
  fs::dir_create(file.path(path, path_lib))
  list(cluster = "wpia-hn",
       shares = dide_cluster_paths(shares, path),
       r_version = r_version,
       path_lib = unix_path_slashes(path_lib))
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
