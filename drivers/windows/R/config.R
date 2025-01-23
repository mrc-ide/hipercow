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


select_r_version <- function(r_version, ours = getRversion(),
                             valid = unique(c(
                               r_versions("windows"),
                               r_versions("linux")))) {

  select_by_match <- is.null(r_version)
  if (select_by_match) {
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
    if (!(r_version %in% valid)) {
      stop("Unsupported R version: ", as.character(r_version))
    }
  }

  check_old_versions(valid, r_version, ours)

  r_version
}


check_old_versions <- function(valid, selected, ours) {
  ## So we have a set of versions, and we want to know how many minor
  ## versions behind they are:
  series <- valid[, -3]
  age <- match(series, rev(unique(series))) - 1

  age_selected <- age[match(selected[, -3], series)]
  age_ours <- age[match(ours[, -3], series)]
  current <- max(valid)

  if (age_selected > 1) {
    cli::cli_alert_warning(
      "Selected an old R version '{selected}' to use on the cluster")
    cli::cli_alert_info(paste(
      "The version that you have selected to run on the cluster is now",
      "more than 1 minor version old, which means that it no longer has",
      "new builds of binary packages on CRAN.  This means provisioning",
      "will be slow and eventually will fail."))
    cli::cli_alert_info(paste(
      "The minimum recommended version is '{min(valid[age <= 1])}' and",
      "the most recent supported version is '{current}'"))
    cmd <- sprintf('hipercow_configure("windows", r_version = "%s")', current)
    cli::cli_alert_info(paste(
      "You can select a newer R version on the cluster without affecting",
      "your local installation by running '{nonbreaking(cmd)}'"))
  }

  is_old <- ours < current && (is.na(age_ours) || age_ours > 2)
  if (is_old) {
    cli::cli_alert_warning(paste(
      "Your {.strong local} R installation is very old ('{ours}');",
      "you should upgrade it!"))
  }
}
