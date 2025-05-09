path <- sprintf("{{bootstrap_path}}/%s",
                paste(unclass(getRversion())[[1]], collapse = "."))
path_next <- sprintf("%s-next", path)
path_prev <- sprintf("%s-prev", path)
unlink(path_next, recursive = TRUE)
unlink(path_prev, recursive = TRUE)
if (file.exists(path_prev)) {
  stop("Failed to remove previous-previous library")
}
if (file.exists(path_next)) {
  stop("Failed to remove previous-next library")
}
dir.create(path_next, FALSE, TRUE)
.libPaths(path_next, FALSE)
message(sprintf("Installing packages into %s", path_next))
pkgs <- {{bootstrap_pkgs}}
repos <- {{bootstrap_repos}}
install.packages(pkgs, path_next, repos = repos)
ok <- all(file.exists(file.path(path_next, pkgs, "Meta", "package.rds")))
if (!ok) {
  stop("Failed to install all packages")
}

curr_exists <- file.exists(path)
if (curr_exists) {
  # Default behaviour is to warn and just continue if the rename
  # fails, which is wild, and also terrible.
  stopifnot(file.rename(path, path_prev))
}
stopifnot(file.rename(path_next, path))
if (curr_exists) {
  unlink(path_prev, recursive = TRUE)
}

if (!is.null({{development_ref}})) {
  .libPaths(path, FALSE)
  ## We need to install this directly into the final library,
  ## otherwise we can't move things over because "remotes" will have
  ## been loaded and that creates a lock.
  remotes::install_github("mrc-ide/hipercow", ref = {{development_ref}},
                          upgrade = FALSE)
}
