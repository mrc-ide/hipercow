path <- sprintf("I:/bootstrap/%s",
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
pkgs <- c("hermod", "remotes", "pkgdepends")
repos <- c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org")
install.packages(pkgs, path_next, repos = repos)
ok <- all(file.exists(file.path(path_next, pkgs, "Meta", "package.rds")))
if (!ok) {
  stop("Failed to install all packages")
}
curr_exists <- file.exists(path)
if (curr_exists) {
  file.rename(path, path_prev)
}
file.rename(path_next, path)
if (curr_exists) {
  unlink(path_prev, recursive = TRUE)
}
