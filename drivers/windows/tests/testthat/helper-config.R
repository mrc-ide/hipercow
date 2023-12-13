example_root <- function(mount_path, sub = "b/c") {
  fs::dir_create(mount_path)
  path <- file.path(mount_path, sub)
  root <- suppressMessages(hermod::hermod_init(path))
  path <- normalize_path(path)
  shares <- windows_path("home", mount_path, "//host/share/path", "X:")
  suppressMessages(
    config <- hermod::hermod_configure("windows", shares = shares, root = root))
  root
}


example_mounts <- function(root) {
  remote <- c("\\\\fi--didef3\\other",
              "\\\\fi--san03\\homes\\bob",
              "\\\\fi--didenas1\\Project",
              "\\\\fi--didef3\\tmp",
              "\\\\wpia-hn\\newshare")
  local <- file.path(root, c("other", "home", "proj", "temp", "sk"))
  fs::dir_create(file.path(local, "sub"))
  cbind(remote = remote, local = local)
}
