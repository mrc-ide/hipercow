example_root <- function(mount_path, sub = "b/c") {
  fs::dir_create(mount_path)
  path <- file.path(mount_path, sub)
  root <- suppressMessages(hipercow::hipercow_init(path))
  path <- normalize_path(path)
  shares <- windows_path(mount_path, "//host/share/path", "X:")
  suppressMessages(
    hipercow::hipercow_configure("windows", shares = shares, root = root))
  root
}


example_mounts <- function(root) {
  remote <- c("\\\\projects\\other",
              "\\\\qdrive\\homes\\bob",
              "\\\\wpia-hn\\Project",
              "\\\\projects\\tmp",
              "\\\\wpia-hn\\newshare")
  local <- file.path(root, c("other", "home", "proj", "temp", "sk"))
  fs::dir_create(file.path(local, "sub"))
  cbind(remote = remote, local = local)
}
