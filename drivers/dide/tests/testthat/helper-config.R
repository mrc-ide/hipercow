example_root <- function(mount_path, sub = "b/c", redis_url = NULL) {
  fs::dir_create(mount_path)
  path <- file.path(mount_path, sub)
  root <- suppressMessages(hipercow::hipercow_init(path))
  path <- normalize_path(path)
  shares <- dide_path(mount_path, "//qdrive/homes/wes", "X:")
  suppressMessages(
    hipercow::hipercow_configure("dide-windows", shares = shares,
                                 redis_url = redis_url, root = root))
  suppressMessages(
    hipercow::hipercow_configure("dide-linux", shares = shares,
                                 redis_url = redis_url, root = root))
  root
}


example_mounts <- function(root) {
  remote <- c("\\\\wpia-hn\\Cluster-Storage\\test1",
              "\\\\qdrive\\homes\\bob",
              "\\\\wpia-hn2\\Climate-Storage\\test2",
              "\\\\wpia-hn2\\vimc-cc2-storage\\test3")
  local <- file.path(root, c("t1", "home", "t2", "t3"))
  fs::dir_create(file.path(local, "sub"))
  cbind(remote = remote, local = local)
}
