bootstrap_update <- function(root = NULL) {
  path_script <- "hipercow/bootstrap-windows.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  writelines_if_different(
    readLines(hipercow_windows_file("bootstrap.R")),
    path_script_abs)
  hipercow::hipercow_provision("script", script = path_script, root = root)
}
