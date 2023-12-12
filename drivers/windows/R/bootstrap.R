bootstrap_update <- function(root = NULL) {
  path_script <- "hermod/bootstrap-windows.R"
  path_root <- hermod:::hermod_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  writelines_if_different(
    readLines(hermod_windows_file("bootstrap.R")),
    path_script_abs)
  hermod::hermod_provision("script", script = path_script, root = root)
}
