bootstrap_update <- function(development = FALSE, root = NULL) {
  path_script <- "hipercow/bootstrap-windows.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  bootstrap <- read_template("bootstrap.R")
  path_bootstrap <- if (development) "bootstrap-dev" else "bootstrap"
  writelines_if_different(
    glue_whisker(bootstrap, list(bootstrap_path = path_bootstrap)),
    path_script_abs)
  hipercow::hipercow_provision("script", script = path_script, root = root)
}
