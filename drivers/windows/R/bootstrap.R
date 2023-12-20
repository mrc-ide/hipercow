bootstrap_update <- function(development = NULL, root = NULL) {
  path_script <- "hipercow/bootstrap-windows.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)
  dir.create(dirname(path_script_abs), FALSE, TRUE)
  bootstrap <- read_template("bootstrap.R")
  if (is.null(development)) {
    data <- list(bootstrap_path = "bootstrap",
                 development_ref = "NULL")
  } else {
    data <- list(bootstrap_path = "bootstrap-dev",
                 development_ref = dquote(development))
  }
  writelines_if_different(glue_whisker(bootstrap, data),
                          path_script_abs)
  hipercow::hipercow_provision("script", script = path_script, root = root)
}
