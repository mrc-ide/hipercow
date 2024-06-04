cmdstan_install <- function(cmdstan_version = NULL,
                            cmdstanr_remotes = FALSE,
                            overwrite = FALSE,
                            root = NULL) {
  path_script <- "hipercow/install-cmdstan.R"
  path_root <- hipercow:::hipercow_root(root)$path$root
  path_script_abs <- file.path(path_root, path_script)

  data <- list(cmdstan_version = deparse1(cmdstan_version),
               cmdstanr_use_remotes = deparse1(cmdstanr_use_remotes),
               overwrite = deparse1(overwrite))
  template <- read_template("stan.R")
  writelines_if_different(glue_whisker(template, data),
                          path_script_abs)

  hipercow::hipercow_provision("script", script = path_script, root = root)
}
