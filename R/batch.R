write_batch_task_run <- function(task_id, workdir, root) {
  data <- template_data(workdir, root)
  data$hermod_task_id <- task_id
  str <- glue_whisker(read_template("task_run"), data)
  path <- file.path(root$path$tasks, task_id, "run.bat")
  writeLines(str, path)
}


read_template <- function(name) {
  read_lines(hermod_file(sprintf("templates/%s.bat", name)))
}


template_data <- function(workdir, root) {
  config <- hermod_config(root)

  if (!fs::path_has_parent(workdir, root$path$root)) {
    cli::cli_abort(c(
      "Expected working directory to be within hermod root",
      i = "Working directory: '{workdir}'",
      i = "hermod root: '{workdir}'"))
  }
  workdir <- prepare_path(workdir, config$shares)
  hermod_root <- prepare_path(root$path$root, config$shares)

  ## Same path, absolute, that will be used remotely
  hermod_root_abs <- windows_path(
    file.path(hermod_root$drive_remote, hermod_root$rel))

  r_version_str <- paste(unclass(config$r_version)[[1]], collapse = "_")

  network_shares_data <- list(
    drive = lapply(config$shares, "[[", "drive_remote"),
    path = lapply(config$shares, "[[", "path_remote"))
  network_shares_create <- glue_whisker(
    "ECHO mapping {{drive}} -^> {{path}}\nnet use {{drive}} {{path}} /y",
    network_shares_data)
  network_shares_delete <- glue_whisker(
    "ECHO Removing mapping {{drive}}\nnet use {{drive}} /delete /y",
    network_shares_data)

  list(hostname = hostname(),
       date = as.character(Sys.Date()),
       hermod_version = hermod_version(),
       r_version = r_version_str,
       network_shares_create = paste(network_shares_create, collapse = "\n"),
       network_shares_delete = paste(network_shares_delete, collapse = "\n"),
       hermod_drive = workdir$drive_remote,
       hermod_workdir = paste0("\\", windows_path(workdir$rel)),
       hermod_root = hermod_root_abs,
       cluster_name = config$cluster)
}
