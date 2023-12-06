write_batch_task_run <- function(task_id, config, path_root) {
  data <- template_data(config, path_root)
  data$hermod_task_id <- task_id
  str <- glue_whisker(read_template("task_run.bat"), data)
  path <- file.path(path_root, "hermod", "tasks", task_id, BATCH_RUN)
  writeLines(str, path)
  path
}


write_batch_provision_script <- function(id, config, path_root) {
  path <- file.path(path_root, "hermod", "provision", id)
  data <- template_data(config, path_root)
  data$id <- id
  str <- glue_whisker(read_template("provision.bat"), data)
  path_bat <- file.path(path, "provision.bat")
  fs::dir_create(path)
  writeLines(str, path_bat)
  path_bat
}


read_template <- function(name) {
  read_lines(hermod_windows_file(sprintf("templates/%s", name)))
}


template_data <- function(config, path_root) {
  hermod_root <- prepare_path(path_root, config$shares)

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
       date = as.character(Sys.time()),
       hermod_version = hermod_version(),
       r_version = version_string(config$r_version),
       network_shares_create = paste(network_shares_create, collapse = "\n"),
       network_shares_delete = paste(network_shares_delete, collapse = "\n"),
       hermod_root_drive = hermod_root$drive_remote,
       hermod_root_path = paste0("\\", windows_path(hermod_root$rel)),
       cluster_name = config$cluster)
}
