write_batch_task_run <- function(task_id, config, path_root) {
  data <- template_data(config, path_root)
  data$task_id <- task_id
  data$task_id_1 <- substr(task_id, 1, 2)
  data$task_id_2 <- substr(task_id, 3, nchar(task_id))
  str <- glue_whisker(read_template("task_run.bat"), data)
  path <- path_to_task_file(path_root, task_id, BATCH_RUN)
  writeLines(str, path)
  path
}


write_batch_provision_script <- function(id, config, path_root) {
  data <- template_data(config, path_root)
  data$id <- id
  str <- glue_whisker(read_template("provision.bat"), data)
  path_job <- file.path(path_root, "hipercow", "provision", id)
  path <- file.path(path_job, "provision.bat")
  fs::dir_create(path_job)
  writeLines(str, path)
  path
}


read_template <- function(name) {
  read_lines(hipercow_windows_file(sprintf("templates/%s", name)))
}


template_data <- function(config, path_root) {
  hipercow_root <- prepare_path(path_root, config$shares)

  network_shares_data <- list(
    drive = lapply(config$shares, "[[", "drive_remote"),
    path = lapply(config$shares, "[[", "path_remote"))
  network_shares_create <- glue_whisker(
    "ECHO mapping {{drive}} -^> {{path}}\nnet use {{drive}} {{path}} /y",
    network_shares_data)
  network_shares_delete <- glue_whisker(
    "ECHO Removing mapping {{drive}}\nnet use {{drive}} /delete /y",
    network_shares_data)

  ## Semicolon delimited list on windows; see "Managing libraries" in
  ## https://cran.r-project.org/doc/manuals/r-release/R-admin.html
  hipercow_library <- paste(config$path_lib, path_bootstrap(config), sep = ";")

  list(
    hostname = hostname(),
    date = as.character(Sys.time()),
    hipercow_version = hipercow_version(),
    hipercow_windows_version = hipercow_windows_version(),
    r_version = version_string(config$r_version),
    network_shares_create = paste(network_shares_create, collapse = "\n"),
    network_shares_delete = paste(network_shares_delete, collapse = "\n"),
    hipercow_root_drive = hipercow_root$drive_remote,
    hipercow_root_path = paste0("\\", windows_path_slashes(hipercow_root$rel)),
    hipercow_library = hipercow_library,
    cluster_name = config$cluster)
}


path_bootstrap <- function(config) {
  use_development <- getOption("hipercow.development", FALSE)
  base <- if (use_development) "bootstrap-dev" else "bootstrap"
  sprintf("I:/%s/%s", base, version_string(config$r_version, "."))
}
