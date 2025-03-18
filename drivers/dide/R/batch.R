read_template <- function(name) {
  read_lines(hipercow_dide_file(sprintf("templates/%s", name)))
}

template_data_task_run <- function(task_id, config, path_root) {
  platform <- config$platform
  data <- template_data_common(config, path_root)
  data$task_id <- task_id
  data$task_id_1 <- substr(task_id, 1, 2)
  data$task_id_2 <- substr(task_id, 3, nchar(task_id))

  data$hipercow_library <- paste(
    remote_path(file.path(path_root, config$path_lib), config$shares,
                config$platform),
    bootstrap_path_from_config(config),
    sep = path_delimiter(config$platform))

  data$renviron_path <-
    remote_path(path_to_task_file(path_root, task_id, "Renviron"),
                config$shares, config$platform)

  data
}

template_data_provision_script <- function(id, config, path_root) {
  data <- template_data_common(config, path_root)
  data$id <- id
  data
}

get_hipercow_root <- function(platform, path_data) {
  if (platform == "windows") {
    paste0("\\", windows_path_slashes(path_data$rel))
  } else {
    unc_to_linux_hpc_mount(path_data)
  }
}

template_data_common <- function(config, path_root) {
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

  # Convert R_version into string with separator:
  #    For windows, underscores for: call setr64_4_4_2.bat
  #    For linux,          dots for: module load R/4.4.2

  r_version <- version_string(config$r_version, sep = (
                              if (config$platform == "windows") "_" else "."))

  list(
    hostname = hipercow:::hostname(),
    date = as.character(Sys.time()),
    hipercow_version = hipercow_version(),
    hipercow_dide_version = hipercow_dide_version(),
    r_version = r_version,
    network_shares_create = paste(network_shares_create, collapse = "\n"),
    network_shares_delete = paste(network_shares_delete, collapse = "\n"),
    hipercow_root_drive = hipercow_root$drive_remote,
    hipercow_root_path = get_hipercow_root(config$platform, hipercow_root),
    cluster_name = config$cluster)
}

bootstrap_path_from_config <- function(config) {
  use_development <- getOption("hipercow.development", NULL)
  sprintf("%s/%s", bootstrap_path(config$platform, use_development),
          version_string(config$r_version, "."))
}
