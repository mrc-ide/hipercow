write_batch_task_run <- function(task_id, config, path_root) {

  run_on_linux <- config$platform == "linux"
  template_file <- if (run_on_linux) "task_run.sh" else "task_run.bat"
  out_script <- if (run_on_linux) SH_RUN else BATCH_RUN
  write_os_lines <- if (run_on_linux) write_linux_lines else writeLines

  data <- template_data_task_run(task_id, config, path_root)
  str <- glue_whisker(read_template(template_file), data)
  path <- path_to_task_file(path_root, task_id, out_script)
  write_os_lines(str, path)

  if (run_on_linux) {
    path_dat <- prepare_path(path, config$shares)
    linux_path <- path_on_linux(path_dat)
    wrap_path <- path_to_task_file(path_root, task_id, SH_WRAP_RUN)
    write_linux_lines(
      sprintf("python -u /opt/hpcnodemanager/kwrap.py %s", linux_path), 
        wrap_path)
  }
  path
}


write_batch_provision_script <- function(id, config, path_root) {
  run_on_linux <- config$platform == "linux"
  template_file <- if (run_on_linux) "provision.sh" else "provision.bat"
  write_os_lines <- if (run_on_linux) write_linux_lines else writeLines

  data <- template_data_provision_script(id, config, path_root)
  str <- glue_whisker(read_template(template_file), data)
  path_job <- file.path(path_root, "hipercow", "provision", id)
  path <- file.path(path_job, template_file)
  fs::dir_create(path_job)
  write_os_lines(str, path)

  if (run_on_linux) {
    path_dat <- prepare_path(path, config$shares)
    linux_path <- path_on_linux(path_dat)
    wrap_path <- file.path(dirname(path), "wrap_provision.sh")
    write_linux_lines(sprintf("python -u /opt/hpcnodemanager/kwrap.py %s",
                              linux_path), wrap_path)
  }
  path
}


read_template <- function(name) {
  read_lines(hipercow_windows_file(sprintf("templates/%s", name)))
}

template_data_task_run <- function(task_id, config, path_root) {
  platform <- config$platform
  data <- template_data_common(config, path_root)
  data$task_id <- task_id
  data$task_id_1 <- substr(task_id, 1, 2)
  data$task_id_2 <- substr(task_id, 3, nchar(task_id))

  data$hipercow_library <- paste(
    remote_path(file.path(path_root, config$path_lib), config$shares,
                platform == "linux"),
    path_bootstrap(config),
    sep = path_delimiter(config$platform))

  data$renviron_path <-
    remote_path(path_to_task_file(path_root, task_id, "Renviron"),
                config$shares, platform == "linux")

  data
}

template_data_provision_script <- function(id, config, path_root) {
  data <- template_data_common(config, path_root)
  data$id <- id
  data
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

  r_version <- config$r_version
  if (config$platform != "linux") {
    r_version <- version_string(config$r_version)
  }

  list(
    hostname = hipercow:::hostname(),
    date = as.character(Sys.time()),
    hipercow_version = hipercow_version(),
    hipercow_windows_version = hipercow_windows_version(),
    r_version = r_version,
    network_shares_create = paste(network_shares_create, collapse = "\n"),
    network_shares_delete = paste(network_shares_delete, collapse = "\n"),
    hipercow_root_drive = hipercow_root$drive_remote,
    hipercow_root_path = paste0("\\", windows_path_slashes(hipercow_root$rel)),
    cluster_name = config$cluster)
}

path_bootstrap <- function(config) {
  platform <- config$platform
  use_development <- getOption("hipercow.development", FALSE)
  base <- if (use_development) "bootstrap-dev" else "bootstrap"
  version <- version_string(config$r_version, ".")
  if (platform == "windows") {
    ## TODO: update to I:/bootstrap(-dev)?/(windows|linux)/<version>
    ## - Bit of a pain to migrate as I:/bootstrap is active.
    ## - Can we tolerate I:/bootstrap, I:/bootstrap-dev  and
    ##   /wpia-hn/Hipercow/bootstrap-linux /wpia-hn/Hipercow/bootstrap-dev-linux
    sprintf("I:/%s/%s", base, version)
  } else {
    sprintf("/wpia-hn/Hipercow/%s-linux/%s", base, version)
  }
}
