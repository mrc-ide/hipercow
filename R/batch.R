write_batch_task_run <- function(root, config, task_id) {
  data <- template_data(root, config)
  data$hermod_task_id <- task_id
  template <- read_template("task_run")
  str <- glue_whisker(template, data)
  path <- file.path(root$path$tasks, task_id, "run.bat")
  writeLines(str, path)
}


read_template <- function(name) {
  read_lines(hermod_file(sprintf("templates/%s.bat", name)))
}


template_data <- function(root, config) {
  ## Work out both of our paths on the remote machine; the context
  ## root and the workdir
  hermod_root <- prepare_path(root$path$root, config$shares)
  workdir <- prepare_path(config$workdir %||% getwd(), config$shares)

  ## Same path, absolute, that will be used remotely
  hermod_root_abs <- windows_path(
    file.path(hermod_root$drive_remote, hermod_root$rel))

  r_version_str <- paste(unclass(config$r_version)[[1]], collapse = "_")

  network_shares_data <- list(
    drive = lapply(config$shares, "[[", "drive_remote"),
    path = lapply(config$shares, "[[", "path_remote"))
  temp_drive <- remote_drive_temp(config$shares)
  if (is.null(temp_drive)) {
    temp_drive <- available_drive(config$shares, "", "T")
    network_shares_data$drive <- c(network_shares_data$drive, temp_drive)
    network_shares_data$path <- c(network_shares_data$path,
                                  "\\\\fi--didef3.dide.ic.ac.uk\\tmp")
  }
  network_shares_create <- glue_whisker(
    "ECHO mapping {{drive}} -^> {{path}}\nnet use {{drive}} {{path}} /y",
    network_shares_data)
  network_shares_delete <- glue_whisker(
    "ECHO Removing mapping {{drive}}\nnet use {{drive}} /delete /y",
    network_shares_data)

  list(hostname = hostname(),
       date = as.character(Sys.Date()),
       hermod_version = as.character(packageVersion("hermod")),
       r_version = r_version_str,
       network_shares_create = paste(network_shares_create, collapse = "\n"),
       network_shares_delete = paste(network_shares_delete, collapse = "\n"),
       hermod_drive = workdir$drive_remote,
       hermod_workdir = paste0("\\", windows_path(workdir$rel)),
       hermod_root = hermod_root_abs,
       r_libs_user = "T:/hermod-testing",
       cluster_name = config$cluster,
       use_java = config$use_java,
       java_home = config$java_home)
}


remote_drive_temp <- function(shares) {
  for (s in shares) {
    re <- "^\\\\\\\\fi--didef3(\\.dide\\.ic\\.ac\\.uk)?\\\\tmp"
    if (grepl(re, s$path_remote, ignore.case = TRUE)) {
      return(s$drive_remote)
    }
  }
  NULL
}
