write_batch_task_run_windows <- function(task_id, config, path_root) {
  data <- template_data_task_run(task_id, config, path_root)
  str <- glue_whisker(read_template("task_run.bat"), data)
  path <- path_to_task_file(path_root, task_id, BATCH_RUN)
  writeLines(str, path)
  path
}


write_batch_provision_script_windows <- function(id, config, path_root) {
  data <- template_data_provision_script(id, config, path_root)
  str <- glue_whisker(read_template("provision.bat"), data)
  path_job <- file.path(path_root, "hipercow", "provision", id)
  path <- file.path(path_job, "provision.bat")
  fs::dir_create(path_job)
  writeLines(str, path)
  path
}
