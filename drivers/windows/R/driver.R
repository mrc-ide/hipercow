hermod_driver_windows <- function() {
  hermod::hermod_driver(
    configure = windows_configure,
    submit = windows_submit)
}


windows_submit <- function(id, config, path_root) {
  write_batch_task_run(id, config, path_root)
}
