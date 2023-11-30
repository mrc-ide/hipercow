submit <- function(id, path_root) {
  workdir <- getwd()
  config <- hermod::hermod_get_configuration("windows", path_root)
  path_batch <- write_batch_task_run(id, workdir, config, path_root)

  path_batch_dat <- prepare_path(path_batch, config$shares)
  path_batch_unc <- windows_path(
    file.path(path_batch_dat$path_remote, path_batch_dat$rel))

  client <- get_web_client()
  dide_id <- client$submit(path_batch_unc, id, config$template)
  dest <- file.path(path_tasks(path_root), id, DIDE_ID)
  writeLines(dide_id, dest)
}
