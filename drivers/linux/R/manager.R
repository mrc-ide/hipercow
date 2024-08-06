do_linux_submit <- function(path_batch, config, path_root) {
  scheduler_id <- switch(
    config$manager,
    pbs = do_linux_submit_pbs(path_batch, config, path_root),
    cli::cli_abort("Submission from unknown manager '{config$manager}'"))
}


do_linux_status <- function(scheduler_id, config) {
  switch(config$manager,
         pbs = do_linux_status_pbs(scheduler_id, config),
         cli::cli_abort("Status from unknown manager '{config$manager}'"))
}


do_linux_cancel <- function(scheduler_id, config) {
  switch(config$manager,
         pbs = do_linux_cancel_pbs(scheduler_id, config),
         cli::cli_abort("Submission from unknown manager '{config$manager}'"))
}


do_linux_submit_pbs <- function(path_batch, id, config, path_root) {
  res <- withr::with_dir(path_root, system3("qsub", path_batch, stdout = TRUE))
  if (!res$success) {
    output <- paste(res$output, collapse = "\n")
    cli::cli_abort(
      "Job submission failed with code {res$code} and error: {output}")
  }
  sub(".pbs$", "", res$output)
}


do_linux_status_pbs <- function(scheduler_id, config) {
  ## TODO: we don't really know what this returns yet, and how to
  ## interpret it.
  system3("qstat", scheduler_id)
}


do_linux_cancel_pbs <- function(scheduler_id, config) {
  ## TODO: we don't really know what this returns yet, and how to
  ## interpret it.
  system3("qdel", scheduler_id)
}
