new_linux_config <- function(manager) {
  if (manager != "pbs") {
    cli::cli_abort("Only pbs cluster managers are supported")
  }
  list(manager = manager)
}
