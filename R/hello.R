##' Hello world in hipercow. This function sends a tiny test task
##' through the whole system to confirm that everything is configured
##' correctly.
##'
##' @title Hello world
##'
##' @inheritParams task_log_watch
##'
##' @return Unclear
##'
##' @export
##' @examples
##'
##' cleanup <- hipercow_example_helper()
##' hipercow_hello()
##'
##' cleanup()
hipercow_hello <- function(progress = NULL, timeout = NULL, root = NULL) {
  root <- hipercow_root(root)

  if (identical(names(root$config), "windows") && !windows_check()) {
    cli::cli_alert_danger("Can't send test task")
    return(invisible(NULL))
  }

  ## TODO: here we also want to modify the template to use the
  ## fast-but-short queue; we can't do that until mrc-4801 is done
  ## though.
  id <- task_create_expr(hipercow:::hipercow_handshake(),
                         submit = TRUE, root = root)
  ok <- task_log_watch(id, timeout = timeout, progress = progress, root = root)
  result <- task_result(id, root = root)
  if (ok) {
    hipercow_speak(system.file("comms/moo2.wav", package = "hipercow"))
    cli::cli_alert_success("Successfully ran test task '{id}'")
  } else {
    status <- task_status(id, root = root)
    cli::cli_alert_danger("Failed to run test task '{id}'")
    cli::cli_alert_danger("Task status is '{status}'")
    if (inherits(result, "condition")) {
      cli::cli_alert_info("Original error: {conditionMessage(result)}")
    }

    ## TODO: perhaps we should also get the outer logs here too? It
    ## might be sensible to put this into some sort of
    ## hipercow_diagnose_failure() function though, which collects up
    ## all the relevant bits (including things like the configuration)
  }

  invisible(result)
}
