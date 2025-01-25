##' Hello world in hipercow. This function sends a tiny test task
##' through the whole system to confirm that everything is configured
##' correctly.
##'
##' @title Hello world
##'
##' @param driver The driver to use to send the test task.  This can
##'   be omitted where you have exactly one driver, but we error if
##'   not given when you have more than one driver, or if you have not
##'   configured any drivers.
##'
##' @param platform If the driver can target nodes running different
##'   operating systems, then we can run hello world targeting that
##'   platform. The default is `windows`, and `linux` can also be used
##'   to test linux nodes connected to our MS-HPC cluster with the
##'   hipercow.windows driver.
##'
##' @inheritParams task_log_watch
##'
##' @return The string "Moo", direct from your cluster.
##'
##' @export
##' @examples
##'
##' cleanup <- hipercow_example_helper()
##' hipercow_hello()
##'
##' cleanup()
hipercow_hello <- function(progress = NULL, timeout = NULL, driver = NULL,
                           platform = "windows") {
  root <- hipercow_root(NULL)
  driver <- hipercow_driver_select(driver, TRUE, root, rlang::current_env())

  dat <- hipercow_driver_prepare(driver, root, environment())
  resources <- dat$driver$check_hello(dat$config, root$path$root, platform)

  moo <- read_lines(hipercow_file("comms/moo"))
  id <- task_create_expr({
    message(moo)
    "Moo"
  }, driver = driver, resources = resources, root = root)

  ok <- task_log_watch(id, timeout = timeout, progress = progress, root = root)
  result <- task_result(id, root = root)
  if (ok) {
    hipercow_speak(2)
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
