##' Set parallel options.  Having requested more than one core using
##' [hipercow_resources], here hipercow can start up a local cluster
##' on the node you are running on, using either the `future` or
##' `parallel` package.
##'
##' Here, hipercow automatically does some setup work for the
##' supported methods, to initialise a local cluster of processes
##' that can be used with `future_map` or `clusterApply`,
##' depending on your method.
##'
##' By default, hipercow initialises a cluster with the same number
##' of processes as the number of cores you requested using
##' `hipercow_resources`. You can also set `cores_per_process`,
##' to make  hipercow launch as many processes as it can with
##' each process having `cores_per_process`, and the total cores being
##' at most what you requested with `hipercow_resources`.
##'
##' For example, you could request 32 cores with `hipercow_resources`,
##' and then calll `hipercow_parallel` with `cores_per_process = 4`,
##' and hipercow will create a local cluster with 8 processes, each
##' of which reporting `4` cores if that process calls
##' `hipercow_parallel_get_cores`.
##'
##' If you did the same with `cores_per_process = 5`, hipercow would
##' create 6 local processes, each reporting `5` cores, and
##' (conceptually) two cores would be unallocated.
##'
##' Here are some brief examples; see `vignette("parallel")` for more detail.
##'
##' For using the `future` package:
##'
##' ```
##' resources <- hipercow_resources(cores = 4)
##' id <- task_create_expr(
##'   furrr::future_map(1:4,
##'     ~c(Sys.getpid(), hipercow_parallel_get_cores()),
##'   parallel = hipercow_parallel("future"),
##'   resources = resources)
##'
##' ```
##'
##' where `furrr` must be provisioned using [hipercow_provision].
##' Here is an equivalent example with `parallel`:
##'
##' ```
##' resources <- hipercow_resources(cores = 4)
##' id <- task_create_expr(
##'   parallel::clusterApply(NULL, 1:4, function(x)
##'     c(Sys.getpid(), hipercow_parallel_get_cores()),
##'   parallel = hipercow_parallel("parallel"),
##'   resources = resources)
##' ```
##'
##'
##' @title Specify parallel use of cores
##'
##' @param method The parallel method that hipercow will prepare.
##' Three options are available: the `future` package, the `parallel`
##' package, or `NULL`, the default, will do nothing. See the details for
##' examples.
##'
##' @param cores_per_process The number of cores allocated to each
##' process when launching a local cluster using one of the parallel
##' methods. By default, this will be `1`. See details.
##'
##'
##' @return A list containing your parallel configuration.
##'
##' @export
hipercow_parallel <- function(method = NULL, cores_per_process = 1L) {
  assert_scalar_integer(cores_per_process)

  if (!is.null(method) &&
      (!method %in% c("future", "parallel"))) {
    cli::cli_abort(c(
      'Parallel method "{method}" unknown.',
      i = 'Use either "future", "parallel", or leave as NULL'))
  }

  res <- list(method = method, cores_per_process = cores_per_process)
  class(res) <- "hipercow_parallel"
  res
}



##' Lookup number of cores allocated to the task
##'
##' @title Get number of cores
##'
##' @return The number of cores a cluster has allocated to your task. This will
##'   be less than or equal to the number of cores on the cluster node
##'   running your task.
##'
##' @export
hipercow_parallel_get_cores <- function() {
  as.integer(Sys.getenv("HIPERCOW_CORES", NA))
}



##' Sets the environment variables `MC_CORES`, `OMP_NUM_THREADS`,
##' `OMP_THREAD_LIMIT`, `R_DATATABLE_NUM_THREADS` and `HIPERCOW_CORES` to
##' the given number of cores. This is used to help various thread-capable
##' packages use the correct number of cores. You
##' can also call it yourself if you know specifically how many cores you want
##' to be available to code that looks up these environment variables.
##'
##' @title Set various environment variables that report the number of cores
##' available for execution.
##'
##' @param cores Number of cores to be used.
##'
##' @param envir Environment in which the variables will be set to limit their
##' lifetime. This should not need setting in general, but
##' see `withr::local_envvar` for example use.
##'
##' @export
hipercow_parallel_set_cores <- function(cores, envir = NULL) {
  if (is.na(cores) || cores <= 0 || !rlang::is_integerish(cores)) {
    cli::cli_abort("cores must be a positive integer, not {cores}")
  }
  prev <- hipercow_parallel_get_cores()
  if (!is.na(prev) && cores > prev) {
    cli::cli_alert_info(
    "Note: increasing cores alone is unlikely to improve performance")
  }
  if (is.null(envir)) {
    Sys.setenv(HIPERCOW_CORES = cores,
               MC_CORES = cores,
               OMP_NUM_THREADS = cores,
               OMP_THREAD_LIMIT = cores,
               R_DATATABLE_NUM_THREADS = cores)
  } else {
    withr::local_envvar(HIPERCOW_CORES = cores,
                        MC_CORES = cores,
                        OMP_NUM_THREADS = cores,
                        OMP_THREAD_LIMIT = cores,
                        R_DATATABLE_NUM_THREADS = cores,
                        .local_envir = envir)
  }
  invisible()
}



hipercow_parallel_setup <- function(parallel) {
  if (is.null(parallel$method)) {
    return()
  }

  processes <- hipercow_parallel_get_cores() %/% parallel$cores_per_process
  cores_per_process <- parallel$cores_per_process

  if (is.na(processes)) {
    cli::cli_abort(c(
      "Couldn't find HIPERCOW_CORES environment variable.",
      i = "This function should only get called on a cluster node.",
      i = "Please get in touch if you see this error."))
  }

  switch(
    parallel$method,
    future = hipercow_parallel_setup_future(processes, cores_per_process),
    parallel = hipercow_parallel_setup_parallel(processes, cores_per_process)
  )
  invisible()
}

hipercow_parallel_setup_future <- function(processes, cores_per_process) {
  cli::cli_alert_info(
    paste0("Creating a future cluster with {processes} process{?es}, ",
           "each with {cores_per_process} core{?s}"))

  # rscript_libs is already set by default to .libPaths() in future::plan
  # but we also want to call set cores...

  future::plan(future::multisession, workers = processes,
               rscript_startup = sprintf(
                 "hipercow::hipercow_parallel_set_cores(%s)",
                 cores_per_process))
  cli::cli_alert_success("Cluster ready to use")
}

hipercow_parallel_setup_parallel <- function(processes, cores_per_process) {
  cli::cli_alert_info(
    paste0("Creating a parallel cluster with {processes} process{?es}, ",
           "each with {cores_per_process} core{?s}"))
  cl <- parallel::makeCluster(spec = processes)
  parallel::setDefaultCluster(cl)

  # set lib paths to my lib paths

  parallel::clusterCall(cl, .libPaths, .libPaths())
  parallel::clusterCall(cl, hipercow::hipercow_parallel_set_cores,
                        cores_per_process)

  # may need some tweaking to find the function.
  # later on we'll also load some packages, source some files

  cli::cli_alert_success("Cluster ready to use")
}


##' @export
print.hipercow_parallel <- function(x, ...) {
  print_simple_s3(x, "hipercow parallel control (hipercow_parallel)")
}
