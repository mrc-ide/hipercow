##' Set parallel options.  Having requested more than one core using
##' [hipercow_resources], here hipercow can start up a local cluster
##' on the node you are running on, using either the `future` or
##' `parallel` package.
##'
##' For example, using `future`:
##'
##' ```
##' resources <- hipercow_resources(cores = 4)
##' id <- task_create_expr(
##'   furrr::future_map(1:4, ~Sys.getpid()),
##'   parallel = hipercow_parallel("future"),
##'   resources = resources)
##' ```
##'
##' where `furrr` must be provisioned using [hipercow_provision].
##'
##' For use with `parallel`:
##'
##' ```
##' id <- task_create_expr(
##'   parallel::clusterApply(NULL, 1:4, function(x) Sys.getpid()),
##'   parallel = hipercow_parallel("parallel"),
##'   resources = resources)
##' ```
##'
##' @title Specify parallel use of cores
##'
##' @param method The parallel method that hipercow will prepare.
##' Three options are available: the `future` package, the `parallel`
##' page, or `NULL`, the default, will do nothing. See the details for
##' examples.
##'
##'
##' @return A list containing your parallel configuration.
##'
##' @export
hipercow_parallel <- function(method = NULL) {
  if (!is.null(method) &&
      (!method %in% c("future", "parallel"))) {
    cli::cli_abort(c(
      "Parallel method {method} unknown.",
      i = 'Use either "future", "parallel", or leave as NULL'))
  }

  res <- list(method = method)
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
##' the given number of cores. This is used to ensure that parallel workers you
##' launch using [hipercow_parallel] will have the correct resources, but you
##' can also call it yourself if you know specifically how many cores you want
##' to be available to code that looks up these environment variables.
##'
##' @title Set various environment variables that report the number of cores
##' available for execution.
##'
##' @param cores Number of cores to be used.
##'
##' @export
hipercow_parallel_set_cores <- function(cores) {
  prev <- hipercow_parallel_get_cores()
  if (!is.na(prev) && (!is.na(cores)) && (cores > prev)) {
    cli::cli_alert_info(
    "Note: increasing cores alone is unlikely to improve performance")
  }

  Sys.setenv(HIPERCOW_CORES = cores,
             MC_CORES = cores,
             OMP_NUM_THREADS = cores,
             OMP_THREAD_LIMIT = cores,
             R_DATATABLE_NUM_THREADS = cores)
  invisible()
}



hipercow_parallel_setup <- function(method) {
  if (is.null(method)) {
    return()
  }

  cores <- hipercow_parallel_get_cores()
  if (is.na(cores)) {
    cli::cli_abort(c(
      "Couldn't find HIPERCOW_CORES environment variable.",
      i = "This function should only get called on a cluster node.",
      i = "Please get in touch if you see this error."))
  }

  switch(
    method,
    future = hipercow_parallel_setup_future(cores),
    parallel = hipercow_parallel_setup_parallel(cores),
    cli::cli_abort("Unknown method {method} for setting up parallel execution")
  )
  invisible()
}

hipercow_parallel_setup_future <- function(cores) {
  cli::cli_alert_info("Creating a future cluster with {cores} core{?s}")

  # rscript_libs is already set by default to .libPaths() in future::plan
  # but we also want to call set cores...

  future::plan(future::multisession, workers = cores,
               rscript_startup = "hipercow::hipercow_parallel_set_cores(1)")
  cli::cli_alert_success("Cluster ready to use")
}

hipercow_parallel_setup_parallel <- function(cores) {
  # total number of cores passed in
  cli::cli_alert_info("Creating a parallel cluster with {cores} core{?s}")
  cl <- parallel::makeCluster(spec = cores)
  parallel::setDefaultCluster(cl)
  # set lib paths to my lib paths
  parallel::clusterCall(cl, .libPaths, .libPaths())
  parallel::clusterCall(cl, hipercow::hipercow_parallel_set_cores, 1)
  # may need some tweaking to find the function.
  # later on we'll also load some packages, source some files
  cli::cli_alert_success("Cluster ready to use")
}


##' @export
print.hipercow_parallel <- function(x, ...) {
  print_simple_s3(x, "hipercow parallel control (hipercow_parallel)")
}
