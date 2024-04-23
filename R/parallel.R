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
##' `hipercow_resources`. Each process here would be use a single core.
##'
##' You can also call `hipercow_parallel` with `cores_per_process`,
##' to make hipercow launch as many processes as it can with
##' each process having the number of cores you request, with
##' the total cores being at most what you requested with
##' `hipercow_resources`.
##'
##' For example, you could request 32 cores with `hipercow_resources`,
##' and then calll `hipercow_parallel` with `cores_per_process = 4`,
##' and hipercow will create a local cluster with 8 processes, each
##' of which reporting `4` cores if that process calls
##' `hipercow_parallel_get_cores`.
##'
##' If you did the same with `cores_per_process = 5`, hipercow would
##' create 6 local processes, each reporting `5` cores, and
##' two cores would be effectively unallocated.
##'
##' Here are some brief examples; see `vignette("parallel")` for more
##' details. In each example, we are looking up the process id (to
##' show that different processes are being launched), and asking each
##' process how many cores it should be using.
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
##' @param environment The name of the environment to load into your
##'   parallel workers.  The default is to use the environment that
##'   you submit your task with (which defaults to `default`), which
##'   means that each worker gets the same environment as your main
##'   process.  This is often what you want, but can mean that you
##'   load too much into each worker and incur a speed or memory
##'   cost.  In that case you may want to create a new environment
##'   ([hipercow_environment_create]) that contains fewer packages or
##'   sources fewer functions and specify that here. If you want to
##'   suppress loading any packages into the workers you can use the
##'   `empty` environment, which always exists.
##'
##' @return A list containing your parallel configuration.
##'
##' @export
hipercow_parallel <- function(method = NULL, cores_per_process = 1L,
                              environment = NULL) {
  if (is.na(cores_per_process) || cores_per_process <= 0 ||
      !rlang::is_integerish(cores_per_process)) {
    cli::cli_abort(paste("'cores_per_process' must be a positive integer,",
                         "not {cores_per_process}"))
  }

  if (!is.null(method) && !(method %in% c("future", "parallel"))) {
    cli::cli_abort(c(
      "Parallel method '{method}' unknown",
      i = 'Use either "future", "parallel", or leave as NULL'))
  }

  if (!is.null(environment)) {
    assert_scalar_character(environment)
  }

  res <- list(method = method,
              cores_per_process = cores_per_process,
              environment = environment)
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
    cli::cli_abort("'cores' must be a positive integer, not {cores}")
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


##' Load an environment into a parallel worker.  This is a helper
##' function designed for use from parallel backends, and is exported
##' mostly because it makes it easier for us to work with.  Users
##' should never need to call this function directly.
##'
##'
##' @title Load an environment in a parallel context
##'
##' @param envir The name of the environment
##'
##' @return Nothing, called for side effects only
##'
##' @export
##' @keywords internal
hipercow_parallel_load_environment <- function(name, envir = .GlobalEnv) {
  root <- hipercow_root(getwd())
  environment_apply(name, envir, root, rlang::current_env())
}



hipercow_parallel_setup <- function(parallel) {
  if (is.null(parallel$method)) {
    return()
  }

  all_cores <- hipercow_parallel_get_cores()
  if (is.na(all_cores)) {
    cli::cli_abort(c(
      "Couldn't find HIPERCOW_CORES environment variable.",
      i = "This function should only get called on a cluster node.",
      i = "Please get in touch if you see this error."))
  }

  leftover <- all_cores %% parallel$cores_per_process
  if (leftover > 0) {
    cli::cli_alert_info(paste(
      "Running {parallel$cores_per_process} core{?s} per process leaves",
      "{leftover} unallocated core{?s} on a {all_cores}-core task"))
  }

  processes <- all_cores %/% parallel$cores_per_process
  cores_per_process <- parallel$cores_per_process
  environment <- parallel$environment

  switch(
    parallel$method,
    future = hipercow_parallel_setup_future(processes, cores_per_process,
                                            environment),
    parallel = hipercow_parallel_setup_parallel(processes, cores_per_process,
                                                environment)
  )
  invisible()
}

hipercow_parallel_setup_future <- function(processes, cores_per_process,
                                           environment) {
  cli::cli_alert_info(
    paste0("Creating a future cluster with {processes} process{?es}, ",
           "each with {cores_per_process} core{?s}"))

  # rscript_libs is already set by default to .libPaths() in future::plan
  # but we also want to call set cores and environment.
  script <- c(
    sprintf("hipercow::hipercow_parallel_set_cores(%d)", cores_per_process),
    sprintf('hipercow::hipercow_parallel_load_environment("%s")', environment))

  future::plan(
    future::multisession,
    workers = processes,
    rscript_startup = paste0(script, "\n", collapse = ""))
  cli::cli_alert_success("Cluster ready to use")
}

hipercow_parallel_setup_parallel <- function(processes, cores_per_process,
                                             environment) {
  cli::cli_alert_info(
    paste0("Creating a parallel cluster with {processes} process{?es}, ",
           "each with {cores_per_process} core{?s}"))
  cl <- parallel::makeCluster(spec = processes)
  parallel::setDefaultCluster(cl)

  # set lib paths to my lib paths

  parallel::clusterCall(cl, .libPaths, .libPaths())
  parallel::clusterCall(cl, hipercow::hipercow_parallel_set_cores,
                        cores_per_process)
  parallel::clusterCall(cl, hipercow::hipercow_parallel_load_environment,
                        environment)

  # may need some tweaking to find the function.
  # later on we'll also load some packages, source some files

  cli::cli_alert_success("Cluster ready to use")
}


##' @export
print.hipercow_parallel <- function(x, ...) {
  print_simple_s3(x, "hipercow parallel control (hipercow_parallel)")
}

parallel_validate <- function(parallel, cores, environment, root, call = NULL) {
  if (is.null(parallel)) {
    return(NULL)
  }
  if (is.null(parallel$method)) {
    if (parallel$cores_per_process > 1) {
      cli::cli_abort(c(
        paste("You chose {parallel$cores_per_process} core{?s} per process,",
              "but no parallel method is set")))
    }
  } else {
    if (cores == 1) {
      cli::cli_abort(c(
        "You chose parallel method '{parallel$method}', with 1 core",
        i = "You need multiple cores for this - check your hipercow_resources"))
    }
    if (parallel$cores_per_process > cores) {
      cli::cli_abort(c(
        paste("You chose {parallel$cores_per_process} core{?s} per process,",
              "but requested only {cores} core{?s} in total")))
    }
  }

  if (is.null(parallel$environment)) {
    parallel$environment <- environment
  }
  ensure_environment_exists(parallel$environment, root, call)

  parallel
}
