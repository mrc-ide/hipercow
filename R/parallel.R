##' Set parallel options.  Having requested more than one core using
##' [hipercow_resources], here we specify the way that those cores
##' might be used in parallel.
##'
##' @title Specify parallel use of cores
##'
##' @param method The parallel method that hipercow will prepare.
##' Four options are available: `future` will allocate your
##' reserved cores to a [furrr](https://furrr.futureverse.org/)
##' cluster; `parallel` will make you a cluster using the
##' [parallel](https://www.rdocumentation.org/packages/parallel/)
##' package; `doParallel` will initialise using the
##' [doParallel](https://cran.r-project.org/web/packages/doParallel/index.html)
##' package, and finally `NULL`, the default, will do nothing,
##' and leave you to configure any parallelism yourself. The packages you
##' use should be provisioned in the usual way with [hipercow_provision].
##'
##'
##' @return A list containing your parallel configuration.
##'
##' @export
hipercow_parallel <- function(method = NULL) {

  if (!is.null(method) &&
      (!method %in% c("future", "parallel", "doParallel"))) {
    cli::cli_abort(c(
      "Parallel method {method} unknown.",
      i = "Use either future, parallel, doParallel, or leave as NULL"))
  }

  res <- list(method = method)
  class(res) <- "hipercow_parallel"
  res
}



##' Lookup number of cores allocated to the task
##'
##' @title Get number of cores
##'
##' @param envir The environment in which to look for environment variables
##'
##' @return The number of cores a cluster has allocated to your task. This will
##'   be less than or equal to the number of cores on the cluster node
##'   running your task.
##'
##' @export
hipercow_get_cores <- function(envir = rlang::current_env()) {
  cores_env_var <- Sys.getenv("HIPERCOW_CORES_VARIABLE_NAME", envir)
  Sys.getenv(cores_env_var, envir)
}

parallel_setup <- function(method, envir) {
  if (is.null(method)) {
    return()
  }
  cores <- hipercow_get_cores(envir)
  code <- switch(
    method,
    future = sprintf("future::plan(future::multisession, %s", cores),
    parallel = sprintf("parallel::makeCluster(%s)", cores),
    doParallel = sprintf(
      "doParallel::registerDoParallel(doParallel::makeCluster(%s)", cores),
    cli::cli_abort("Unknown method {method} for setting up parallel execution")
  )
  code
}
