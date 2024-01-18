##' Set parallel options.  Having requested more than one core using
##' [hipercow_resources], here we specify the way that those cores
##' might be used in parallel. 
##'
##' @title Specify parallel use of cores
##'
##' @param method The parallel method that hipercow will prepare.
##' Four options are available: `future` will allocate your
##' reserved cores to a (furrr)[https://furrr.futureverse.org/]
##' cluster; `parallel` will make you a cluster using the 
##' (parallel)[https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf]
##' package; `doParallel` will initialise using the 
##' (doParallel)[https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf] 
##' package, and finally `NULL`, the default, will do nothing,
##' and leave you to configure any parallelism yourself.
##'
##' @inheritParams task_submit
##'
##' @return A list containing your parallel configuration.
##'
##' @export
hipercow_parallel <- function(method = NULL, ..., driver = NULL,
                               root = NULL) {
  
  if (!is.null(method) && (!method %in% c("future", "parallel", "doParallel"))) {
    cli::cli_abort(c(
      "Parallel method {method} unknown.",
      i = "Use either future, parallel, doParallel, or leave as NULL"))
  }
  
  res <- list(method = method)
  class(res) <- "hipercow_parallel"
  res
}

