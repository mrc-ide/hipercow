##' Specify what resources a task requires to run.
##' 
##' # Windows cluster
##' 
##' The windows cluster currently is all 32-core nodes, with RAM
##' mainly 512Gb, and a few 384Gb.
##' 
##' 
##' # Linux cluster
##' 
##' Coming Soon.
##'
##' @title Hipercow Task Resources
##'
##' @param cores The number of cores your task requires. This is 1 by 
##'   default. An error is reported if you request more cores than
##'   is available on the largest cluster node. Setting to `Inf` will
##'   request any entire node, however many cores it has.
##'
##' @param exclusive Set this to `TRUE` to ensure no other tasks will be
##'   concurrently run on the node while it runs your task.
##'
##' @param runtime Set this to specify a time limit for running your job.
##'   Acceptable formats are either an integer number of minutes, or 
##'   characters in the form `"m"`, `"h:m"`, or `"d:h:m"` to optionally 
##'   specify days, hours and minutes.
##'   
##' @param hold_until POSIXt, Date, the special strings "tonight",  
##'   "midnight", "weekend" (midnight Saturday morning). 
##'   
##'   Specify your task should wait in the queue,
##'   rather than start running as soon as possible. Options are, an
##'   integer number of minutes, characters in the form `"m"`, or `"h:m"` 
##'   for hours and minutes, or `"tonight"` for 7pm, or `"midnight"` for
##'   midnight.
##'   
##' @param memory_per_node Specify your task can only run on a node
##'   with at least the specified memory. This is an integer, 
##'   measured in gigabytes, or strings such as `"64Gb"` or `"1Tb"`.
##'   
##' @param memory_per_process If you can provide an estimate of how
##'   much RAM your task requires, then the cluster can ensure the
##'   total memory required by running multiple tasks on a node
##'   does not exceed how much memory the node has. Specify this as
##'   an integer number of gigabytes, or characters such as `"10Gb"`
##'   
##' @param requested_nodes If you have been in touch with us or DIDE
##'   IT, and you need to run your task on a selection of named compute
##'   nodes, then specify this here as a vector of strings for the node names.
##'   
##' @param priority If the tasks you are launching are low priority, you can
##'   allow other queuing tasks to jump over them, by setting the priority to 
##'   to `low`; otherwise, the default is `normal`. These are the only 
##'   acceptable values.
##'   
##' @param queue Specify a particular queue to submit your tasks to.
##'   This is in development as we decide over time what
##'   queues we best need for DIDE's common workflows. See the Details
##'   for more information
##'   
##' @param headnode At present, we are only supporting the new `wpia-hn`
##'   headnode, but hipercow may support others in the future.
##'
##' @return A hipercow_resource list, which can be specified when running
##'   tasks.
##'
##' @export

hipercow_task_resources <- function(cores = 1L, 
                                   exclusive = FALSE,
                                   runtime = NULL, 
                                   hold_until = NULL,
                                   memory_per_node = NULL,
                                   memory_per_process = NULL,
                                   requested_nodes = NULL,
                                   priority = NULL,
                                   queue = NULL,
                                   driver = NULL,
                                   root = NULL) {
  root <- hipercow_root(root)
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  
  validate_cores(cores)
  if (rlang::is_integerish(cores)) {
    cores <- as.integer(cores)
  }
  
  assert_scalar_logical(exclusive)
  is.null(runtime) || validate_runtime(runtime)
  is.null(hold_until) || validate_hold_until(hold_until)
  is.null(memory_per_node) || validate_memory(memory_per_node)
  is.null(memory_per_process) || validate_memory(memory_per_process)
  is.null(requested_nodes) || validate_nodes(requested_nodes)
  is.null(priority) || validate_priority(priority)
  is.null(queue) || validate_queue(queue)
  
  res <- as.list(environment())
  class(res) <- "hypercow_resource"
  
  res <- dat$driver$task_resources(res)
  invisible(res)
}
  
validate_cores <- function(cores) {
  assert_scalar(cores)
  if (cores != Inf) {
    if (!rlang::is_integerish(cores)) {
      stop("Number of cores must be an integer, or Inf.")
    }
    if (cores <= 0) {
      stop("Number of cores must be positive")
    }
  }
}

validate_runtime <- function(runtime) {
  assert_scalar(runtime)
  if (is.integer(runtime)) return()
  stop("Runtime format failed")
  #TODO
}

validate_hold_until <- function(hold_until) {
  if (class(hold_until) == "character") {
    if (hold_until %in% c("tonight", "midnight")) return()
  }
  #TODO
}

validate_memory <- function(mem) {
  assert_scalar(mem)
  #TODO
}

validate_nodes <- function(nodes) {
  assert_character(nodes)
  
}

validate_priority <- function(priority) {
  assert_scalar_character(priority)
  if (!priority %in% c("low", "normal")) {
    cli::cli_abort("Priority can only be `low` or `normal`")
  }
}

validate_queue <- function(queue) {
  assert_scalar_character(queue)
}
