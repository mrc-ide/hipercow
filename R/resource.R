##' Specify what resources a task requires to run.  This creates a
##' validated list of resources that can be passed in as the
##' `resources` argument to [task_create_expr] or other task
##' creation functions.
##'
##' # Windows cluster (`wpia-hn`)
##'
##' * Cores at present must be between 1 and 32
##' * Memory per node (or per task) can be 512Gb at most.
##' * The available queues are `AllNodes` and `Testing` - the latter
##'   has a maximum runtime of 30 minutes; jobs will be aborted if they
##'   exceed this. 
##' * The node names are between `wpia-001` and `wpia-089`, excluding
##'   41, 42, 49 and 50.
##'
##' # Linux cluster (hermod)
##'
##' Coming Soon.
##'
##' @title Hipercow Resources
##'
##' @param cores The number of cores your task requires. This is 1 by
##'   default. Setting to `Inf` will request any single node single node,
##'   however many cores that node has has.
##'
##' @param exclusive Set this to `TRUE` to ensure no other tasks will be
##'   concurrently run on the node while it runs your task. This is done
##'   implicitly if `cores` is `Inf`. This might be useful for a single
##'   core task that uses a very large amount of memory, or for multiple
##'   tasks that for some reason cannot co-exist on the same node.
##'
##' @param max_runtime Set this to specify a time limit for running your job.
##'   Acceptable formats are either an integer number of minutes, or
##'   strings specifying any combination of hours (h), days (d) and
##'   minutes (m). Example valid values: `60`, `"1h30m`", `"5h"`,
##'   or `"40d"`.
##'
##' @param hold_until Specify your task should wait in the queue until
##'   a certain time, or for a certain period. For the former, this can
##'   be a [POSIXt] (i.e., a date and time in the future), a [Date] (midnight
##'   on a day in the future), the special strings "tonight" (7pm),
##'   "midnight", or "weekend" (midnight Saturday morning). To delay for
##'   a period, you can specify an integer number of minutes, or
##'   strings specifying any combination of hours (h), days (d) and
##'   minutes (m). Example valid values: `60`, `"1h30m`", `"5h"`,
##'   or `"3d"`.
##'
##' @param memory_per_node Specify your task can only run on a node
##'   with at least the specified memory. This is an integer assumed to
##'   be gigabytes, or a string in gigabytes or terabytes written as
##'   `"64G"` or `"1T"` for example.
##'
##' @param memory_per_process If you can provide an estimate of how
##'   much RAM your task requires, then the cluster can ensure the
##'   total memory required by running multiple tasks on a node
##'   does not exceed how much memory the node has. Specify this as
##'   an integer number of gigabytes, or characters such as `"10G"`
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
##'   for more information, and the queues available on each cluster.
##'
##' @return If the function succeeds, it returns a `hipercow_resources` list
##'   of parameters which is syntactically valid, although not yet
##'   validated against a particular driver to see if the resources can be
##'   satisfied. If the function fails, it will return information
##'   about why the arguments could not be validated. Do not modify the
##'   return value.
##'
##' @export
##' @examples
##' # The default set of resources
##' hipercow_resources()
##'
##' # A more complex case:
##' hipercow_resources(
##'   cores = 32,
##'   exclusive = TRUE,
##'   priority = "low")
##'
##' # (remember that in order to change resources you would pass the
##' # return value here into the "resources" argument of
##' # task_create_expr() or similar)
hipercow_resources <- function(cores = 1L,
                               exclusive = FALSE,
                               max_runtime = NULL,
                               hold_until = NULL,
                               memory_per_node = NULL,
                               memory_per_process = NULL,
                               requested_nodes = NULL,
                               priority = NULL,
                               queue = NULL) {
  call <- rlang::current_env()
  res <- list(
    cores = validate_cores(cores, call),
    exclusive = validate_exclusive(exclusive),
    max_runtime = validate_max_runtime(max_runtime, call),
    hold_until = validate_hold_until(hold_until, call),
    memory_per_node = validate_memory(memory_per_node, "memory_per_node", call),
    memory_per_process = validate_memory(memory_per_process,
                                         "memory_per_process", call),
    requested_nodes = validate_nodes(requested_nodes, call),
    priority = validate_priority(priority, call),
    queue = validate_queue(queue, call)
  )

  class(res) <- "hipercow_resources"
  res
}

validate_cores <- function(cores, call = NULL) {
  assert_scalar(cores, call = call)
  if (identical(cores, Inf)) {
    return(Inf)
  }
  if (is.na(cores) || !rlang::is_integerish(cores) || cores <= 0) {
    cli::cli_abort(
      c("Invalid value for 'cores': {cores}",
        i = "Number of cores must be a positive integer, or 'Inf'"),
      call = call, arg = "cores")
  }
  as.integer(cores)
}

validate_exclusive <- function(exclusive) {
  assert_scalar_logical(exclusive)
  exclusive
}

validate_max_runtime <- function(max_runtime, call = NULL) {
  if (is.null(max_runtime)) {
    return(NULL)
  }
  assert_scalar(max_runtime, call = call)
  duration_to_minutes(max_runtime, "max_runtime", call)

}

## Special hold until values that will be computed only at the point
## of submission
hold_until_special <- c("tonight", "midnight", "weekend")


validate_hold_until <- function(hold_until, call = NULL) {
  if (is.null(hold_until)) {
    return(NULL)
  }

  assert_scalar(hold_until)
  if (inherits(hold_until, "POSIXt") || inherits(hold_until, "Date")) {
    computed <- as.POSIXlt(hold_until)
    if (computed < Sys.time()) {
      cli::cli_abort(c("Invalid value for 'hold_until': {hold_until}",
                       x = "{hold_until} is in the past"),
                     arg = "hold_until", call = call)
    }
  } else if (hold_until %in% hold_until_special) {
    computed <- hold_until
  } else {
    computed <- duration_to_minutes(hold_until, "hold_until")
  }
  computed
}


validate_memory <- function(value, name, call = NULL) {
  if (is.null(value)) {
    return(NULL)
  }

  assert_scalar(value, name = name, call = call)
  if (is.numeric(value)) {
    if (!rlang::is_integerish(value) || is.na(value) || value < 0) {
      cli::cli_abort(
        c("Invalid value for '{name}': {value}",
          i = "Amount of memory must be a positive integer"),
        call = call, arg = name)
    }
    computed <- value
  } else if (is.character(value)) {
    value <- trimws(value)
    re <- "^\\s*([0-9]+)([GT])?$"
    if (!grepl(re, value)) {
      cli::cli_abort(
        c("Invalid string representation of memory for '{name}': {value}",
          i = paste("Examples: '4' (as an integer, assumed gigabytes),",
                    "or '8G' or '1T'")),
        call = call, arg = name)
    }
    computed <- as.integer(sub(re, "\\1", value))
    if (sub(re, "\\2", value) == "T") {
      computed <- computed * 1000
    }
  } else {
    cli::cli_abort(
      c("Invalid value for '{name}': {value}",
        i = "Expected an integer or a string representing a size"),
      call = call, arg = name)
  }

  if (computed == 0) {
    cli::cli_abort(
      c("Invalid value for '{name}': {value}",
        i = "We need some memory to run your tasks!"),
      call = call, arg = name)
  }

  computed
}

validate_nodes <- function(nodes, call = NULL) {
  if (is.null(nodes)) {
    return(NULL)
  }
  assert_character(nodes, call = call)
  unique(trimws(nodes))
}

validate_priority <- function(priority, call = call) {
  if (is.null(priority)) {
    return(NULL)
  }

  assert_scalar_character(priority)
  if (priority == "high") {
    utils::browseURL("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
  }
  if (!priority %in% c("low", "normal")) {
    cli::cli_abort(c(
      "Could not understand priority '{priority}'",
      i = "Priority can only be 'low' or 'normal'"))
  }
  priority
}

validate_queue <- function(queue, call = call) {
  if (is.null(queue)) {
    return(NULL)
  }
  assert_scalar_character(queue, call = call)
  trimws(queue)
}


##' Query a driver to find information about the cluster, and then validate
##' a [hipercow_resources] list against that driver to see if the resources
##' requested could be satisfied.
##'
##' @title Validate a `hipercow_resources` list for a driver.
##'
##' @param resources A [hipercow_resources] list returned by
##'   [hipercow_resources], or `NULL`
##'
##' @return TRUE if the resources are compatible with this driver.
##'
##' @inheritParams task_submit
##' @export
##' @examples
##' cleanup <- hipercow_example_helper()
##' hipercow_resources_validate(hipercow_resources(cores = 1))
##'
##' # This example does not allow more than one core
##' tryCatch(
##'   hipercow_resources_validate(hipercow_resources(cores = 32)),
##'   error = identity)
##'
##' cleanup()
hipercow_resources_validate <- function(resources, driver = NULL, root = NULL) {
  root <- hipercow_root(root)
  driver <- hipercow_driver_select(driver, FALSE, root, rlang::current_env())
  resources_validate(resources, driver, root)
}


resources_validate <- function(resources, driver, root) {
  already_valid <-
    (identical(attr(resources, "validated", exact = TRUE), driver)) &&
    !is.null(driver) # identical would be true when driver not given otherwise
  if (already_valid) {
    return(resources)
  }

  given_resources <- !is.null(resources)
  if (given_resources) {
    assert_is(resources, "hipercow_resources")
  } else {
    resources <- hipercow_resources()
  }

  if (is.null(driver)) {
    if (given_resources) {
      cli::cli_alert_warning(
        "Resources given but no driver selected, so not validating")
    }
    return(resources)
  }
  cluster_resources <- cluster_info(driver, root)$resources

  if (is.null(resources$queue)) {
    resources$queue <- cluster_resources$default_queue
  }

  validate_cluster_cores(resources$cores, cluster_resources$max_cores)

  validate_cluster_memory(
    resources$memory_per_node, cluster_resources$max_ram, "node")
  validate_cluster_memory(
    resources$memory_per_process, cluster_resources$max_ram, "process")

  validate_cluster_queue(
    resources$queue, cluster_resources$queues)

  validate_cluster_requested_nodes(
    resources$requested_nodes, cluster_resources$nodes)

  attr(resources, "validated") <- driver
  resources
}


validate_cluster_cores <- function(cores, max_cores) {
  if (cores != Inf && cores > max_cores) {
    cli::cli_abort(c(
      "{cores} is too many cores for this cluster.",
      i = "The largest node has {max_cores} cores."))
  }
}


validate_cluster_memory <- function(mem, max_mem, type) {
  if (!is.null(mem) && mem > max_mem) {
    cli::cli_abort(c(
      "{mem}Gb per {type} is too large for this cluster.",
      i = "The largest node has {max_mem}Gb."))
  }
}


validate_cluster_queue <- function(queue, queues) {
  if (!is.null(queue)) {
    if (!tolower(queue) %in% tolower(queues)) {
      cli::cli_abort(c(
        "The {queue} queue is not available on this cluster.",
        i = "Available: {queues}."))
    }
  }
}


validate_cluster_requested_nodes <- function(nodes, cluster_nodes) {
  if (!is.null(nodes)) {
    nodes <- tolower(nodes)
    missing <- nodes[!nodes %in% tolower(cluster_nodes)]
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "The {missing} nodes{?s} {?is/are} does not exist on this cluster.",
        i = "Available: {cluster_nodes}."))
    }
  }
}


##' @export
print.hipercow_resources <- function(x, ...) {
  print_simple_s3(x, "hipercow resource control (hipercow_resources)")
}
