##' Specify what resources a task requires to run.
##'
##' # Windows cluster (`wpia-hn`)
##'
##' * Cores at present must be between 1 and 32
##' * Memory per node (or per task) can be 512Gb at most.
##' * The available queues are `AllNodes` and `Training`
##' * The node names are between `wpia-001` and `wpia-070`, excluding
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
##' @return If the function succeeds, it returns a `hipercow_resource` list
##'   of parameters which is syntactically valid, although not yet
##'   validated against a particular driver to see if the resources can be
##'   satisfied. If the function fails, it will return information
##'   about why the arguments could not be validated.
##'
##' @export
##' @examples
##' hipercow_resources()
##'
##' # A more complex case:
##' r <- hipercow_resources(
##'   cores = 32,
##'   exclusive = TRUE,
##'   priority = "low")
hipercow_resources <- function(cores = 1L,
                               exclusive = FALSE,
                               max_runtime = NULL,
                               hold_until = NULL,
                               memory_per_node = NULL,
                               memory_per_process = NULL,
                               requested_nodes = NULL,
                               priority = NULL,
                               queue = NULL) {
  res <- list(
    cores = validate_cores(cores),
    exclusive = validate_exclusive(exclusive),
    max_runtime = validate_max_runtime(max_runtime),
    hold_until = validate_hold_until(hold_until),
    memory_per_node = validate_memory(memory_per_node),
    memory_per_process = validate_memory(memory_per_process),
    requested_nodes = validate_nodes(requested_nodes),
    priority = validate_priority(priority),
    queue = validate_queue(queue)
  )

  class(res) <- "hipercow_resource"
  res
}

validate_cores <- function(cores) {
  assert_scalar(cores)
  if (!identical(cores, Inf)) {
    if (is.na(cores) || !rlang::is_integerish(cores) || cores <= 0) {
      cli::cli_abort(c("Could not understand number of cores '{cores}'",
        i = "Number of cores must be a positive integer, or 'Inf'"))
    }
    cores2 <- as.integer(cores)
  } else {
    cores2 <- Inf
  }
  list(original = cores, computed = cores2)
}

validate_exclusive <- function(exclusive) {
  assert_scalar_logical(exclusive)
  list(original = exclusive, computed = exclusive)
}

validate_max_runtime <- function(max_runtime) {
  if (is.null(max_runtime)) {
    return(list(original = NULL, computed = NULL))
  }

  assert_scalar(max_runtime)
  period <- tolower(as.character(max_runtime))
  minutes <- duration_to_minutes(period, "max_runtime")

  if (minutes == 0) {
    cli::cli_abort(c(
      "{max_runtime} evaluates to zero minutes.",
      i = "max_runtime must be positive."))
  }

  list(original = max_runtime, computed = minutes)

}

validate_hold_until <- function(hold_until) {
  if (is.null(hold_until)) {
    return(list(original = NULL, computed = NULL))
  }

  assert_scalar(hold_until)

  if (inherits(hold_until, "POSIXt")) {
    if (hold_until < Sys.time()) {
      cli::cli_abort("hold_until time {hold_until} is in the past.")
    }
    return(list(original = hold_until, computed = hold_until))
  }

  if (inherits(hold_until, "Date")) {
    if (hold_until <= Sys.Date()) {
      cli::cli_abort("hold_until date {hold_until} is in the past.")
    }
    return(list(original = hold_until, computed = as.POSIXlt(hold_until)))
  }

  if (hold_until %in% c("tonight", "midnight")) {
    return(list(original = hold_until, computed = hold_until))
  }

  # Remaining case is similar to max_runtime

  period <- tolower(as.character(hold_until))
  minutes <- duration_to_minutes(period, "hold_until")

  if (minutes == 0) {
    cli::cli_abort(c(
      "{hold_until} duration evaluates to zero minutes.",
      i = "hold_until, if specified as a duration, must be positive."))
  }

  # NB - will also have to process "tonight" or "midnight"
  # into real times when we submit a task

  list(original = hold_until, computed = minutes)
}

validate_memory <- function(mem) {
  if (is.null(mem)) {
    return(list(original = NULL, computed = NULL))
  }

  assert_scalar(mem)
  orig <- mem

  # If actual integer is specified, or a string containing
  # just an integer, then assume Gigabytes

  if (rlang::is_integerish(mem) || grepl("^[0-9]+$", mem)) {
    mem <- paste0(mem, "G")
  }

  mem <- trimws(mem)
  gb <- grepl("^(\\d+)G$", mem)
  tb <- grepl("^(\\d+)T$", mem)

  if (!gb && !tb) {
    cli::cli_abort(c(
      "Could not interpret memory format from '{mem}'.",
      i = "Examples: '4' (as an integer, assumed gigabytes), or '8G' or '1T',"))
  }

  num <- as.integer(substring(mem, 1, nchar(mem) - 1))
  num <- num * (gb + (1000 * tb))
  list(original = orig, computed = num)
}

validate_nodes <- function(nodes) {
  if (is.null(nodes)) {
    return(list(original = NULL, computed = NULL))
  }

  assert_character(nodes)
  list(original = nodes, computed = unique(trimws(nodes)))
}

validate_priority <- function(priority) {
  if (is.null(priority)) {
    return(list(original = NULL, computed = NULL))
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
  list(original = priority, computed = priority)
}

validate_queue <- function(queue) {
  if (is.null(queue)) {
    return(list(original = NULL, computed = NULL))
  }

  assert_scalar_character(queue)
  list(original = queue, computed = trimws(queue))
}

hipercow_cluster_info <- function(driver = NULL, root = NULL) {
  root <- hipercow_root(root)
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$cluster_info(dat$config, root$path$root)
}


##' Query a driver to find information about the cluster, and then validate
##' a `hipercow_resource` list against that driver to see if the resources
##' requested could be satisfied.
##'
##' @title Validate a `hipercow_resource` for a driver.
##'
##' @param resources A `hipercow_resource` list returned by
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

  given_resources <- !is.null(resources)
  if (given_resources) {
    assert_is(resources, "hipercow_resource")
  } else {
    resources <- hipercow_resources()
  }

  driver <- hipercow_driver_select(driver, root, rlang::current_env())
  if (is.null(driver)) {
    if (given_resources) {
      cli::cli_alert_warning(
        "Resources given but no driver selected, so not validating")
    }
    return(resources)
  }

  cluster_info <- hipercow_cluster_info(driver, root)

  if (is.null(resources$queue$computed)) {
    resources$queue$computed <- cluster_info$default_queue
  }

  validate_cluster_cores(resources$cores$computed, cluster_info$max_cores)

  validate_cluster_memory(
    resources$cores$memory_per_node$computed, cluster_info, "node")

  validate_cluster_memory(
    resources$cores$memory_per_process$computed, cluster_info, "process")

  validate_cluster_queue(
    resources$queue$computed, cluster_info$queues)

  validate_cluster_requested_nodes(
    resources$requested_nodes$computed, cluster_info$nodes)

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
  if (!is.null(mem)) {
    if (mem > max_mem) {
      cli::cli_abort(c(
        "{mem}Gb per {type} is too large for this cluster.",
        i = "The largest node has {$max_mem}Gb."))
    }
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
