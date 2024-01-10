## windows-specific resources code, called from hipercow.
windows_task_resources <- function(res) {
  stopifnot("hipercow_resource" %in% class(res))

  validate_cores(res$cores)
  is.null(res$memory_per_node) || 
    validate_memory(interpret_memory(res$memory_per_node))
  is.null(res$memory_per_process) ||
    validate_memory(interpret_memory(res$memory_per_process), "process")

  res$queue <- res$queue %||% "AllNodes"
  validate_queue(res$queue)

  res
}

validate_cores <- function(cores, max_cores = 32) {
  if (cores > max_cores) {
    cli::cli_abort(c(
      "{cores} is too many cores.",
      i = "The largest node has {max_cores} cores."))
  }
}

validate_memory <- function(mem, scope = "node", max_mem = 512) {
  if (mem > max_mem) {
    cli::cli_abort(c(
      "{mem}Gb is too much memory per {scope}.",
      i = "The largest node has {max_mem}Gb of RAM."))
  }
  TRUE
}

validate_queue <- function(queue) {
  if (!queue %in% c("AllNodes", "Training")) {
    cli::cli_abort(c(
      "Queue {queue} is unknown.",
      i = "Please use AllNodes, or Training"))
  }
  TRUE
}

interpret_memory <- function(mem) {
  if (rlang::is_integerish(mem)) {
    return(as.integer(mem))
  }
  gb <- grepl("^(\\d+)Gb$", mem)
  tb <- grepl("^(\\d+)Tb$", mem)
  num <- as.integer(substring(mem, 1, nchar(mem) - 2))
  if (gb) {
    return(num)
  }
  num * 1000
}