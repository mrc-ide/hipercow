## windows-specific resources code, called from hipercow.
windows_task_resources <- function(res) {
  stopifnot("hipercow_resource" %in% class(res))
  max_cores <- 32
  max_mem <- 512

  if (res$cores > max_cores) {
    cli::cli_abort(c(
      "{resources$cores} is too many cores.",
      i = "The largest node has {max_cores} cores."))
  }


  if (!is.null(res$memory_per_node)) {
    mem_per_node <- interpret_memory(res$memory_per_node)

    if ((!is.null(mem_per_node)) && (mem_per_node > max_mem)) {
      cli::cli_abort(c(
        "{resources$memory_per_node}Gb is too much memory per node.",
        i = "The largest node has {max_mem}Gb of RAM."))
    }
  }

  if (!is.null(res$memory_per_proc)) {

    mem_per_proc <- interpret_memory(res$memory_per_process)

    if ((!is.null(mem_per_proc)) && (mem_per_proc > max_mem)) {
      cli::cli_abort(c(
        "{resources$memory_per_node}Gb is too much memory per process.",
        i = "The largest node has {max_mem}Gb of RAM."))
    }
  }

  if (is.null(res$queue)) {
    res$queue <- "AllNodes"
  } else if (!res$queue %in% c("AllNodes", "Training")) {
    cli::cli_abort(c(
      "Queue {queue} is unknown.",
      i = "Please use AllNodes, or Training"))
  }

  res
}

interpret_memory <- function(mem) {
  if (rlang::is_integerish(mem)) {
    return(mem)
  }
  gb <- grepl("^(\\d+)Gb$", mem)
  tb <- grepl("^(\\d+)Tb$", mem)
  num <- as.integer(substring(mem, 1, nchar(mem) - 2))
  if (gb) {
    return(num)
  }
  num * 1000
}