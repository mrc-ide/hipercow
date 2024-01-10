## windows-specific resources code, called from hipercow.
windows_validate_resources <- function(res) {
  
  max_cores <- 32
  max_mem <- 512
  
  if (res$cores > max_cores) {
    cli::cli_abort(c(
      "{resources$cores} is too many cores.",
      i = "The largest node has {max_cores} cores."))
  }
  
  mem_per_node <- res$memory_per_node
  
  if ((!is.null(mem_per_node)) && (mem_per_node > max_mem)) {
    cli::cli_abort(c(
      "{resources$memory_per_node}Gb is too much memory per node.",
      i = "The largest node has {max_mem}Gb of RAM."))
  }
  
  mem_per_proc <- res$memory_per_process
  
  if ((!is.null(mem_per_proc)) && (mem_per_proc > max_mem)) {
    cli::cli_abort(c(
      "{resources$memory_per_node}Gb is too much memory per process.",
      i = "The largest node has {max_mem}Gb of RAM."))
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


windows_resources <- function(cores = 1L,
                              exclusive = FALSE,
                              runtime = NULL,
                              hold_until = NULL,
                              memory_per_node = NULL,
                              memory_per_process = NULL,
                              requested_nodes = NULL,
                              priority = NULL,
                              queue = NULL) {
                                
  res <- hipercow::hipercow_resource(cores, exclusive, runtime,
                                     hold_until, memory_per_node,
                                     memory_per_process, requested_nodes,
                                     priority, queue)
  
  windows_validate_resources(res)
                                     
}
