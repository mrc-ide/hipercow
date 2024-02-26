write_batch_task_run <- function(task_id, config, resources, path_root) {
  data <- template_data(config, resources, path_root)
  data$task_id <- task_id
  template_name <- sprintf("%s/task_run.sh", config$manager)
  str <- glue_whisker(read_template(template_name), data)
  path <- file.path(path_root, "hipercow", "tasks", task_id, BATCH_RUN)
  writeLines(str, path)
  path

}


read_template <- function(name) {
  read_lines(hipercow_linux_file(sprintf("templates/%s", name)))
}


template_data <- function(config, resources, path_root) {
  ## Semicolon delimited list on windows; see "Managing libraries" in
  ## https://cran.r-project.org/doc/manuals/r-release/R-admin.html
  hipercow_library <- paste(config$path_lib, path_bootstrap(config), sep = ":")

  ## TODO: default walltime should come from the configuration
  walltime <- walltime_with_default(resources, "01:00:00")
  ## TODO: treatment of cores is quite hard, especially where cores =
  ## Inf wants to select a single node.  There's also issues around
  ## "mpinodes" and issues around the complete lack of any sort of
  ## usable documentation.
  cores <- resources$cores
  stopifnot(is.finite(cores))

  memory <- memory_with_default(resources, "4")

  ## TODO: we're going to need to query this periodically and find the
  ## best match.  Running 'module avail R/' gets us most of the way
  ## there but we need this in machine readable format and also limit
  ## to those in the "prod" set..  The prod module will be ict
  ## specific so that needs to go into the configuration too.
  r_module <- "R/4.3.2-gfbf-2023a"

  list(
    hostname = hostname(),
    date = as.character(Sys.time()),
    hipercow_version = hipercow_version(),
    r_module = r_module,
    walltime = walltime,
    cores = cores,
    memory = memory,
    hipercow_library = hipercow_library)
}



walltime_with_default <- function(resources, default) {
  if (is.null(resources$max_runtime)) {
    return(default)
  }
  mins <- resources$max_runtime %% 60
  hours <- resources$max_runtime %/% 60
  sprintf("%02d:02d:00", hours, mins)
}


memory_with_default <- function(resources, default) {
  if (!is.null(resources$memory_per_node)) {
    return(resources$memory_per_node)
  }
  if (!is.null(resources$memory_per_process)) {
    stopifnot(is.finite(resources$cores))
    return(resources$memory_per_process * resources$cores)
  }
  default
}
