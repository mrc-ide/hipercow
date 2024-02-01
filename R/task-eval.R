##' Run a task that has been created by a `task_create_*` function,
##' e.g., [task_create_explicit()], [task_create_expr()]. Generally
##' users should not run this function directly.
##'
##' @title Run a task
##'
##' @param id The task identifier
##'
##' @param envir An environment in which to evaluate the
##'   expression. For non-testing purposes, generally ignore this, the
##'   global environment will be likely the expected environment.
##'
##' @param verbose Logical, indicating if we should print information
##'   about what we do as we do it.
##'
##' @param root A hipercow root, or path to it. If `NULL` we search up
##'   your directory tree.
##'
##' @return Logical indicating success (`TRUE`) or failure (`FALSE`)
##' @export
##' @examples
##' cleanup <- hipercow_example_helper(runner = FALSE)
##' id <- task_create_expr(runif(1), driver = FALSE)
##' # Status is only 'created', not 'submitted', as we did not submit
##' # task.  This task can never run.
##' task_status(id)
##'
##' # Explicitly evaluate the task:
##' task_eval(id, verbose = TRUE)
##' task_result(id)
##'
##' cleanup()
task_eval <- function(id, envir = .GlobalEnv, verbose = FALSE, root = NULL) {
  root <- hipercow_root(root)
  t0 <- Sys.time()
  p0 <- proc.time()
  if (verbose) {
    version <- utils::packageVersion("hipercow")
    cli::cli_h1("hipercow {version} running at '{root$path$root}'")
    cli::cli_alert_info("library paths:")
    cli::cli_li(.libPaths())
    cli::cli_alert_info("id: {id}")
    cli::cli_alert_info("starting at: {t0}")
  }
  path <- file.path(root$path$tasks, id)
  status <- task_status(id, follow = FALSE, root = root)
  if (status %in% c("running", "success", "failure", "cancelled")) {
    cli::cli_abort("Can't start task '{id}', which has status '{status}'")
  }
  file.create(file.path(path, STATUS_RUNNING))
  data <- readRDS(file.path(path, DATA))

  top <- rlang::current_env() # not quite right, but better than nothing
  local <- new.env(parent = emptyenv())
  local$warnings <- collector()

  if (verbose) {
    cli::cli_alert_info("task type: {data$type}")
  }
  result <- rlang::try_fetch({
    if (data$type == "retry") {
      data <- readRDS(file.path(root$path$tasks, data$base, DATA))
      if (verbose) {
        cli::cli_alert_info("pointing at {data$id} ({data$type})")
      }
      data$id <- id
    }
    envvars_apply(data$envvars, top)

    default_cores <- hipercow_parallel_get_cores()
    if (is.na(default_cores)) default_cores <- NULL
    hipercow_parallel_set_cores(default_cores, rlang::current_env())

    environment_apply(data$environment, envir, root, top)
    if (!is.null(data$parallel)) {
      hipercow_parallel_setup(data$parallel$method)
    }
    check_globals(data$variables$globals, envir, top)
    withr::local_dir(file.path(root$path$root, data$path))
    switch(
      data$type,
      explicit = task_eval_explicit(data, envir, verbose),
      expression = task_eval_expression(data, envir, verbose),
      script = task_eval_script(data, envir, verbose),
      cli::cli_abort("Tried to evaluate unknown type of task '{data$type}'"))
  },
  warning = function(e) {
    local$warnings$add(e)
    tryInvokeRestart("muffleWarning")
  },
  error = function(e) {
    if (is.null(e$trace)) {
      e$trace <- rlang::trace_back(top)
    }
    local$error <- e
    NULL
  })

  success <- is.null(local$error)
  warnings <- local$warnings$get()
  t1 <- Sys.time()
  p1 <- proc.time()
  times <- c(created = data$time,
             started = t0,
             finished = t1)
  if (success) {
    status_file <- STATUS_SUCCESS
    status <- "success"
  } else {
    result <- local$error
    if (length(warnings) > 0) {
      result$warnings <- warnings
    }
    status_file <- STATUS_FAILURE
    status <- "failure"
  }
  ## NOTE: saving memory information here with the call to gc would be
  ## more accurate with `full = TRUE`, but that adds a reasonable
  ## overhead. We might want to change this later.
  memory <- gc(full = FALSE)
  info <- list(status = status, times = times, cpu = p1 - p0, memory = memory)
  saveRDS(info, file.path(path, INFO))
  saveRDS(result, file.path(path, RESULT))
  file.create(file.path(path, status_file))

  if (verbose) {
    if (success) {
      cli::cli_alert_success("status: success")
    } else {
      cli::cli_alert_danger("status: failure")
      cli::cli_alert_danger("Error: {conditionMessage(result)}")
    }

    show_collected_warnings(warnings)

    t1 <- Sys.time()
    cli::cli_alert_info(
      "finishing at: {t0} (elapsed: {format(t1 - t0, digits = 4)})")
  }

  invisible(success)
}


task_eval_explicit <- function(data, envir, verbose) {
  task_show_expr(data$expr, verbose)
  task_show_locals(data$variables$locals, verbose)

  if (!is.null(data$variables$locals)) {
    list2env(data$variables$locals, envir)
  }
  eval_with_hr(eval(data$expr, envir), "task logs", verbose)
}


task_eval_expression <- function(data, envir, verbose) {
  task_show_expr(data$expr, verbose)
  task_show_locals(data$variables$locals, verbose)
  rlang::env_bind(envir, !!!data$variables$locals)
  eval_with_hr(eval(data$expr, envir), "task logs", verbose)
}


task_eval_script <- function(data, envir, verbose) {
  script <- data$script
  chdir <- data$chdir
  echo <- data$echo
  eval_with_hr(
    source(script, local = envir, chdir = chdir, echo = echo,
           max.deparse.length = Inf, keep.source = TRUE, spaced = FALSE),
    "task logs", verbose)
  ## Nothing sensible can be returned here!
  NULL
}
