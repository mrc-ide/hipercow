##' Create an explicit task. Explicit tasks are the simplest sort of
##' task in hipercow and do nothing magic. They accept an R expression
##' (from `quote` or friends) and possibly a set of variables to
##' export from the global environment.  This can then be run on a
##' cluster by loading your variables and running your expression.  If
##' your expression depends on packages being *attached* then you
##' should pass a vector of package names too.  This function may
##' disappear, and is used by us to think about the package, it's not
##' designed to really be used.
##'
##' @title Create explicit task
##'
##' @param expr Unevaluated expression object, e.g., from `quote`
##'
##' @param export Optional character vector of names of objects to
##'   export into the evaluating environment
##'
##' @param envir Local R environment in which to find variables for
##'   `export`
##'
##' @param environment Name of the hipercow environment to evaluate the
##'   task within.
##'
##' @param submit Control over task submission. This will expand over
##'   time once we support specifying resources. The most simple
##'   interface is to use `TRUE` here to automatically submit a task,
##'   using your default configuration, or `FALSE` to prevent
##'   submission.  The default `NULL` will submit a task if a driver
##'   is configured.
##'
##' @inheritParams task_eval
##'
##' @return A task id, a string of hex characters. Use this to
##'   interact with the task.
##'
##' @export
task_create_explicit <- function(expr, export = NULL, envir = .GlobalEnv,
                                 environment = "default", submit = NULL,
                                 root = NULL) {
  root <- hipercow_root(root)
  variables <- task_variables(export, envir, environment, root,
                              rlang::current_env())
  path <- relative_workdir(root$path$root)
  id <- task_create(root, "explicit", path, environment,
                    expr = expr, variables = variables)
  task_submit_maybe(id, submit, root, rlang::current_env())
  id
}


##' Create a task based on an expression. This is similar to
##' [task_create_explicit] except more magic, and is closer to
##' the interface that we expect people will use.
##'
##' @title Create a task based on an expression
##'
##' @param expr The expression, does not need quoting.
##'
##' @inheritParams task_create_explicit
##'
##' @inherit task_create_explicit return
##' @export
task_create_expr <- function(expr, environment = "default", submit = NULL,
                             root = NULL) {
  root <- hipercow_root(root)
  expr <- check_expression(rlang::enquo(expr))
  variables <- task_variables(
    all.vars(expr$value), expr$envir, environment, root, rlang::current_env())
  path <- relative_workdir(root$path$root)
  id <- task_create(root, "expression", path, environment,
                    expr = expr$value, variables = variables)
  task_submit_maybe(id, submit, root, rlang::current_env())
  id
}


##' Create a task from a script.  This will arrange to run the file
##' `script` via hipercow.  The script must exist within your hipercow
##' root, but you may change to the directory of the script as it
##' executes (otherwise we will evaluate from your current directory
##' relative to the hipercow root, as usual).
##'
##' @title Create script task
##'
##' @param script Path for the script
##'
##' @param chdir Logical, indicating if we should change the working
##'   directory to the directory containing `script` before executing
##'   it (similar to the `chdir` argument to [`source`]).
##'
##' @param echo Passed through to `source` to control printing while
##'   evaluating.  Generally you will want to leave this as `TRUE`
##'
##' @inheritParams task_create_explicit
##'
##' @return A task id, a string of hex characters. Use this to
##'   interact with the task.
##'
##' @export
task_create_script <- function(script, chdir = FALSE, echo = TRUE,
                               environment = "default", submit = NULL,
                               root = NULL) {
  root <- hipercow_root(root)
  if (!file.exists(script)) {
    cli::cli_abort("Script file '{script}' does not exist")
  }
  if (!fs::path_has_parent(script, root$path$root)) {
    cli::cli_abort(
      "Script file '{script}' is not contained within hipercow root")
  }
  path <- relative_workdir(root$path$root)
  script <- as.character(fs::path_rel(script, getwd()))
  assert_scalar_logical(chdir, call = rlang::current_env())
  ensure_environment_exists(environment, root, rlang::current_env())

  id <- task_create(root, "script", path, environment,
                    script = script, chdir = chdir, echo = echo)
  task_submit_maybe(id, submit, root, rlang::current_env())
  id
}


##' Create a bulk set of tasks. This is an experimental interface and
##' does not have an analogue within didehpc.  Variables in `data`
##' take precedence over variables in the environment in which `expr`
##' was created. There is no "pronoun" support yet (see rlang docs).
##' Use `!!` to pull a variable from the environment if you need to,
##' but be careful not to inject something really large (e.g., any
##' vector really) or you'll end up with a revolting expression and
##' poor backtraces.  We will likely change some of these semantics
##' later, be careful.
##'
##' @title Create bulk tasks via with expressions
##'
##' @param expr An expression, as for [task_create_expr]
##'
##' @param data Data that you wish to inject _row-wise_ into the expression
##'
##' @inheritParams task_create_explicit
##'
##' @return A vector of ids, with the same length as `data` has rows.
##'
##' @export
task_create_bulk_expr <- function(expr, data, environment = "default",
                                  submit = NULL, root = NULL) {
  root <- hipercow_root(root)
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("Expected 'data' to be a data.frame (or tbl, etc)")
  }

  ## This will allow `!!x` to reference a value in the enclosing
  ## environment and we'll splice it into the expression. This will
  ## work pretty well for simple things and _terribly_ for large
  ## objects, which would be better pulled in by name if possible.
  ##
  ## We could do this using "eval_tidy" and use "pronouns" but that
  ## will require a little more setup; probably worth considering
  ## though.  For now this is fine, but we can improve this by:
  ##
  ## * Not doing the injection until later
  ## * Setting up the bits for eval_tidy and exporting them
  ## * Analysing the expression before injection and making sure
  ##   that anything injected is small
  expr <- check_expression(rlang::inject(rlang::enquo(expr)))

  ## Warn about lack of overlap here? That is, if there's nothing
  ## within locals that could be referenced from the data.frame that's
  ## likely an error.
  extra <- setdiff(all.vars(expr$value), names(data))
  variables <- task_variables(
    extra, expr$envir, environment, root, rlang::current_env())
  path <- relative_workdir(root$path$root)
  id <- vcapply(seq_len(nrow(data)), function(i) {
    variables_i <- variables
    variables_i$locals <- c(variables$locals, as.list(data[i, ]))
    task_create(root, "expression", path, environment,
                expr = expr$value, variables = variables_i)
  })
  task_submit_maybe(id, submit, root, rlang::current_env())
  id
}


## We'll move this out everywhere soon:
task_create <- function(root, type, path, environment, ...) {
  id <- ids::random_id()
  time <- Sys.time()
  dest <- file.path(root$path$tasks, id)
  fs::dir_create(dest)
  data <- list(type = type, id = id, time = time,
               path = path, environment = environment,
               ...)
  saveRDS(data, file.path(dest, EXPR))
  id
}


check_expression <- function(quo) {
  if (rlang::quo_is_symbol(quo)) {
    sym <- rlang::as_name(rlang::quo_get_expr(quo))
    envir <- rlang::quo_get_env(quo)
    if (!rlang::env_has(envir, sym, inherit = TRUE)) {
      cli::cli_abort("Could not find expression '{sym}'")
    }
    expr <- rlang::env_get(envir, sym, inherit = TRUE)
    if (!rlang::is_call(expr)) {
      cli::cli_abort(c(
        "Expected 'expr' to be a function call",
        i = paste("You passed a symbol '{sym}', but that turned out to be",
                  "an object of type {typeof(expr)} and not a call")))
    }
  } else {
    if (!rlang::quo_is_call(quo)) {
      cli::cli_abort("Expected 'expr' to be a function call")
    }
    envir <- rlang::quo_get_env(quo)
    expr <- rlang::quo_get_expr(quo)
  }

  if (rlang::is_call(expr, "quote")) {
    given <- rlang::expr_deparse(expr)
    alt <- rlang::expr_deparse(expr[[2]])
    cli::cli_abort(
      c("You have an extra layer of quote() around 'expr'",
        i = "You passed '{given}' but probably meant to pass '{alt}'"))
  }
  list(value = expr, envir = envir)
}


task_variables <- function(names, envir, environment, root, call = NULL) {
  if (length(names) == 0) {
    ensure_environment_exists(environment, root, rlang::current_env())
    NULL
  } else {
    in_environment <- environment_load(environment, root, call)$globals
    nms_globals <- intersect(names, in_environment)
    nms_locals <- setdiff(names, nms_globals)

    locals <- rlang::env_get_list(envir, nms_locals, inherit = TRUE,
                                  last = topenv())
    check_locals_size(locals, call = call)

    validate_globals <- getOption("hipercow.validate_globals", FALSE)
    if (validate_globals && length(nms_globals) > 0) {
      globals <- rlang::env_get_list(envir, nms_globals, inherit = TRUE,
                                     last = topenv())
      globals <- vcapply(globals, rlang::hash)
    } else {
      globals <- NULL
    }

    list(locals = locals, globals = globals)
  }
}


task_submit_maybe <- function(id, submit, root, call) {
  if (!is.null(submit)) {
    ## Could also allow character here soon.
    assert_scalar_logical(submit, call = call)
  }
  has_config <- length(root$config) > 0
  if (isFALSE(submit) || (!has_config && is.null(submit))) {
    return(FALSE)
  }
  if (!has_config) {
    cli::cli_abort(
      c("Can't submit task because no driver configured",
        i = "Run 'hipercow::hipercow_configure()' to configure a driver"),
      call = call)
  }
  if (length(root$config) == 1) {
    driver <- names(root$config)
  } else {
    cli::cli_abort("Can't cope with more than one driver configured yet",
                   call = call)
  }
  task_submit(id, driver = driver, root = root)
  TRUE
}


check_locals_size <- function(locals, call = NULL) {
  if (length(locals) == 0) {
    return()
  }
  max_size <- getOption("hipercow.max_size_local", 1e6)
  if (!is.finite(max_size)) {
    return()
  }
  size <- vnapply(locals, utils::object.size)
  err <- names(locals)[size > max_size]
  if (length(err) > 0) {
    max_size_bytes <- format_bytes(max_size)
    cli::cli_abort(
      c("Object{?s} too large to save with task: {squote(err)}",
        x = "Objects saved with a hipercow task can only be {max_size_bytes}",
        i = paste("You can increase the limit by increasing the value of",
                  "the option 'hipercow.max_size_local', even using 'Inf' to",
                  "disable this check entirely"),
        i = paste("Better again, create large objects from your 'sources'",
                  "argument to your environment, and then advertise this",
                  "using the 'globals' argument")),
      call = call)
  }
}
