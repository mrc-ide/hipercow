##' Create a bulk set of tasks. This is an experimental interface and
##' does not have an analogue within didehpc.  Variables in `data`
##' take precidence over variables in the environment in which `expr`
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
##' @param data Data that you wish to inject _rowwise_ into the expression
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
  quo <- rlang::inject(rlang::enquo(expr))

  ## TODO: might copy over the same bits as for expr(); easy enough to
  ## refactor out.
  if (!rlang::quo_is_call(quo)) {
    cli::cli_abort("Expected 'expr' to be a function call")
  }
  envir <- rlang::quo_get_env(quo)
  expr <- rlang::quo_get_expr(quo)

  ## warn about lack of overlap here?
  extra <- setdiff(all.vars(expr), names(data))
  variables <- task_variables(extra, envir, environment, root,
                              rlang::current_env())

  path <- relative_workdir(root$path$root)

  id <- vcapply(seq_len(nrow(data)), function(i) {
    variables_i <- variables
    variables_i$locals <- c(variables$locals, as.list(data[i, ]))
    task_create(root, "expression", path, environment,
                expr = expr, variables = variables_i)
  })

  task_submit_maybe(id, submit, root, rlang::current_env())
  id
}


## We'll move this out everywhere soon:
task_create <- function(root, type, path, environment, ...) {
  id <- ids::random_id()
  dest <- file.path(root$path$tasks, id)
  fs::dir_create(dest)
  data <- list(type = type, id = id, path = path, environment = environment,
               ...)
  saveRDS(data, file.path(dest, EXPR))
  file.create(file.path(dest, STATUS_CREATED))
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
