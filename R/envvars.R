##' Create environment variables for use with a hipercow task.
##'
##' @title Environment variables
##'
##' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named environment
##'   variable.  If unnamed, it is assumed to refer to an environment
##'   variable that exists.
##'
##' @return A list with class `envvars` which should not be modified.
##'
##' @export
envvars <- function(...) {
  args <- rlang::dots_list(..., .named = FALSE)
  if (length(args) == 0) {
    return(structure(character(), names = character(), class = "envvars"))
  }
  ok <- vlapply(args, rlang::is_scalar_character)
  if (!all(ok)) {
    cli::cli_abort("All arguments to 'envvars' must be strings")
  }
  env <- vcapply(args, identity)
  i <- !nzchar(names(env))
  if (any(i)) {
    value <- Sys.getenv(vcapply(env[i], identity), NA_character_)
    if (any(is.na(value))) {
      cli::cli_abort(
        c(paste("Failed to look up environment variables for",
                "unnamed arguments to 'envvars'"),
          set_names(env[i][is.na(value)], "x")))
    }
    names(env)[i] <- env[i]
    env[i] <- value
  }
  class(env) <- "envvars"
  env
}


##' @export
c.envvars <- function(...) {
  inputs <- list(...)
  is_envvar <- vlapply(inputs, inherits, "envvars")

  if (any(!is_envvar)) {
    inputs[!is_envvar] <- lapply(inputs[!is_envvar], function(x) envvars(!!!x))
  }

  ret <- unlist(lapply(inputs, unclass))
  class(ret) <- "envvars"
  ret
}
