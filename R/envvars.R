##' Create environment variables for use with a hipercow task.
##'
##' @title Environment variables
##'
##' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named environment
##'   variable.  If unnamed, it is assumed to refer to an environment
##'   variable that exists.
##'
##' @param secret Are these environment variables secret?  If so we will save
##'
##' @return A list with class `envvars` which should not be modified.
##'
##' @export
envvars <- function(..., secret = FALSE) {
  assert_scalar_logical(secret)
  args <- rlang::dots_list(..., .named = FALSE)
  if (length(args) == 0) {
    return(make_envvars(character(), character(), logical()))
  }
  ok <- vlapply(args, rlang::is_scalar_atomic)
  if (!all(ok)) {
    cli::cli_abort("All arguments to 'envvars' must be scalars")
  }
  value <- vcapply(args, as.character)
  name <- names(value)
  i <- !nzchar(name)
  if (any(i)) {
    name[i] <- value[i]
    value[i] <- Sys.getenv(vcapply(name[i], identity), NA_character_)
    if (any(is.na(value[i]))) {
      cli::cli_abort(
        c(paste("Failed to look up environment variables for",
                "unnamed arguments to 'envvars'"),
          set_names(name[i][is.na(value[i])], "x")))
    }
  }
  make_envvars(name, value, secret)
}


##' @export
c.envvars <- function(...) {
  inputs <- list(...)
  is_envvar <- vlapply(inputs, inherits, "envvars")
  if (any(!is_envvar)) {
    cli::cli_abort("Can't combine 'envvars' objects and other objects")
  }
  ret <- rlang::inject(rbind(!!!inputs))
  class(ret) <- c("envvars", "data.frame")
  ret
}


make_envvars <- function(name, value, secret) {
  ret <- data.frame(name = as.character(name),
                    value = as.character(value),
                    secret = secret,
                    stringsAsFactors = FALSE)
  class(ret) <- c("envvars", class(ret))
  ret
}
