assert_scalar <- function(x, name = deparse(substitute(x)), call = NULL) {
  if (length(x) != 1) {
    cli::cli_abort("'{name}' must be a scalar", call = NULL)
  }
}


assert_scalar_character <- function(x, name = deparse(substitute(x)),
                                    call = NULL) {
  assert_scalar(x, name, call)
  assert_character(x, name, call)
}


assert_scalar_logical <- function(x, name = deparse(substitute(x)),
                                  call = NULL) {
  assert_scalar(x, name, call)
  assert_logical(x, name, call)
}


assert_character <- function(x, name = deparse(substitute(x)), call = NULL) {
  if (!is.character(x)) {
    cli::cli_abort("'{name}' must be a character", call = call)
  }
}


assert_logical <- function(x, name = deparse(substitute(x)), call = NULL) {
  if (!is.logical(x)) {
    cli::cli_abort("'{name}' must be logical", call = call)
  }
}


assert_scalar_integer <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_integer(x, name)
}


assert_integer <- function(x, name = deparse(substitute(x))) {
  if (!(is.integer(x) || all(x - round(x) == 0))) {
    stop(sprintf("'%s' must be an integer", name), call. = FALSE)
  }
}


assert_is <- function(x, what, name = deparse(substitute(x)), arg = name, 
                      call = NULL) {
  if (!inherits(x, what)) {
    cli::cli_abort(
      c("'{name}' must be a {paste(what, collapse = ' / ')}", 
        "{name} was a {paste(class(x), collapse = ' / ')}"), 
      call = call, arg = arg)
  }
}
