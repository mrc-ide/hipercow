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
