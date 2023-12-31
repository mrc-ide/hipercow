assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}


assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}


assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be a character", name), call. = FALSE)
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


match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("'%s' must be one of %s",
                 name, paste(squote(choices), collapse = ", ")))
  }
  arg
}
