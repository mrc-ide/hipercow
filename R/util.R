`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}
