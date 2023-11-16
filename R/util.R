`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


read_lines <- function(...) {
  paste(readLines(...), collapse = "\n")
}


from_json <- function(x, ...) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE, ...)
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


httr_text <- function(r) {
  httr::content(r, as = "text", encoding = "UTF-8")
}


squote <- function(x) {
  sprintf("'%s'", x)
}
