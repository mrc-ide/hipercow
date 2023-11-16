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


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


httr_text <- function(r) {
  httr::content(r, as = "text", encoding = "UTF-8")
}


squote <- function(x) {
  sprintf("'%s'", x)
}


is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}


modify_list <- function(x, val, name = deparse(substitute(val))) {
  extra <- setdiff(names(val), names(x))
  if (length(extra) > 0L) {
    stop(sprintf("Unknown elements in %s: %s",
                    name, paste(extra, collapse = ", ")))
  }
  modifyList(x, val)
}


string_starts_with <- function(x, y) {
  substr(x, 1, nchar(y)) == y
}



sys_which <- function(name) {
  ret <- Sys.which(name)
  if (ret == "") {
    stop(sprintf("%s not found in $PATH", name))
  }
  ret
}


system_intern_check <- function(...) {
  res <- suppressWarnings(system(..., intern = TRUE))
  status <- attr(res, "status", exact = TRUE)
  if (!is.null(status) && status > 0) {
    stop("Error running command")
  }
  res
}
