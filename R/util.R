`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


ensure_package <- function(name) {
  if (!requireNamespace(name, quietly = TRUE)) {
    ## TODO: once packages are on our universe, let's install
    ## automatically too.
    cli::cli_abort(c(
      "Please install the '{name}' package",
      c(i = "Try at https://github.com/mrc-ide/{name}")))
  }
  getNamespace(name)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


na_omit <- function(x) {
  x[!is.na(x)]
}
