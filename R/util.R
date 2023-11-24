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


glue_whisker <- function(template, data) {
  transformer <- function(...) {
    ## This transformer prevents a NULL entry destroying the string
    glue::identity_transformer(...) %||% ""
  }
  glue::glue(template, .envir = data, .open = "{{", .close = "}}",
             .trim = FALSE, .transformer = transformer)
}


hostname <- function() {
  Sys.info()[["nodename"]]
}


hermod_file <- function(path) {
  system.file(path, mustWork = TRUE, package = "hermod")
}


is_directory <- function(path) {
  file.exists(path) && file.info(path, extra_cols = FALSE)[["isdir"]]
}


hermod_version <- function() {
  as.character(utils::packageVersion("hermod"))
}


normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}


clean_path <- function(x) {
  as.character(fs::path_tidy(x))
}


windows_path <- function(x) {
  gsub("/", "\\", x, fixed = TRUE)
}


unix_path <- function(x) {
  gsub("\\", "/", x, fixed = TRUE)
}


get_system_username <- function() {
  Sys.getenv(if (is_windows()) "USERNAME" else "USER")
}


readline_with_default <- function(prefix, default) {
  prompt <- sprintf("%s (default: %s) > ", prefix, default)
  result <- readline(prompt)
  if (result == "") default else result
}
