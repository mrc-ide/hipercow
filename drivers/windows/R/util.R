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


dquote <- function(x) {
  sprintf('"%s"', x)
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


hipercow_windows_file <- function(path) {
  system.file(path, mustWork = TRUE, package = "hipercow.windows")
}


hipercow_version <- function() {
  as.character(utils::packageVersion("hipercow"))
}


normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


clean_path <- function(x) {
  as.character(fs::path_tidy(x))
}


windows_path_slashes <- function(x) {
  gsub("/", "\\", x, fixed = TRUE)
}


unix_path_slashes <- function(x) {
  gsub("\\", "/", x, fixed = TRUE)
}


get_system_username <- function() {
  Sys.getenv(if (is_windows()) "USERNAME" else "USER", NA_character_)
}


readline_with_default <- function(prefix, default) {
  if (is.na(default) || default == "") {
    prompt <- sprintf("%s > ", prefix)
  } else {
    prompt <- sprintf("%s (default: %s) > ", prefix, default)
  }
  result <- readline(prompt)
  if (result == "") {
    if (is.na(default)) {
      cli::cli_abort("A value must be provided")
    }
    result <- default
  }
  result
}


readlines_if_exists <- function(path, ...) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readLines(path, ...)
}


writelines_if_different <- function(text, path) {
  skip <- file.exists(path) && identical(readLines(path), text)
  if (!skip) {
    writeLines(text, path)
  }
}


version_string <- function(v, sep = "_") {
  paste(unclass(v)[[1]], collapse = sep)
}
