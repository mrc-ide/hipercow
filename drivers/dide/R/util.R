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
  glue::glue_data(data, template, .open = "{{", .close = "}}",
                  .trim = FALSE, .transformer = transformer)
}


hipercow_dide_file <- function(path) {
  system.file(path, mustWork = TRUE, package = "hipercow.dide")
}


hipercow_version <- function() {
  if (is.null(cache$hipercow_version)) {
    cache$hipercow_version <- as.character(utils::packageVersion("hipercow"))
  }
  cache$hipercow_version
}


hipercow_dide_version <- function() {
  if (is.null(cache$hipercow_dide_version)) {
    cache$hipercow_dide_version <-
      as.character(utils::packageVersion("hipercow.dide"))
  }
  cache$hipercow_dide_version
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


quoted_comma_sep <- function(v) {
  sprintf('c("%s")', paste0(v, collapse = "\", \""))
}


menu <- function(choices, cancel = choices[[1]]) {
  idx <- utils::menu(choices)
  if (idx == 0) cancel else choices[[idx]]
}


nonbreaking <- function(x) {
  gsub(" ", "\u00a0", x)
}


as_character_integer <- function(x) {
  format(x, scientific = FALSE)
}


## Semicolon delimited list on windows; see "Managing libraries" in
## https://cran.r-project.org/doc/manuals/r-release/R-admin.html
path_delimiter <- function(platform) {
  if (platform == "windows") ";" else ":"
}

write_linux_lines <- function(text, con) {
  file <- file(con, "wb")
  writeLines(text, file)
  close(file)
}
