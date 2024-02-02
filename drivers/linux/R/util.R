`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


glue_whisker <- function(template, data) {
  transformer <- function(...) {
    ## This transformer prevents a NULL entry destroying the string
    glue::identity_transformer(...) %||% ""
  }
  glue::glue_data(data, template, .open = "{{", .close = "}}",
                  .trim = FALSE, .transformer = transformer)
}

hostname <- function() {
  Sys.info()[["nodename"]]
}


hipercow_linux_file <- function(path) {
  system.file(path, mustWork = TRUE, package = "hipercow.linux")
}


hipercow_version <- function() {
  as.character(utils::packageVersion("hipercow"))
}


read_lines <- function(...) {
  paste(readLines(...), collapse = "\n")
}


normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
