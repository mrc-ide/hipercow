hermod_init <- function(path = ".") {
  dest <- file.path(path, "hermod.json")
  if (file.exists(dest)) {
    cli::cli_alert_info("hermod already initialised at '{path}'")
  } else {
    dir.create(path, FALSE, TRUE)
    writeLines("{}", dest)
    cli::cli_alert_success("Initialised hermod at '{path}'")
    cli::cli_alert_info("Next, call hermod_configure()")
  }
  invisible(hermod_root(path))
}


hermod_root <- function(root = NULL) {
  if (inherits(root, "hermod_root")) {
    return(root)
  }
  path <- hermod_root_find(root)
  if (is.null(cache$path)) {
    ret <- new.env(parent = emptyenv())
    ret$path <- list(root = path,
                     tasks = file.path(path, "hermod", "tasks"),
                     config = file.path(path, "hermod", "config"))
    if (file.exists(ret$path$config)) {
      ret$config <- readRDS(ret$path$config)
    }
    class(ret) <- "hermod_root"
    cache$path <- ret
  }
  cache$path
}


hermod_root_find <- function(path) {
  path <- rprojroot::find_root(rprojroot::has_file("hermod.json"),
                               path %||% ".")
  normalize_path(path)
}
