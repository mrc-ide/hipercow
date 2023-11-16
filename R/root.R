hermod_init <- function(path) {
  dest <- file.path(path, "hermod.json")
  if (file.exists(dest)) {
    cli::cli_alert_info("hermod already initialised at '{path}'")
  } else {
    dir.create(path, FALSE, TRUE)
    writeLines("{}", dest)
    cli::cli_alert_info("Initialised hermod at '{path}'")
  }
  invisible(hermod_root(path))
}


hermod_root <- function(root = NULL) {
  if (inherits(root, "hermod_root")) {
    return(root)
  }
  ret <- new.env(parent = emptyenv())
  path <- rprojroot::find_root(rprojroot::has_file("hermod.json"),
                               root %||% ".")
  ret$path <- list(root = path,
                   tasks = file.path(path, "hermod", "tasks"))
  class(ret) <- "hermod_root"
  ret
}
