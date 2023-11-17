init_quietly <- function(...) {
  suppressMessages(hermod_init(...))
}


same_path <- function(a, b) {
  normalizePath(a, "/", TRUE) == normalizePath(b, "/", TRUE)
}
