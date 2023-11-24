init_quietly <- function(...) {
  suppressMessages(hermod_init(...))
}


same_path <- function(a, b) {
  normalizePath(a, "/", TRUE) == normalizePath(b, "/", TRUE)
}


cache$r_versions <- numeric_version(c("4.0.5", "4.1.3", "4.2.3", "4.3.0"))
