## Common support code for vignettes. This will not be echoed to the
## user, so be sure not to define functions here that they might want
## to use.
##
## Typically, include this in the Rmd within a block like:
##
## ```{r, include = FALSE}
## ...
## ```
knitr::opts_chunk$set(
  collapse = TRUE,
  error = FALSE,
  comment = "#>"
)

add_header <- function() {
  writeLines("<!-- Please edit the file in vignettes_src/ -->")
}

dir_tree <- function(path, sub = ".", ...) {
  withr::with_dir(path, fs::dir_tree(sub, ...))
}

lang_output <- function(x, lang) {
  writeLines(c(sprintf("```%s", lang), x, "```"))
}

plain_output <- function(x) {
  lang_output(x, "")
}

r_output <- function(x) {
  lang_output(x, "r")
}

inline <- function(x) {
  sprintf("`%s`", format(x))
}

dir_tree_hipercow <- function(path) {
  withr::with_dir(path, fs::dir_tree(glob = "hipercow/*", invert = TRUE))
}

sys_getenv <- function(x) {
  res <- Sys.getenv(x)
  if (res == "") {
    return(NULL)
  }
  res
}

new_hipercow_root_path <- function(windows = FALSE) {
  base <- if (windows) sys_getenv("HIPERCOW_VIGNETTE_ROOT") else NULL
  path <- hipercow:::hipercow_temporary_directory_path(base)
  dir.create(path, FALSE, TRUE)
  path
}

set_vignette_root <- function(path) {
  dir.create(path, FALSE, TRUE)
  knitr::opts_knit$set(root.dir = path)
  if (!isTRUE(getOption("knitr.in.progress"))) {
    setwd(path)
  }
}

abbrev_id <- function(x) {
  inline(paste0(substr(x, 1, 6), "..."))
}

# Would be nice to scope these to run on knitr start, and unset later...
options(
  hipercow.auto_install_missing_packages = FALSE,
  hipercow.timeout = 60)
