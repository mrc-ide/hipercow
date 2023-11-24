tmp_options_didehpc <- function(...) {
  opts <- options()
  i <- grepl("^didehpc\\.", names(opts))
  if (any(i)) {
    opts[i] <- list(NULL)
    base <- opts[i]
  } else {
    base <- list()
  }
  c(base, ...)
}


example_mounts <- function(root) {
  remote <- c("\\\\fi--didef3\\other",
              "\\\\fi--san03\\homes\\bob",
              "\\\\fi--didenas1\\Project",
              "\\\\fi--didef3\\tmp",
              "\\\\wpia-hn\\newshare")
  local <- file.path(root, c("other", "home", "proj", "temp", "sk"))
  fs::dir_create(file.path(local, "sub"))
  cbind(remote = remote, local = local)
}


example_config <- function(..., root = tempfile()) {
  mounts <- example_mounts(root)
  workdir <- file.path(root, "sk", "sub")
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)


  withr::with_options(
    tmp_options_didehpc(),
    didehpc_config(credentials = "bob", workdir = workdir, ...))
}


example_root <- function() {
  root <- tempfile()
  path <- file.path(root, "share", "sub")
  dir.create(path)

}
