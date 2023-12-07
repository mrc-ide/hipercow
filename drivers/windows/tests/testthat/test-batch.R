test_that("batch data creates entries for share drives", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root

  config <- root$config$windows
  dat <- template_data(config, path_root)

  nms <- c("hostname",
           "date",
           "hermod_version",
           "r_version",
           "network_shares_create",
           "network_shares_delete",
           "hermod_root_drive",
           "hermod_root_path",
           "cluster_name")
  expect_setequal(names(dat), nms)
  expect_true(all(vlapply(dat, function(x) is.character(x) && length(x) == 1)))
  expect_match(dat$network_shares_create,
               "net use X:", fixed = TRUE)

  expect_equal(dat$hermod_root_drive, "X:")
  expect_equal(dat$hermod_root_path, "\\b\\c")
})


test_that("batch data can run from subdirectory of root", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  path <- file.path(mount, "b/c/d")
  fs::dir_create(path)
  path <- normalize_path(path)
  dat <- template_data(config, path_root)
  expect_equal(dat$hermod_root_drive, "X:")
  expect_equal(dat$hermod_root_path, "\\b\\c")
})


test_that("can write a runner batch file", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hermod::hermod_task_create_explicit(quote(sessionInfo())))
  write_batch_task_run(id, config, path_root)
  expect_true(file.exists(file.path(root$path$tasks, id, "run.bat")))
})


test_that("can write a provision batch file", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- "abc123"
  path <- write_batch_provision_script(id, config, path_root)
  expect_equal(
    tail(fs::path_split(path)[[1]], 7),
    c(basename(mount), "b", "c", "hermod", "provision", id, "provision.bat"))
})
