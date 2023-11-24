test_that("batch data creates entries for share drives", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  dat <- template_data(root$path$root, root)

  nms <- c("hostname",
           "date",
           "hermod_version",
           "r_version",
           "network_shares_create",
           "network_shares_delete",
           "hermod_workdir_drive",
           "hermod_workdir_path",
           "hermod_root_path_abs",
           "cluster_name")
  expect_setequal(names(dat), nms)
  expect_true(all(vlapply(dat, function(x) is.character(x) && length(x) == 1)))
  expect_match(dat$network_shares_create,
               "net use X:", fixed = TRUE)

  expect_equal(dat$hermod_workdir_drive, "X:")
  expect_equal(dat$hermod_workdir_path, "\\b\\c")
  expect_equal(dat$hermod_root_path_abs, "X:\\b\\c")
})


test_that("batch data can run from subdirectory of root", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path <- file.path(mount, "b/c/d")
  fs::dir_create(path)
  dat <- template_data(path, root)
  expect_equal(dat$hermod_workdir_drive, "X:")
  expect_equal(dat$hermod_workdir_path, "\\b\\c\\d")
  expect_equal(dat$hermod_root_path_abs, "X:\\b\\c")
})


test_that("batch data can not run outside root directory", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path <- file.path(mount, "b")
  expect_error(template_data(path, root),
               "Expected working directory to be within hermod root")
})


test_that("can write a runner batch file", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  dat <- template_data(root$path$root, root)
  id <- hermod_task_create_explicit(quote(sessionInfo()), root = root)
  write_batch_task_run(id, root$path$root, root)
  expect_true(file.exists(file.path(root$path$tasks, id, "run.bat")))
})
