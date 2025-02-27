test_that("batch data creates entries for share drives", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root

  config <- root$config[["dide-windows"]]
  dat <- template_data_common(config, path_root)

  nms <- c("hostname",
           "date",
           "hipercow_version",
           "hipercow_windows_version",
           "r_version",
           "network_shares_create",
           "network_shares_delete",
           "hipercow_root_drive",
           "hipercow_root_path",
           "cluster_name")
  expect_setequal(names(dat), nms)
  expect_true(all(vlapply(dat, function(x) is.character(x) && length(x) == 1)))
  expect_match(dat$network_shares_create,
               "net use X:", fixed = TRUE)

  expect_equal(dat$hipercow_root_drive, "X:")
  expect_equal(dat$hipercow_root_path, "\\b\\c")
})


test_that("batch data uses absolute paths", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root

  config <- root$config[["dide-windows"]]
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), driver = FALSE))
  dat <- template_data_task_run(id, config, path_root)

  v <- version_string(config$r_version, ".")
  expected <- sprintf(
    "X:/b/c/hipercow/lib/windows/%s;I:/bootstrap-windows/%s", v, v)
  expect_equal(dat$hipercow_library, expected)

  expected <- sprintf("X:/b/c/hipercow/tasks/%s/%s/Renviron",
                      substr(id, 1, 2),
                      substr(id, 3, nchar(id)))
  expect_equal(dat$renviron_path, expected)
})


test_that("batch data can run from subdirectory of root", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config[["dide-windows"]]

  dat <- template_data_common(config, path_root)
  expect_equal(dat$hipercow_root_drive, "X:")
  expect_equal(dat$hipercow_root_path, "\\b\\c")
})


test_that("can write a runner batch file", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config[["dide-windows"]]
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), driver = FALSE))
  write_batch_task_run_windows(id, config, path_root)
  expect_true(file.exists(path_to_task_file(path_root, id, "run.bat")))
})


test_that("can write a provision batch file", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config[["dide-windows"]]
  id <- "abc123"
  path <- write_batch_provision_script_windows(id, config, path_root)
  expect_equal(
    tail(fs::path_split(path)[[1]], 7),
    c(basename(mount), "b", "c", "hipercow", "provision", id, "provision.bat"))
})


test_that("Can prepare a linux task", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config[["dide-linux"]]
  id <- "12345678123456781234567812345678"
  #res <- write_batch_task_run_linux(id, config, path_root)

})