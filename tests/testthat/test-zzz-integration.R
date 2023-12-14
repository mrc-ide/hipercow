test_that("can provision a library", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  writeLines('install.packages("R6")', file.path(path_here, "provision.R"))
  hipercow_provision(root = path_here, show_log = FALSE)
  expect_true(file.exists(file.path(path_there, "hipercow", "lib", "R6")))
})
