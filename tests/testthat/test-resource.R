test_that("Validate resource args", {
  expect_silent(validate_cores(Inf))
  expect_error(validate_cores(-Inf))
  expect_silent(validate_cores(1))
  expect_error(validate_cores(-1))
  expect_error(validate_cores("potato"))
  expect_error(validate_cores(NULL))
  expect_error(validate_cores(NA))
  expect_error(validate_cores(mtcars))
  expect_error(validate_cores(c(1, 2, 3)))
  
  expect_error(validate_runtime(c(1, 2, 3)))
  expect_silent(validate_runtime(10L))
  
  expect_silent(validate_hold_until("tonight"))
  expect_silent(validate_hold_until("midnight"))
  expect_error(validate_hold_until(c("a", "b", "c")))
  
  expect_error(validate_memory(c("a", "b")))
  
  expect_error(validate_nodes(NA))
  
  expect_error(validate_priority(c(1, 2, 3)))
  
  expect_error(validate_queue(c("a", "b")))
  
})

test_that("Can create task resource", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  root <- hipercow_root(path_here)
  
  res <- task_resources(driver = "elsewhere", root = root)
  expect_equal(res$cores, 1)
  expect_equal(res$exclusive, FALSE)
})
