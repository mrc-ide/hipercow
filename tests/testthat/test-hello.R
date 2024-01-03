test_that("can send simple hello world task", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, action = "immediate",
                       root = path_here))
  res <- evaluate_promise(withVisible(
    withr::with_dir(path_here, hipercow_hello())))
  expect_s3_class(res$result$value, "sessionInfo")
  expect_false(res$result$visible)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Submitted task")
  expect_match(res$messages[[2]], "Successfully ran test task")
})


test_that("don't send test task if windows not configured correctly", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)
  root$config <- list(windows = NULL)
  mock_check <- mockery::mock(FALSE)
  mockery::stub(hipercow_hello, "windows_check", mock_check)
  res <- evaluate_promise(withVisible(
    withr::with_dir(path, hipercow_hello())))
  expect_equal(res$result, list(value = NULL, visible = FALSE))
  expect_length(res$messages, 1)
  expect_match(res$messages, "Can't send test task")
})


test_that("can recover from failure in hello task", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, action = "immediate",
                       root = path_here))

  mock_watch <- mockery::mock(FALSE)
  mock_status <- mockery::mock("failure")
  mock_result <- mockery::mock(simpleError("Some error"))
  mockery::stub(hipercow_hello, "task_log_watch", mock_watch)
  mockery::stub(hipercow_hello, "task_status", mock_status)
  mockery::stub(hipercow_hello, "task_result", mock_result)

  res <- evaluate_promise(withVisible(
    withr::with_dir(path_here, hipercow_hello())))

  expect_equal(res$result,
               list(value = simpleError("Some error"), visible = FALSE))
  expect_length(res$messages, 4)
  expect_match(res$messages[[1]], "Submitted task")
  expect_match(res$messages[[2]], "Failed to run test task")
  expect_match(res$messages[[3]], "Task status is 'failure'")
  expect_match(res$messages[[4]], "Original error: Some error")
})
