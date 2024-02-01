test_that("can send simple hello world task", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, action = "immediate",
                       root = path_here))
  mock_play <- mockery::mock()
  mockery::stub(hipercow_hello, "hipercow_speak", mock_play)

  res <- evaluate_promise(withVisible(
    withr::with_dir(path_here, hipercow_hello())))
  expect_equal(res$result$value, "Moo")
  expect_false(res$result$visible)
  expect_length(res$messages, 3)
  expect_match(res$messages[[1]], " -----\nMoooo")
  expect_match(res$messages[[2]], "Submitted task")
  expect_match(res$messages[[3]], "Successfully ran test task")
  mockery::expect_called(mock_play, 1)
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
  mock_play <- mockery::mock()
  mockery::stub(hipercow_hello, "task_log_watch", mock_watch)
  mockery::stub(hipercow_hello, "task_status", mock_status)
  mockery::stub(hipercow_hello, "task_result", mock_result)
  mockery::stub(hipercow_hello, "hipercow_speak", mock_play)

  res <- evaluate_promise(withVisible(
    withr::with_dir(path_here, hipercow_hello())))

  expect_equal(res$result,
               list(value = simpleError("Some error"), visible = FALSE))
  expect_length(res$messages, 5)
  expect_match(res$messages[[1]], " -----\nMoooo")
  expect_match(res$messages[[2]], "Submitted task")
  expect_match(res$messages[[3]], "Failed to run test task")
  expect_match(res$messages[[4]], "Task status is 'failure'")
  expect_match(res$messages[[5]], "Original error: Some error")
  mockery::expect_called(mock_play, 0)
})


test_that("driver can provide custom resources to hello", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  resources <- hipercow_resources(memory_per_process = 1, queue = "Tesco")
  cache$drivers$elsewhere$check_hello <- mockery::mock(resources)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, action = "immediate",
                       root = path_here))

  mock_play <- mockery::mock()
  mockery::stub(hipercow_hello, "hipercow_speak", mock_play)
  res <- evaluate_promise(withVisible(
    withr::with_dir(path_here, hipercow_hello())))

  mockery::expect_called(cache$drivers$elsewhere$check_hello, 1)
  mockery::expect_called(mock_play, 1)
  id <- dir(file.path(path_here, "hipercow", "tasks"))
  resources_used <- readRDS(
    file.path(path_here, "hipercow", "tasks", id, "resources"))
  expect_equal(resources, resources)
})
