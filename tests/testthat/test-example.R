test_that("can run simple example", {
  owd <- getwd()
  expect_message(
    cleanup <- hipercow_example_helper(),
    "This example uses a special helper")
  cwd <- getwd()
  expect_false(cwd == owd)
  expect_equal(dir(), "hipercow")
  expect_true(is.function(cleanup))

  id <- suppressMessages(task_create_expr(sqrt(2)))
  expect_true(task_wait(id, timeout = 5))
  expect_equal(task_result(id), sqrt(2))

  expect_message(
    cleanup(),
    "Cleaning up example")
  expect_equal(getwd(), owd)
  expect_false(file.exists(cwd))
})
