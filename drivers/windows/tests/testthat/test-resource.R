test_that("Resource validation for windows", {
  res <- list(cores = 1L,
              exclusive = FALSE,
              runtime = NULL,
              hold_until = NULL,
              memory_per_node = NULL,
              memory_per_process = NULL,
              requested_nodes = NULL,
              priority = NULL,
              queue = NULL)
  class(res) <- "hipercow_resource"

  res2 <- windows_task_resources(res)
  expect_equal(res2$queue, "AllNodes")

  res$cores <- 999
  expect_error(windows_task_resources(res))

  res$cores <- 1
  res$memory_per_node <- 256
  expect_silent(windows_task_resources(res))
  res$memory_per_node <- 9999
  expect_error(windows_task_resources(res))
  res$memory_per_node <- NULL

  res$memory_per_process <- 256
  expect_silent(windows_task_resources(res))
  res$memory_per_process <- 9999
  expect_error(windows_task_resources(res))
  res$memory_per_process <- NULL

  res$queue <- "Potato"
  expect_error(windows_task_resources(res))
})

test_that("Validate memory strings", {
  expect_equal(interpret_memory(100), 100)
  expect_equal(interpret_memory("17Gb"), 17)
  expect_equal(interpret_memory("3Tb"), 3000)
})

