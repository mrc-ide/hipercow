test_that("Resource validation for windows", {
  res <- hipercow::hipercow_resource()
  expect_silent(windows_validate_resources(res))
  res$cores <- 999
  expect_error(windows_validate_resources(res))
  
  res$cores <- 1
  res$memory_per_node <- 256
  expect_silent(windows_validate_resources(res))
  res$memory_per_node <- 9999
  expect_error(windows_validate_resources(res))
  res$memory_per_node <- NULL
  
  res$memory_per_process <- 256
  expect_silent(windows_validate_resources(res))
  res$memory_per_process <- 9999
  expect_error(windows_validate_resources(res))
  res$memory_per_process <- NULL
  
  res$queue <- NULL
  res <- windows_validate_resources(res)
  expect_equal(res$queue, "AllNodes")
  
  res$queue <- "Potato"
  expect_error(windows_validate_resources(res))
})

test_that("Can create resources", {
  res <- windows_resources()
  expect_equal(res$cores, 1L)
  expect_false(res$exclusive)
  expect_equal(res$queue, "AllNodes")
  
  res <- windows_resources(2, TRUE)
  expect_equal(res$cores, 2L)
  expect_false(res$exclusive)
  expect_equal(res$queue, "AllNodes")
  
})
  