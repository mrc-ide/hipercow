test_that("Validate parallel args", {
  expect_error(hipercow_parallel("potato"),
               "Parallel method potato unknown")

  res <- hipercow_parallel(NULL)
  expect_true(inherits(res, "hipercow_parallel"))
  expect_true(is.null(res$method))

  for (accepted in c("future", "parallel", "doParallel")) {
    res <- hipercow_parallel(accepted)
    expect_true(inherits(res, "hipercow_parallel"))
    expect_equal(res$method, accepted)
  }
})
