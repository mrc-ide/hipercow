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

test_that("Can read cores from environment", {
  withr::with_envvar(new = c(
    "HIPERCOW_CORES_VARIABLE_NAME" = "hpc_core_count",
    "hpc_core_count" = 13), {
      expect_equal(hipercow_get_cores(), 13)
  })
})

test_that("Can do a parallel setup", {
  expect_equal(parallel_setup(NULL), NULL)
  expect_true(grepl("future::plan.*", parallel_setup("future")))
  expect_true(grepl("parallel::make.*", parallel_setup("parallel")))
  expect_true(grepl("doParallel::reg.*", parallel_setup("doParallel")))
  expect_error(parallel_setup("cactus"), "Unknown method cactus")
})
