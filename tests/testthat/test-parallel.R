test_that("Validate parallel args", {
  expect_error(hipercow_parallel("potato"),
               "Parallel method potato unknown")

  res <- hipercow_parallel(NULL)
  expect_true(inherits(res, "hipercow_parallel"))
  expect_true(is.null(res$method))

  for (accepted in c("future", "parallel")) {
    res <- hipercow_parallel(accepted)
    expect_true(inherits(res, "hipercow_parallel"))
    expect_equal(res$method, accepted)
  }
})


test_that("Can read cores from environment", {
  withr::with_envvar(new = c(
    "HIPERCOW_CORES" = 13), {
      expect_equal(hipercow_parallel_get_cores(), 13)
  })
})


test_that("Increasing core count gives a friendly warning", {
  hipercow_parallel_set_cores(1)
  res <- evaluate_promise(hipercow_parallel_set_cores(2))
  expect_true(grepl("(.*)increasing cores alone(.*)", res$messages))
  expect_silent(hipercow_parallel_set_cores(NA))
})


test_that("Parallel setup with NA cores", {
  mock_get_cores <- mockery::mock(NA)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)
  expect_error(hipercow_parallel_setup("future"),
               "Couldn't find HIPERCOW_CORES")
})


test_that("Can do NULL parallel setup", {
  expect_equal(hipercow_parallel_setup(NULL), NULL)
})


test_that("Can do parallel setup for future", {
  mock_get_cores <- mockery::mock(4, cycle = TRUE)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)

  mock_parallel_setup_future <- mockery::mock()
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_setup_future",
                mock_parallel_setup_future)

  hipercow_parallel_setup("future")
  mockery::expect_called(mock_parallel_setup_future, 1)
})


test_that("Can do parallel setup for parallel", {
  mock_get_cores <- mockery::mock(4, cycle = TRUE)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)

  mock_parallel_setup_parallel <- mockery::mock()
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_setup_parallel",
                mock_parallel_setup_parallel)

  hipercow_parallel_setup("parallel")
  mockery::expect_called(mock_parallel_setup_parallel, 1)
})


test_that("Parallel setup unknown method", {
  mock_get_cores <- mockery::mock(4, cycle = TRUE)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)
  expect_error(hipercow_parallel_setup("cactus"),
               "Unknown method cactus")
})

test_that("Can setup future cluster", {
  mock_plan <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_future,
                "future::plan", mock_plan)

  suppressWarnings(hipercow_parallel_set_cores(1))
  suppressMessages(hipercow_parallel_setup_future(4))
  expect_equal(mockery::mock_args(mock_plan)[[1]]$workers, 4)
  mockery::expect_called(mock_plan, 1)
})

test_that("Can setup parallel cluster", {
  mock_make_cluster <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::makeCluster", mock_make_cluster)

  mock_def_cluster <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::setDefaultCluster", mock_def_cluster)

  mock_cluster_call <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::clusterCall", mock_cluster_call)


  suppressMessages(hipercow_parallel_setup_parallel(4))
  mockery::expect_called(mock_make_cluster, 1)
  mockery::expect_called(mock_def_cluster, 1)
  mockery::expect_called(mock_cluster_call, 2)

  expect_equal(mockery::mock_args(mock_make_cluster)[[1]]$spec, 4)
})

test_that("Can set cores and environment variables", {
  env <- new.env()
  suppressWarnings(hipercow_parallel_set_cores(1))
  withr::with_environment(env = env, {
     hipercow_parallel_set_cores(4)
     expect_equal(Sys.getenv("MC_CORES"), "4")
  })
})

