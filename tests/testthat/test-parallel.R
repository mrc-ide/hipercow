test_that("Validate parallel args", {
  expect_error(hipercow_parallel("potato"),
               "Parallel method 'potato' unknown")

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
  expect_error(hipercow_parallel_set_cores(NA),
               "'cores' must be a positive integer")
  expect_error(hipercow_parallel_set_cores(-1),
               "'cores' must be a positive integer")
})


test_that("Parallel setup with NA cores", {
  mock_get_cores <- mockery::mock(NA)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)
  expect_error(hipercow_parallel_setup(hipercow_parallel("future")),
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

  hipercow_parallel_setup(hipercow_parallel("future"))
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

  hipercow_parallel_setup(hipercow_parallel("parallel"))
  mockery::expect_called(mock_parallel_setup_parallel, 1)
})


test_that("Parallel setup unknown method", {
  mock_get_cores <- mockery::mock(4, cycle = TRUE)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)
  expect_error(hipercow_parallel("cactus"),
               "Parallel method 'cactus' unknown.")
})

test_that("Can setup future cluster", {
  mock_plan <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_future,
                "future::plan", mock_plan)

  hipercow_parallel_set_cores(1, environment())
  suppressMessages(hipercow_parallel_setup_future(4, 1, "default"))
  mockery::expect_called(mock_plan, 1)
  args <- mockery::mock_args(mock_plan)[[1]]
  expect_equal(args[[1]], future::multisession)
  expect_equal(args$workers, 4)
  expect_equal(
    args$rscript_startup,
    paste0('hipercow::hipercow_parallel_set_cores(1)\n',
           'hipercow::hipercow_parallel_load_environment("default")\n'))
})

test_that("Can setup parallel cluster", {
  cl <- new.env() # Some singleton
  mock_make_cluster <- mockery::mock(cl)
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::makeCluster", mock_make_cluster)

  mock_def_cluster <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::setDefaultCluster", mock_def_cluster)

  mock_cluster_call <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::clusterCall", mock_cluster_call)


  suppressMessages(hipercow_parallel_setup_parallel(4, 1, "default"))
  mockery::expect_called(mock_make_cluster, 1)
  mockery::expect_called(mock_def_cluster, 1)
  mockery::expect_called(mock_cluster_call, 3)

  expect_equal(mockery::mock_args(mock_make_cluster)[[1]], list(spec = 4))
  expect_equal(mockery::mock_args(mock_def_cluster)[[1]], list(cl))

  expect_equal(
    mockery::mock_args(mock_cluster_call)[[1]],
    list(cl, .libPaths, .libPaths()))
  expect_equal(
    mockery::mock_args(mock_cluster_call)[[2]],
    list(cl, hipercow::hipercow_parallel_set_cores, 1))
  expect_equal(
    mockery::mock_args(mock_cluster_call)[[3]],
    list(cl, hipercow::hipercow_parallel_load_environment, "default"))
})

test_that("Can set cores and environment variables", {
  env <- new.env()
  suppressMessages(hipercow_parallel_set_cores(1))
  withr::with_environment(env = env, {
    suppressMessages(hipercow_parallel_set_cores(4))
    expect_equal(Sys.getenv("MC_CORES"), "4")
  })
})


test_that("can print default parallel control", {
  x <- hipercow_parallel()
  res <- evaluate_promise(print(x))
  expect_match(res$messages, "hipercow parallel control (hipercow_parallel)",
               all = FALSE, fixed = TRUE)
  expect_match(res$messages, "Unset: 'method'",
               all = FALSE, fixed = TRUE)
})


test_that("can print parallel control", {
  x <- hipercow_parallel("parallel")
  res <- evaluate_promise(print(x))
  expect_match(res$messages, "hipercow parallel control (hipercow_parallel)",
               all = FALSE, fixed = TRUE)
  expect_match(res$messages, "method: parallel",
               all = FALSE, fixed = TRUE)
})


test_that("Can't run parallel with 1 core", {
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  expect_error(
    withr::with_dir(path, suppressMessages(
      task_create_expr(sqrt(2),
        resources = hipercow_resources(cores = 1),
        parallel = hipercow_parallel(method = "future")))),
    "You chose parallel method 'future', with 1 core")
})


test_that("validate cores_per_process", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)

  expect_error(hipercow_parallel("future", 1.5),
               "'cores_per_process' must be a positive integer")

  expect_error(
    parallel_validate(hipercow_parallel("future", 5), 4, "default",
                      root = root),
    "You chose 5 cores per process, but requested only 4 cores in total")

  expect_error(
    parallel_validate(hipercow_parallel(NULL, 4), 4, "default", root = root),
    "You chose 4 cores per process, but no parallel method is set")

  expect_no_error(
    parallel_validate(hipercow_parallel(NULL), 1, "default", root = root))

  expect_no_error(parallel_validate(NULL, 1, "default", root = root))
})



test_that("Parallel setup unknown method", {
  mock_get_cores <- mockery::mock(4, cycle = TRUE)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)
  expect_error(hipercow_parallel("cactus"),
               "Parallel method 'cactus' unknown.")
})

test_that("Can setup future cluster with multi core per process", {
  mock_plan <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_future,
                "future::plan", mock_plan)

  suppressMessages(hipercow_parallel_setup_future(3, 2, "special"))
  mockery::expect_called(mock_plan, 1)
  args <- mockery::mock_args(mock_plan)[[1]]
  expect_equal(args$workers, 3)
  expect_equal(
    args$rscript_startup,
    paste0('hipercow::hipercow_parallel_set_cores(2)\n',
           'hipercow::hipercow_parallel_load_environment("special")\n'))
})

test_that("Can setup parallel cluster with multi core per process", {
  cl <- new.env() # Some singleton
  mock_make_cluster <- mockery::mock(cl)
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::makeCluster", mock_make_cluster)

  mock_def_cluster <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::setDefaultCluster", mock_def_cluster)

  mock_cluster_call <- mockery::mock()
  mockery::stub(hipercow_parallel_setup_parallel,
                "parallel::clusterCall", mock_cluster_call)

  suppressMessages(hipercow_parallel_setup_parallel(3, 2, "special"))

  expect_equal(mockery::mock_args(mock_make_cluster)[[1]], list(spec = 3))
  expect_equal(mockery::mock_args(mock_def_cluster)[[1]], list(cl))

  expect_equal(
    mockery::mock_args(mock_cluster_call)[[1]],
    list(cl, .libPaths, .libPaths()))
  expect_equal(
    mockery::mock_args(mock_cluster_call)[[2]],
    list(cl, hipercow::hipercow_parallel_set_cores, 2))
  expect_equal(
    mockery::mock_args(mock_cluster_call)[[3]],
    list(cl, hipercow::hipercow_parallel_load_environment, "special"))
})

test_that("Warning on idle cores with multi core per process", {
  mock_make_cluster <- mockery::mock()
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_setup_future", mock_make_cluster)

  mock_get_cores <- mockery::mock(32)
  mockery::stub(hipercow_parallel_setup,
                "hipercow_parallel_get_cores", mock_get_cores)

  expect_message(hipercow_parallel_setup(hipercow_parallel("future", 7)),
    paste0("Running 7 cores per process leaves ",
           "4 unallocated cores on a 32-core task"))
  mockery::expect_called(mock_make_cluster, 1)
  mockery::expect_called(mock_get_cores, 1)
})


test_that("can specify environment in parallel setup", {
  p <- hipercow_parallel()
  expect_null(p$environment)
  p <- hipercow_parallel(method = "parallel", environment = "empty")
  expect_equal(p$environment, "empty")
  p <- hipercow_parallel(method = "parallel", environment = "other")
  expect_equal(p$environment, "other")
})


test_that("can set environment on validation", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  expect_error(
    parallel_validate(hipercow_parallel(NULL), 1, "foo", root = root),
    "Environment 'foo' does not exist")
  expect_equal(
    parallel_validate(hipercow_parallel(NULL), 1, "empty", root = root),
    hipercow_parallel(NULL, environment = "empty"))
  expect_equal(
    parallel_validate(hipercow_parallel(NULL, environment = "empty"),
                      1, "default", root = root),
    hipercow_parallel(NULL, environment = "empty"))
})


test_that("can load environment from high-level function", {
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  writeLines("a <- 10", file.path(path, "script.R"))
  suppressMessages(
    hipercow_environment_create(sources = "script.R", root = path))
  e <- new.env()
  withr::with_dir(path, hipercow_parallel_load_environment("default", e))
  expect_equal(e$a, 10)
})
