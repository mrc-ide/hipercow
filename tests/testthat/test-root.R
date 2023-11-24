test_that("can initialise a hermod root", {
  path <- withr::local_tempfile()
  res1 <- testthat::evaluate_promise(hermod_init(path))
  expect_match(res1$messages[[1]], "Initialised hermod at '.+'")
  expect_match(res1$messages[[2]], "Next, call 'hermod_configure()",
               fixed = TRUE)

  res2 <- testthat::evaluate_promise(hermod_init(path))
  expect_match(res2$messages[[1]], "hermod already initialised at '.+'")
  expect_equal(res2$messages[[2]], res1$messages[[2]])

  expect_s3_class(res1$result, "hermod_root")
  expect_s3_class(res2$result, "hermod_root")
  path_norm <- normalizePath(path, "/")
  expect_equal(res1$result$path,
               list(root = path_norm,
                    tasks = file.path(path_norm, "hermod", "tasks"),
                    config = file.path(path_norm, "hermod", "config")))
  expect_identical(res1$result$path, res2$result$path)
  expect_identical(hermod_root(res1$result), res1$result)
})


test_that("Can locate a hermod root from a subdirectory", {
  path1 <- withr::local_tempfile()
  path2 <- file.path(path1, "a", "b", "c")
  dir.create(path2, FALSE, TRUE)
  r <- init_quietly(path1)
  expect_equal(hermod_root(path2)$path, r$path)
})


test_that("Error if root not found", {
  path <- withr::local_tempdir()
  expect_error(hermod_root(path))
})


test_that("Can create configuration", {
  path <- withr::local_tempfile()
  init_quietly(path)
  path <- normalize_path(path)

  mock_package <- list(
    make_configuration = mockery::mock(list(path = getwd())))
  mock_pkg <- mockery::mock(mock_package)
  mockery::stub(hermod_configure, "ensure_package", mock_pkg)

  hermod_configure("foo", a = 1, b = 2, root = path)

  mockery::expect_called(mock_pkg, 1)
  expect_equal(mockery::mock_args(mock_pkg)[[1]], list("hermod.foo"))

  mockery::expect_called(mock_package$make_configuration, 1)
  expect_equal(
    mockery::mock_args(mock_package$make_configuration)[[1]],
    list(a = 1, b = 2))

  expect_equal(hermod_config("foo", path), list(path = path))

  ## Force re-reading from disk
  rm(list = "roots", envir = cache)
  expect_equal(hermod_config("foo", path), list(path = path))
})


test_that("roots don't start with a configuration", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- init_quietly(path)
  err <- expect_error(
    hermod_config("foo", path),
    "This hermod root is not configured for driver 'foo'")
  expect_equal(err$body, c(i = "Please run 'hermod_configure(\"foo\", ...)'"))
})
