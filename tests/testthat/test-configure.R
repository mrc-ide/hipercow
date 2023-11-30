test_that("Can create configuration", {
  skip("rework")
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

  expect_equal(hermod_root(path)$drivers$foo, list(path = path))

  ## Force re-reading from disk
  rm(list = "roots", envir = cache)
  expect_equal(hermod_root(path)$drivers$foo, list(path = path))
})


test_that("roots don't start with a configuration", {
  skip("rework")
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- init_quietly(path)
  err <- expect_error(
    hermod_driver_select("foo", path),
    "No hermod driver configured")
  expect_equal(err$body, c(i = "Please run 'hermod_configure(\"foo\", ...)'"))
})
x
