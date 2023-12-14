test_that("windows_path calls hermod.windows", {
  mock_pkg <- list(windows_path = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_path, "ensure_package", mock_ensure_package)
  p <- getwd()
  windows_path("home", p, "//fi--san03/homes/bob", "Q:")

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hermod.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$windows_path, 1)
  expect_equal(mockery::mock_args(mock_pkg$windows_path)[[1]],
               list("home", p, "//fi--san03/homes/bob", "Q:"))
})


test_that("windows credentials passes through to dide_credentials", {
  mock_pkg <- list(dide_credentials = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_credentials, "ensure_package", mock_ensure_package)
  windows_credentials()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hermod.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$dide_credentials, 1)
  expect_equal(mockery::mock_args(mock_pkg$dide_credentials)[[1]],
               list())
})