test_that("windows_path calls hipercow.windows", {
  mock_pkg <- list(windows_path = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_path, "ensure_package", mock_ensure_package)
  p <- getwd()
  windows_path(p, "//fi--san03/homes/bob", "Q:")

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hipercow.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$windows_path, 1)
  expect_equal(mockery::mock_args(mock_pkg$windows_path)[[1]],
               list(p, "//fi--san03/homes/bob", "Q:"))
})


test_that("windows authenticate passes through to hipercow.windows", {
  mock_pkg <- list(windows_authenticate = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_authenticate, "ensure_package", mock_ensure_package)
  windows_authenticate()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hipercow.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$windows_authenticate, 1)
  expect_equal(mockery::mock_args(mock_pkg$windows_authenticate)[[1]],
               list())
})
