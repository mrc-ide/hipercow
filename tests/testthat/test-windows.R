test_that("windows_path calls hermod.windows", {
  p <- getwd()
  expect_identical(
    windows_path("home", p, "//fi--san03/homes/bob", "Q:"),
    hermod.windows:::windows_path("home", p, "//fi--san03/homes/bob", "Q:"))
})


test_that("windows credentials passes through to dide_credentials", {
  mock_pkg <- list(dide_credentials = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_credentials, "ensure_package", mock_ensure_package)
  windows_credentials()

  mockery::expect_called(mock_ensure_package, 1)
  expect_equal(mockery::mock_args(mock_ensure_package)[[1]],
               list("hermod.windows"))

  mockery::expect_called(mock_pkg$dide_credentials, 1)
  expect_equal(mockery::mock_args(mock_pkg$dide_credentials)[[1]],
               list())
})
