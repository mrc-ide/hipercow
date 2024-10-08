test_that("windows_path calls hipercow.windows", {
  mock_pkg <- list(windows_path = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_path, "ensure_package", mock_ensure_package)
  p <- getwd()
  windows_path(p, "//qdrive/homes/bob", "Q:")

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hipercow.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$windows_path, 1)
  args <- mockery::mock_args(mock_pkg$windows_path)[[1]]
  expect_equal(args[[1]], p)
  expect_equal(args[[2]], "//qdrive/homes/bob")
  expect_equal(args[[3]], "Q:")
  expect_type(args[[4]], "environment")
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
  args <- mockery::mock_args(mock_pkg$windows_authenticate)[[1]]
  expect_length(args, 1)
  expect_type(args[[1]], "environment")
})


test_that("windows username passes through to hipercow.windows", {
  mock_pkg <- list(windows_username = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_username, "ensure_package", mock_ensure_package)
  windows_username()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hipercow.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$windows_username, 1)
  args <- mockery::mock_args(mock_pkg$windows_username)[[1]]
  expect_length(args, 1)
  expect_type(args[[1]], "environment")
})


test_that("windows check passes through to hipercow.windows", {
  mock_pkg <- list(windows_check = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(windows_check, "ensure_package", mock_ensure_package)
  windows_check()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hipercow.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$windows_check, 1)
  args <- mockery::mock_args(mock_pkg$windows_check)[[1]]
  expect_length(args, 2)
  expect_type(args[[1]], "character")
  expect_type(args[[2]], "environment")
})


test_that("windows keypair passes through to hipercow.windows", {
  mock_pkg <- list(windows_generate_keypair = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg, cycle = TRUE)
  mockery::stub(windows_generate_keypair, "ensure_package", mock_ensure_package)

  windows_generate_keypair()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hipercow.windows")
  expect_type(args[[2]], "environment")

  mockery::expect_called(mock_pkg$windows_generate_keypair, 1)
  args <- mockery::mock_args(mock_pkg$windows_generate_keypair)[[1]]
  expect_equal(names(args)[1], "update")
  expect_equal(args[[1]], FALSE)
  expect_type(args[[2]], "environment")

  windows_generate_keypair(update = TRUE)
  mockery::expect_called(mock_pkg$windows_generate_keypair, 2)
  args <- mockery::mock_args(mock_pkg$windows_generate_keypair)[[2]]
  expect_equal(names(args)[1], "update")
  expect_equal(args[[1]], TRUE)
  expect_type(args[[2]], "environment")
})
