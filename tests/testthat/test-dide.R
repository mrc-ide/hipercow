test_that("dide_path calls hipercow.dide", {
  mock_pkg <- list(dide_path = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(dide_path, "ensure_package", mock_ensure_package)
  p <- getwd()
  dide_path(p, "//qdrive/homes/bob", "Q:")

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args, list("hipercow.dide"))

  mockery::expect_called(mock_pkg$dide_path, 1)
  args <- mockery::mock_args(mock_pkg$dide_path)[[1]]
  expect_equal(args[[1]], p)
  expect_equal(args[[2]], "//qdrive/homes/bob")
  expect_equal(args[[3]], "Q:")
  expect_type(args[[4]], "environment")
})


test_that("dide authenticate passes through to hipercow.dide", {
  mock_pkg <- list(dide_authenticate = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(dide_authenticate, "ensure_package", mock_ensure_package)
  dide_authenticate()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args, list("hipercow.dide"))

  mockery::expect_called(mock_pkg$dide_authenticate, 1)
  args <- mockery::mock_args(mock_pkg$dide_authenticate)[[1]]
  expect_length(args, 1)
  expect_type(args[[1]], "environment")
})


test_that("dide username passes through to hipercow.dide", {
  mock_pkg <- list(dide_username = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(dide_username, "ensure_package", mock_ensure_package)
  dide_username()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args, list("hipercow.dide"))

  mockery::expect_called(mock_pkg$dide_username, 1)
  args <- mockery::mock_args(mock_pkg$dide_username)[[1]]
  expect_length(args, 1)
  expect_type(args[[1]], "environment")
})


test_that("dide check passes through to hipercow.dide", {
  mock_pkg <- list(dide_check = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg)
  mockery::stub(dide_check, "ensure_package", mock_ensure_package)
  dide_check()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args[[1]], "hipercow.dide")

  mockery::expect_called(mock_pkg$dide_check, 1)
  args <- mockery::mock_args(mock_pkg$dide_check)[[1]]
  expect_length(args, 2)
  expect_type(args[[1]], "character")
  expect_type(args[[2]], "environment")
})


test_that("dide keypair passes through to hipercow.dide", {
  mock_pkg <- list(dide_generate_keypair = mockery::mock())
  mock_ensure_package <- mockery::mock(mock_pkg, cycle = TRUE)
  mockery::stub(dide_generate_keypair, "ensure_package", mock_ensure_package)

  dide_generate_keypair()

  mockery::expect_called(mock_ensure_package, 1)
  args <- mockery::mock_args(mock_ensure_package)[[1]]
  expect_equal(args, list("hipercow.dide"))

  mockery::expect_called(mock_pkg$dide_generate_keypair, 1)
  args <- mockery::mock_args(mock_pkg$dide_generate_keypair)[[1]]
  expect_equal(names(args)[1], "update")
  expect_equal(args[[1]], FALSE)
  expect_type(args[[2]], "environment")

  dide_generate_keypair(update = TRUE)
  mockery::expect_called(mock_pkg$dide_generate_keypair, 2)
  args <- mockery::mock_args(mock_pkg$dide_generate_keypair)[[2]]
  expect_equal(names(args)[1], "update")
  expect_equal(args[[1]], TRUE)
  expect_type(args[[2]], "environment")
})
