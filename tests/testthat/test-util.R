test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("can ensure we have a package", {
  ns <- ensure_package("testthat")
  expect_identical(ns, getNamespace("testthat"))
})


test_that("can fail if namespace not available", {
  withr::local_options(hermod.auto_install_missing_packages = FALSE)
  err <- expect_error(
    ensure_package("hermod.area51"),
    "Package 'hermod.area51' is not available")
  expect_length(err$body, 2)
  expect_match(
    err$body[[1]],
    "Please try installing 'hermod.area51' by running")
  expect_match(
    err$body[[2]],
    "To automatically install missing packages, set options")
})


test_that("can install missing packages if wanted", {
  withr::local_options(hermod.auto_install_missing_packages = NULL)

  mock_require_namespace <- mockery::mock(FALSE, TRUE)
  mock_install_packages <- mockery::mock()
  mock_get_namespace <- mockery::mock()
  mockery::stub(ensure_package, "requireNamespace", mock_require_namespace)
  mockery::stub(ensure_package, "utils::install.packages",
                mock_install_packages)
  mockery::stub(ensure_package, "getNamespace", mock_get_namespace)

  msg <- capture_messages(ensure_package("hermod.area51"))
  expect_match(msg, "Trying to install 'hermod.area51'", all = FALSE)
  expect_match(msg, "To prevent this, set options", all = FALSE)
  expect_match(msg, "Installation of 'hermod.area51' successful", all = FALSE)

  mockery::expect_called(mock_require_namespace, 2)
  expect_equal(mockery::mock_args(mock_require_namespace),
               rep(list(list("hermod.area51", quietly = TRUE)), 2))

  mockery::expect_called(mock_install_packages, 1)
  expect_equal(
    mockery::mock_args(mock_install_packages)[[1]],
    list("hermod.area51", repos = c("https://mrc-ide.r-universe.dev",
                                    CRAN = "https://cloud.r-project.org")))

  mockery::expect_called(mock_get_namespace, 1)
  expect_equal(mockery::mock_args(mock_get_namespace),
               list(list("hermod.area51")))
})


test_that("can error if missing package installation fails", {
  withr::local_options(hermod.auto_install_missing_packages = NULL)

  mock_require_namespace <- mockery::mock(FALSE, FALSE)
  mock_install_packages <- mockery::mock()
  mock_get_namespace <- mockery::mock()
  mockery::stub(ensure_package, "requireNamespace", mock_require_namespace)
  mockery::stub(ensure_package, "utils::install.packages",
                mock_install_packages)
  mockery::stub(ensure_package, "getNamespace", mock_get_namespace)

  err <- expect_error(
    suppressMessages(ensure_package("hermod.area51")),
    "Installation of 'hermod.area51' failed!")
  expect_length(err$body, 1)
  expect_match(err$body, "Please try installing 'hermod.area51' by running")

  mockery::expect_called(mock_require_namespace, 2)
  expect_equal(mockery::mock_args(mock_require_namespace),
               rep(list(list("hermod.area51", quietly = TRUE)), 2))

  mockery::expect_called(mock_install_packages, 1)
  expect_equal(
    mockery::mock_args(mock_install_packages)[[1]],
    list("hermod.area51", repos = c("https://mrc-ide.r-universe.dev",
                                    CRAN = "https://cloud.r-project.org")))

  mockery::expect_called(mock_get_namespace, 0)
})
