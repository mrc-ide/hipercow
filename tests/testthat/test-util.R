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
  err <- expect_error(
    ensure_package("hermod.area51"),
    "Please install the 'hermod.area51' package")
  expect_equal(err$body,
               c(i = "Try at https://github.com/mrc-ide/hermod.area51"))
})
