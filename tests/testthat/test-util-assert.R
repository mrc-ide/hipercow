test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_silent(assert_character("a"))
  expect_error(assert_character(1), "must be a character")
  expect_error(assert_character(TRUE), "must be a character")
})


test_that("assert_logical", {
  expect_silent(assert_logical(TRUE))
  expect_error(assert_logical(1), "must be logical")
  expect_error(assert_logical("a"), "must be logical")
})


test_that("assert_integer", {
  expect_silent(assert_scalar_integer(1L))
  expect_silent(assert_scalar_integer(1))
  expect_error(assert_scalar_integer(1.1),
               "must be an integer")

  expect_silent(assert_integer(c(1, 2, 3)))
  expect_error(assert_integer(c("1", "a", "3")))
})
