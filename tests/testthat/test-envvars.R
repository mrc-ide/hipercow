test_that("empty envvars is s3 object still", {
  e <- hipercow_envvars()
  expect_s3_class(e, c("hipercow_envvars", "data.frame"), exact = TRUE)
  expect_equal(names(e), c("name", "value", "secret"))
  expect_equal(e$name, character())
  expect_equal(e$value, character())
  expect_equal(e$secret, logical())
  expect_identical(e, make_envvars(character(), character(), logical()))
})


test_that("can collect environment variables", {
  expect_equal(hipercow_envvars(A = "x"),
               make_envvars("A", "x", FALSE))
  expect_equal(hipercow_envvars(A = "x", b = "y"),
               make_envvars(c("A", "b"), c("x", "y"), FALSE))
  expect_equal(hipercow_envvars(A = "x", b = "y", secret = TRUE),
               make_envvars(c("A", "b"), c("x", "y"), TRUE))
})


test_that("environment variables must be scalars", {
  expect_error(
    hipercow_envvars(A = "x", B = 1:2),
    "All arguments to 'hipercow_envvars' must be scalars")
})


test_that("coerce to character", {
  expect_equal(hipercow_envvars(A = "x", B = 1)$value, c("x", "1"))
})


test_that("look up unnamed envvars from the environment", {
  withr::local_envvar("HIPERCOW_ENVVAR_A" = "a",
                      "HIPERCOW_ENVVAR_B" = "b")
  expect_equal(
    hipercow_envvars(X = "x", "HIPERCOW_ENVVAR_A"),
    make_envvars(c("X", "HIPERCOW_ENVVAR_A"), c("x", "a"), FALSE))
  expect_equal(
    hipercow_envvars(X = "x", "HIPERCOW_ENVVAR_A", "HIPERCOW_ENVVAR_B"),
    make_envvars(c("X", "HIPERCOW_ENVVAR_A", "HIPERCOW_ENVVAR_B"),
                 c("x", "a", "b"),
                 FALSE))
  err <- expect_error(
    hipercow_envvars(X = "x", "HIPERCOW_ENVVAR_X", "HIPERCOW_ENVVAR_Y"),
    "Failed to look up environment variables for unnamed arguments")
  expect_equal(err$body, c("x" = "HIPERCOW_ENVVAR_X",
                           "x" = "HIPERCOW_ENVVAR_Y"))
})


test_that("can concatenate environment variables", {
  e1 <- hipercow_envvars(A = "1")
  e2 <- hipercow_envvars(X = "x", Y = "y")
  e3 <- hipercow_envvars(M = "m", N = "n", O = "o", secret = TRUE)
  expect_equal(c(e1, e2), hipercow_envvars(A = "1", X = "x", Y = "y"))
  expect_equal(c(e1, NULL, e2), hipercow_envvars(A = "1", X = "x", Y = "y"))

  e132 <- c(e1, e3, e2)
  expect_equal(e132$name, c(e1$name, e3$name, e2$name))
  expect_equal(e132$value, c(e1$value, e3$value, e2$value))
  expect_equal(e132$secret, c(e1$secret, e3$secret, e2$secret))

  expect_error(c(e1, "E" = 1),
               "Can't combine 'hipercow_envvars' objects and other objects")
})
