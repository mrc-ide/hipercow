test_that("empty envvars is s3 object still", {
  expect_equal(envvars(),
               structure(character(), names = character(), class = "envvars"))
})


test_that("can collect environment variables", {
  expect_equal(envvars(A = "x"),
               structure(c(A = "x"), class = "envvars"))
  expect_equal(envvars(A = "x", b = "y"),
               structure(c(A = "x", b = "y"), class = "envvars"))
})


test_that("environment variables must be strings", {
  expect_error(
    envvars(A = "x", B = 1),
    "All arguments to 'envvars' must be strings")
})


test_that("look up unnamed envvars from the environment", {
  withr::local_envvar("HIPERCOW_ENVVAR_A" = "a",
                      "HIPERCOW_ENVVAR_B" = "b")
  expect_equal(envvars(X = "x", "HIPERCOW_ENVVAR_A"),
               structure(c(X = "x", HIPERCOW_ENVVAR_A = "a"),
                         class = "envvars"))
  expect_equal(envvars(X = "x", "HIPERCOW_ENVVAR_A", "HIPERCOW_ENVVAR_B"),
               structure(c(X = "x",
                           HIPERCOW_ENVVAR_A = "a",
                           HIPERCOW_ENVVAR_B = "b"),
                         class = "envvars"))
  err <- expect_error(
    envvars(X = "x", "HIPERCOW_ENVVAR_X", "HIPERCOW_ENVVAR_Y"),
    "Failed to look up environment variables for unnamed arguments")
  expect_equal(err$body, c("x" = "HIPERCOW_ENVVAR_X",
                           "x" = "HIPERCOW_ENVVAR_Y"))
})


test_that("can concatenate environment variables", {
  e1 <- envvars(A = "1")
  e2 <- envvars(X = "x", Y = "y")
  e3 <- envvars(M = "m", N = "n", O = "o")
  expect_equal(c(e1, e2), envvars(A = "1", X = "x", Y = "y"))
  expect_equal(c(e1, e2, e3),
               envvars(A = "1", X = "x", Y = "y", M = "m", N = "n", O = "o"))
  expect_equal(c(e1, NULL, e2), envvars(A = "1", X = "x", Y = "y"))

  expect_equal(c(e1, unclass(e2)),
               envvars(A = "1", X = "x", Y = "y"))
  expect_equal(c(e1, unclass(e2), unclass(e3)),
               envvars(A = "1", X = "x", Y = "y", M = "m", N = "n", O = "o"))
})
