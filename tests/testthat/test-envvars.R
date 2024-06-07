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


test_that("can unset environment variables by setting them as NA", {
  e1 <- hipercow_envvars(A = "1", B = "2", C = "3", D = "4")
  expect_equal(
    c(e1, hipercow_envvars(A = "10", B = NA)),
    hipercow_envvars(C = "3", D = "4", A = "10"))
   expect_equal(
    c(e1,
      hipercow_envvars(A = "10", B = NA),
      hipercow_envvars(B = "20")),
    hipercow_envvars(C = "3", D = "4", A = "10", B = "20"))
})


test_that("can override environment variables when concatenating", {
  e1 <- hipercow_envvars(A = "1", B = "x")
  e2 <- hipercow_envvars(A = "2")
  e3 <- hipercow_envvars(A = "2", secret = TRUE)
  expect_equal(c(e1, e2), hipercow_envvars(B = "x", A = "2"))
  expect_equal(c(e1, e3), c(hipercow_envvars(B = "x"), e3))
})


test_that("can encrypt envvars", {
  path_root <- withr::local_tempdir()
  pair <- elsewhere_keypair(NULL, path_root)

  envvars <- c(
    hipercow_envvars("MY_SECRET" = "s3cre7", secret = TRUE),
    hipercow_envvars("MY_ENVVAR" = "hello"),
    hipercow_envvars("MY_OTHER_SECRET" = "password", secret = TRUE))

  res <- encrypt(envvars, pair)
  expect_equal(res$value == envvars$value, !envvars$secret)
  expect_equal(attr(res, "key"), pair$key)
  expect_equal(decrypt(res), envvars)
})


test_that("applying envvars unpacks them locally into an environment", {
  envvars <- hipercow_envvars(MY_ENVVAR = "hello")
  local({
    envir <- environment()
    envvars_apply(envvars, envir)
    expect_equal(Sys.getenv("MY_ENVVAR"), "hello")
  })
  expect_equal(Sys.getenv("MY_ENVVAR"), "")
})


test_that("applying NULL envvars does not set anything", {
  cmp <- Sys.getenv()
  local({
    envir <- environment()
    envvars_apply(NULL, envir)
    expect_equal(Sys.getenv(), cmp)
  })
})


test_that("dont load driver if no secrets present", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)
  e <- hipercow_envvars(MY_ENVVAR = "hello")
  expect_equal(prepare_envvars(NULL, NULL, root),
               DEFAULT_ENVVARS)
  expect_equal(prepare_envvars(e, NULL, root),
               c(DEFAULT_ENVVARS, e))
})


test_that("error if user tries to use encrypted envvars without driver", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)
  e <- hipercow_envvars(MY_SECRET = "secret", secret = TRUE)
  expect_error(
    prepare_envvars(e, NULL, root),
    "No driver configured, so cannot work with secret environment variables")
  suppressMessages(hipercow_configure("example", root = root))
  expect_error(
    prepare_envvars(e, NULL, root),
    "No driver selected, so cannot work with secret environment variables")
})


test_that("environment variables can be exported", {
  path <- withr::local_tempfile()

  e <- hipercow_envvars(A = "x", B = "y")

  envvars_export(e, path)
  expect_equal(readLines(path), c("A=x", "B=y"))
})


test_that("secret environment variables are not exported", {
  path <- withr::local_tempfile()

  e <- c(hipercow_envvars(A = "x"),
         hipercow_envvars(B = "y", secret = TRUE))

  envvars_export(e, path)
  expect_equal(readLines(path), c("A=x"))
})


test_that("Default environment variables are applied", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)

  expect_equal(
    prepare_envvars(NULL, NULL, root),
    DEFAULT_ENVVARS)

  expect_equal(
    prepare_envvars(hipercow_envvars(B = "y"), NULL, root),
    c(DEFAULT_ENVVARS, hipercow_envvars(B = "y")))

  withr::local_options(
    hipercow.default_envvars = hipercow_envvars(A = "x"))

  expect_equal(
    prepare_envvars(NULL, NULL, root),
    c(DEFAULT_ENVVARS, hipercow_envvars(A = "x")))

  expect_equal(
    prepare_envvars(hipercow_envvars(B = "y"), NULL, root),
    c(DEFAULT_ENVVARS, hipercow_envvars(A = "x", B = "y")))

  expect_equal(
    prepare_envvars(hipercow_envvars(A = "y"), NULL, root),
    c(DEFAULT_ENVVARS, hipercow_envvars(A = "y")))
})


test_that("Has built-in default variables", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)

  withr::local_options(
    hipercow.default_envvars = NULL)

  expect_equal(
    prepare_envvars(NULL, NULL, root),
    DEFAULT_ENVVARS)

  expect_equal(envvars_combine(NULL, NULL), DEFAULT_ENVVARS)
})


test_that("overlay driver envvars with defaults", {
  withr::local_options(
    hipercow.default_envvars =
      hipercow_envvars(ENV1 = "a", ENV2 = "b", ENV3 = "c"))
  expect_equal(
    envvars_combine(NULL, NULL),
    c(DEFAULT_ENVVARS,
      hipercow_envvars(ENV1 = "a", ENV2 = "b", ENV3 = "c")))
  expect_equal(
    envvars_combine(hipercow_envvars(ENV1 = "A", ENV4 = "d"), NULL),
    c(DEFAULT_ENVVARS,
      hipercow_envvars(ENV4 = "d", ENV1 = "a", ENV2 = "b", ENV3 = "c")))
  expect_equal(
    envvars_combine(hipercow_envvars(ENV1 = "A", ENV4 = "D", ENV5 = "E"),
                    hipercow_envvars(ENV1 = "AA", ENV5 = "EE", ENV6 = "FF")),
    c(DEFAULT_ENVVARS,
      hipercow_envvars(ENV4 = "D", ENV2 = "b", ENV3 = "c",
                       ENV1 = "AA", ENV5 = "EE", ENV6 = "FF")))
  expect_equal(
    envvars_combine(NULL,
                    hipercow_envvars(ENV1 = "AA", ENV5 = "EE", ENV6 = "FF")),
    c(DEFAULT_ENVVARS,
      hipercow_envvars(ENV2 = "b", ENV3 = "c",
                       ENV1 = "AA", ENV5 = "EE", ENV6 = "FF")))
})
