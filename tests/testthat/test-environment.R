test_that("can construct an environment", {
  env <- new_environment("foo", NULL, NULL)
  expect_s3_class(env, "hipercow_environment")
  expect_setequal(names(env), c("name", "packages", "sources"))
  expect_equal(env$name, "foo")
  expect_null(env$packages)
  expect_null(env$sources)
})


test_that("can construct a nontrivial environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  file.create(file.path(path, c("a.R", "b.R")))
  env <- new_environment("foo", c("a.R", "b.R"), c("x", "y", "z"), root)
  expect_equal(env$name, "foo")
  expect_equal(env$sources, c("a.R", "b.R"))
  expect_equal(env$packages, c("x", "y", "z"))
})


test_that("can print empty environments", {
  testthat::local_reproducible_output()
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  ## I tried to use testthat/cli snapshot testing here, but it really
  ## did not work for me, giving different output on CI with no
  ## indication of what was different.
  msg <- capture_messages(
    hipercow_environment_show("default", root))
  expect_match(msg, "hipercow environment 'default'", all = FALSE)
  expect_match(msg, "packages: (none)", all = FALSE, fixed = TRUE)
  expect_match(msg, "sources: (none)", all = FALSE, fixed = TRUE)
})


test_that("can print nontrivial environments", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  file.create(file.path(path, c("a.R", "b.R", "c.R")))
  suppressMessages(
    hipercow_environment_create("foo",
                              packages = c("x", "y", "z"),
                              sources = c("a.R", "b.R", "c.R"),
                              root = path))
  msg <- capture_messages(
    hipercow_environment_show("foo", root))
  expect_match(msg, "hipercow environment 'foo'", all = FALSE)
  expect_match(msg, "packages: x, y, z", all = FALSE, fixed = TRUE)
  expect_match(msg, "sources: a.R, b.R, c.R", all = FALSE, fixed = TRUE)
})


test_that("error if loading unknown environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  err <- expect_error(
    hipercow_environment_show("foo", path),
    "Environment 'foo' does not exist")
  expect_equal(err$body, c(i = "Valid options are: 'default'"))
})


test_that("can create environment in root", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs <- c("x", "y")
  expect_message(
    hipercow_environment_create(packages = pkgs, root = path),
    "Created environment 'default'")
  expect_equal(environment_load("default", root),
               new_environment("default", NULL, pkgs, root))
})


test_that("can update existing environment in root", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs1 <- c("x", "y")
  pkgs2 <- c("x", "y", "z")
  expect_false(hipercow_environment_exists("foo", path))
  expect_equal(hipercow_environment_list(path), "default")
  expect_message(
    hipercow_environment_create(name = "foo", packages = pkgs1, root = path),
    "Created environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs1, root))
  expect_message(
    hipercow_environment_create(name = "foo", packages = pkgs2, root = path),
    "Updated environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs2, root))
  expect_message(
    hipercow_environment_create(name = "foo", packages = pkgs2, root = path),
    "Environment 'foo' is unchanged")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs2, root))
  expect_true(hipercow_environment_exists("foo", path))
  expect_equal(hipercow_environment_list(path), c("default", "foo"))
})


test_that("can prevent overwriting of an environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs1 <- c("x", "y")
  pkgs2 <- c("x", "y", "z")
  expect_message(
    hipercow_environment_create(name = "foo", packages = pkgs1,
                              overwrite = FALSE, root = path),
    "Created environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs1, root))
  expect_error(
    hipercow_environment_create(name = "foo", packages = pkgs2,
                              overwrite = FALSE, root = path),
    "Environment 'foo' already exists and 'overwrite' is FALSE")
})


test_that("creating initial default does not count as overwriting", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs <- c("x", "y")
  expect_equal(environment_load("default", root),
               new_environment("default", NULL, NULL, root))
  expect_message(
    hipercow_environment_create(name = "default", packages = pkgs,
                              overwrite = FALSE, root = path),
    "Created environment 'default'")
  expect_message(
    hipercow_environment_create(name = "default", packages = pkgs,
                              overwrite = FALSE, root = path),
    "Environment 'default' is unchanged")
  expect_error(
    hipercow_environment_create(name = "default", packages = NULL,
                              overwrite = FALSE, root = path),
    "Environment 'default' already exists and 'overwrite' is FALSE")
})


test_that("sources must exist within the root", {
  path <- withr::local_tempfile()
  srcs <- c("a.R", "b.R")
  root <- init_quietly(path)

  err <- expect_error(
    hipercow_environment_create(name = "default", sources = srcs, root = path),
    "Files in 'sources' not found: 'a.R' and 'b.R'")
  expect_equal(err$body,
               c(i = sprintf("Looking relative to '%s'", root$path$root)))

  file.create(file.path(path, srcs[[1]]))
  expect_error(
    hipercow_environment_create(name = "default", sources = srcs, root = path),
    "File in 'sources' not found: 'b.R'")

  file.create(file.path(path, srcs[[2]]))
  expect_message(
    hipercow_environment_create(name = "default", sources = srcs, root = path),
    "Created environment 'default'")
})


test_that("can load environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("a <- 1", file.path(path, "a.R"))
  suppressMessages(
    hipercow_environment_create("foo", sources = "a.R", root = path))
  env <- new.env()
  environment_apply("foo", env, root)
  expect_equal(names(env), "a")
  expect_equal(env$a, 1)
})


test_that("loading environments can load packages", {
  mock_library <- mockery::mock()
  mockery::stub(environment_apply, "library", mock_library)
  mock_source <- mockery::mock()
  mockery::stub(environment_apply, "sys.source", mock_source)

  path <- withr::local_tempfile()
  root <- init_quietly(path)
  srcs <- c("a.R", "b.R")
  pkgs <- c("x", "y", "z")
  file.create(file.path(path, srcs))
  suppressMessages(
    hipercow_environment_create(sources = srcs, packages = pkgs, root = path))
  env <- new.env()
  environment_apply("default", env, root)

  mockery::expect_called(mock_library, 3)
  expect_equal(mockery::mock_args(mock_library),
               list(list("x", character.only = TRUE),
                    list("y", character.only = TRUE),
                    list("z", character.only = TRUE)))

  mockery::expect_called(mock_source, 2)
  expect_equal(mockery::mock_args(mock_source),
               list(list("a.R", envir = env),
                    list("b.R", envir = env)))
})


test_that("can delete environments", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs <- c("x", "y")
  expect_message(
    hipercow_environment_create("foo", packages = pkgs, root = path),
    "Created environment 'foo'")
  expect_equal(hipercow_environment_list(path), c("default", "foo"))
  expect_message(
    hipercow_environment_delete("foo", path),
    "Deleting environment 'foo' (if it existed)", fixed = TRUE)
  expect_equal(hipercow_environment_list(path), "default")
})
