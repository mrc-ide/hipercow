test_that("can construct an environment", {
  env <- new_environment("foo", NULL, NULL)
  expect_s3_class(env, "hermod_environment")
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


cli::test_that_cli("can print empty environments", {
  env <- new_environment("foo", NULL, NULL)
  testthat::expect_snapshot({
    print(env)
  })
})


cli::test_that_cli("can print nontrivial environments", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  file.create(file.path(path, c("a.R", "b.R")))
  env <- new_environment("foo", c("a.R", "b.R"), c("x", "y", "z"), root)
  testthat::expect_snapshot({
    print(env)
  })
})


test_that("can create environment in root", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs <- c("x", "y")
  expect_message(
    hermod_environment_create(packages = pkgs, root = path),
    "Created environment 'default'")
  expect_equal(environment_load("default", root),
               new_environment("default", NULL, pkgs, root))
})


test_that("can update existing environment in root", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs1 <- c("x", "y")
  pkgs2 <- c("x", "y", "z")
  expect_false(hermod_environment_exists("foo", path))
  expect_equal(hermod_environment_list(path), "default")
  expect_message(
    hermod_environment_create(name = "foo", packages = pkgs1, root = path),
    "Created environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs1, root))
  expect_message(
    hermod_environment_create(name = "foo", packages = pkgs2, root = path),
    "Updated environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs2, root))
  expect_message(
    hermod_environment_create(name = "foo", packages = pkgs2, root = path),
    "Environment 'foo' is unchanged")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs2, root))
  expect_true(hermod_environment_exists("foo", path))
  expect_equal(hermod_environment_list(path), c("default", "foo"))
})


test_that("can prevent overwriting of an environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs1 <- c("x", "y")
  pkgs2 <- c("x", "y", "z")
  expect_message(
    hermod_environment_create(name = "foo", packages = pkgs1,
                              overwrite = FALSE, root = path),
    "Created environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment("foo", NULL, pkgs1, root))
  expect_error(
    hermod_environment_create(name = "foo", packages = pkgs2,
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
    hermod_environment_create(name = "default", packages = pkgs,
                              overwrite = FALSE, root = path),
    "Created environment 'default'")
  expect_message(
    hermod_environment_create(name = "default", packages = pkgs,
                              overwrite = FALSE, root = path),
    "Environment 'default' is unchanged")
  expect_error(
    hermod_environment_create(name = "default", packages = NULL,
                              overwrite = FALSE, root = path),
    "Environment 'default' already exists and 'overwrite' is FALSE")
})


test_that("sources must exist within the root", {
  path <- withr::local_tempfile()
  srcs <- c("a.R", "b.R")
  root <- init_quietly(path)

  err <- expect_error(
    hermod_environment_create(name = "default", sources = srcs, root = path),
    "Files in 'sources' not found: 'a.R' and 'b.R'")
  expect_equal(err$body,
               c(i = sprintf("Looking relative to '%s'", root$path$root)))

  file.create(file.path(path, srcs[[1]]))
  expect_error(
    hermod_environment_create(name = "default", sources = srcs, root = path),
    "File in 'sources' not found: 'b.R'")

  file.create(file.path(path, srcs[[2]]))
  expect_message(
    hermod_environment_create(name = "default", sources = srcs, root = path),
    "Created environment 'default'")
})
