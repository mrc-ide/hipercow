test_that("can construct an environment", {
  env <- new_environment("foo",
                         packages = NULL,
                         sources = NULL,
                         globals = NULL,
                         check = TRUE,
                         root = NULL)
  expect_s3_class(env, "hipercow_environment")
  expect_setequal(names(env), c("name", "packages", "sources", "globals"))
  expect_equal(env$name, "foo")
  expect_null(env$packages)
  expect_null(env$sources)
  expect_null(env$globals)
})


test_that("can construct a nontrivial environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  file.create(file.path(path, c("a.R", "b.R")))
  env <- new_environment("foo",
                         packages = c("x", "y", "z"),
                         sources = c("a.R", "b.R"),
                         globals = c("i", "j", "k"),
                         check = TRUE,
                         root = root)
  expect_equal(env$name, "foo")
  expect_equal(env$sources, c("a.R", "b.R"))
  expect_equal(env$packages, c("x", "y", "z"))
  expect_equal(env$globals, c("i", "j", "k"))
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
  expect_match(msg, "globals: (none)", all = FALSE, fixed = TRUE)
})


test_that("can print nontrivial environments", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  file.create(file.path(path, c("a.R", "b.R", "c.R")))
  suppressMessages(
    hipercow_environment_create("foo",
                              packages = c("x", "y", "z"),
                              sources = c("a.R", "b.R", "c.R"),
                              globals = c("i", "j", "k"),
                              root = path))
  msg <- capture_messages(
    hipercow_environment_show("foo", root))
  expect_match(msg, "hipercow environment 'foo'", all = FALSE)
  expect_match(msg, "packages: x, y, z", all = FALSE, fixed = TRUE)
  expect_match(msg, "sources: a.R, b.R, c.R", all = FALSE, fixed = TRUE)
  expect_match(msg, "globals: i, j, k", all = FALSE, fixed = TRUE)
})


test_that("error if loading unknown environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  err <- expect_error(
    hipercow_environment_show("foo", path),
    "Environment 'foo' does not exist")
  expect_equal(err$body, c(i = "Valid options are: 'default' and 'empty'"))
})


test_that("can create environment in root", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs <- c("x", "y")
  expect_message(
    hipercow_environment_create(packages = pkgs, root = path),
    "Created environment 'default'")
  expect_equal(environment_load("default", root),
               new_environment("default",
                               packages = pkgs,
                               sources = NULL,
                               globals = NULL,
                               check = TRUE,
                               root = root))
})


test_that("special environments always exist", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  expect_true(hipercow_environment_exists("default", root))
  expect_true(hipercow_environment_exists("empty", root))
  expect_false(hipercow_environment_exists("other", root))
  expect_no_error(ensure_environment_exists("default", root))
  expect_no_error(ensure_environment_exists("empty", root))
  expect_error(ensure_environment_exists("other", root),
               "Environment 'other' does not exist")
})


test_that("can update existing environment in root", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  pkgs1 <- c("x", "y")
  pkgs2 <- c("x", "y", "z")

  new_environment2 <- function(name, pkgs) {
    new_environment(name,
                    packages = pkgs,
                    sources = NULL,
                    globals = NULL,
                    check = TRUE,
                    root = root)
  }

  expect_false(hipercow_environment_exists("foo", path))
  expect_equal(hipercow_environment_list(path), c("default", "empty"))
  expect_message(
    hipercow_environment_create(name = "foo", packages = pkgs1, root = path),
    "Created environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment2("foo", pkgs1))
  expect_message(
    hipercow_environment_create(name = "foo", packages = pkgs2, root = path),
    "Updated environment 'foo'")
  expect_equal(environment_load("foo", root),
               new_environment2("foo", pkgs2))
  expect_message(
    hipercow_environment_create(name = "foo", packages = pkgs2, root = path),
    "Environment 'foo' is unchanged")
  expect_equal(environment_load("foo", root),
               new_environment2("foo", pkgs2))
  expect_true(hipercow_environment_exists("foo", path))
  expect_equal(hipercow_environment_list(path), c("default", "empty", "foo"))
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
               new_environment("foo",
                               packages = pkgs1,
                               sources = NULL,
                               globals = NULL,
                               check = TRUE,
                               root = root))
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
               new_environment("default", NULL, NULL, NULL, TRUE, root))
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
  expect_equal(hipercow_environment_list(path), c("default", "empty", "foo"))
  expect_message(
    hipercow_environment_delete("foo", path),
    "Deleting environment 'foo' (if it existed)", fixed = TRUE)
  expect_equal(hipercow_environment_list(path), c("default", "empty"))
})


test_that("can discover globals in environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines(c("a <- 1", "b <- 2"), file.path(path, "src.R"))
  res <- evaluate_promise(
    discover_globals("myenv", NULL, "src.R", root))
  expect_equal(res$result, c("a", "b"))
  expect_match(res$messages[[1]], "Creating 'myenv' in a clean R session")
  expect_match(res$messages[[2]], "Found 2 symbols")
})


test_that("special value 'TRUE' triggers global environment build", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("a <- 1", file.path(path, "src.R"))
  res <- evaluate_promise(
    new_environment("default",
                    packages = NULL,
                    sources = "src.R",
                    globals = TRUE,
                    check = TRUE,
                    root = root))
  expect_equal(res$result$globals, "a")
  expect_match(res$messages[[1]], "Creating 'default' in a clean R session")
  expect_match(res$messages[[2]], "Found 1 symbol\\b")
})


test_that("can validate environment name on creation", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  expect_error(
    hipercow_environment_create("empty", root = path),
    "Can't create environment with special name 'empty'")
  expect_error(
    hipercow_environment_create("foo/bar", root = path),
    "Invalid environment name 'foo/bar'")
  expect_error(
    hipercow_environment_create("", root = path),
    "Invalid environment name ''")
})


test_that("can always load the empty environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  env <- environment_load("empty", root = root)
  expect_equal(env$name, "empty")
  expect_null(env$packages)
  expect_null(env$sources)
  expect_null(env$globals)
})


test_that("can check contents of sources", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("install.packages('whatever')", file.path(path, "src.R"))
  expect_error(
    environment_check_sources(file.path(path, "src.R")),
    "Found call to 'install.packages()' in",
    fixed = TRUE)

  expect_error(
    new_environment("foo",
                    packages = NULL,
                    sources = "src.R",
                    globals = NULL,
                    check = TRUE,
                    root = root),
    "Found call to 'install.packages()' in",
    fixed = TRUE)
})


test_that("warn about directories instead of files in sources", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("TRUE", file.path(path, "src.R"))
  dir.create(file.path(path, "other"))
  expect_error(
    new_environment("foo",
                    packages = NULL,
                    sources = c("src.R", "other"),
                    globals = NULL,
                    check = TRUE,
                    root = root),
    "File in 'sources' is a directory, not a file: 'other'",
    fixed = TRUE)
})


test_that("Can be verbose when applying an environment", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("a <- 1", file.path(path, "a.R"))

  suppressMessages(hipercow_environment_create("foo", packages = "base",
                                               sources = "a.R", root = path))

  env <- new.env()
  res <- evaluate_promise(environment_apply("foo", env, root, verbose = TRUE))
  expect_match(res$messages, "packages: base", all = FALSE, fixed = TRUE)
  expect_match(res$messages, "sources: a.R", all = FALSE, fixed = TRUE)
  expect_match(res$messages, "globals: (none)", all = FALSE, fixed = TRUE)
})
