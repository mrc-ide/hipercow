test_that("can create a script-based task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  path <- normalize_path(path) # issues on windows/mac gh runners
  writeLines('saveRDS(mtcars, "path.rds")', file.path(path, "script.R"))
  id <- withr::with_dir(path, task_create_script("script.R"))

  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")
  expect_equal(task_status(id, root = path), "created")

  d <- readRDS(file.path(path, "hipercow", "tasks", id, "data"))
  expect_equal(d$type, "script")
  expect_equal(d$script, "script.R")
  expect_false(d$chdir)
  expect_true(d$echo)
  expect_equal(d$path, ".")
  expect_equal(d$environment, "default")

  capture.output(expect_true(task_eval(id, root = path)))
  expect_equal(task_status(id, root = path), "success")
  expect_equal(task_result(id, root = path), NULL)
})


test_that("can change directory in script", {
  path <- withr::local_tempdir()
  init_quietly(path)
  path <- normalize_path(path) # issues on windows/mac gh runners
  fs::dir_create(file.path(path, "a/b"))
  path_script <- file.path(path, "a/b/script.R")
  writeLines('saveRDS(mtcars, "path.rds")', path_script)

  ## Four combinations here!
  id1 <- withr::with_dir(
    path,
    task_create_script("a/b/script.R", chdir = FALSE, echo = FALSE))
  id2 <- withr::with_dir(
    path,
    task_create_script("a/b/script.R", chdir = TRUE, echo = FALSE))
  id3 <- withr::with_dir(
    file.path(path, "a", "b"),
    task_create_script("script.R", chdir = FALSE, echo = FALSE))
  id4 <- withr::with_dir(
    file.path(path, "a", "b"),
    task_create_script("script.R", chdir = TRUE, echo = FALSE))

  d1 <- readRDS(file.path(path, "hipercow", "tasks", id1, "data"))
  d2 <- readRDS(file.path(path, "hipercow", "tasks", id2, "data"))
  d3 <- readRDS(file.path(path, "hipercow", "tasks", id3, "data"))
  d4 <- readRDS(file.path(path, "hipercow", "tasks", id4, "data"))

  expect_equal(d1$script, "a/b/script.R")
  expect_equal(d2$script, "a/b/script.R")
  expect_equal(d3$script, "script.R")
  expect_equal(d4$script, "script.R")

  expect_equal(d1$path, ".")
  expect_equal(d2$path, ".")
  expect_equal(d3$path, "a/b")
  expect_equal(d4$path, "a/b")

  expect_false(d1$chdir)
  expect_true(d2$chdir)
  expect_false(d3$chdir)
  expect_true(d4$chdir)

  expect_true(task_eval(id1, root = path))
  expect_true(file.exists(file.path(path, "path.rds")))
  file.remove(file.path(path, "path.rds"))

  expect_true(task_eval(id2, root = path))
  expect_true(file.exists(file.path(path, "a/b/path.rds")))
  file.remove(file.path(path, "a/b/path.rds"))

  expect_true(task_eval(id3, root = path))
  expect_true(file.exists(file.path(path, "a/b/path.rds")))
  file.remove(file.path(path, "a/b/path.rds"))

  expect_true(task_eval(id4, root = path))
  expect_true(file.exists(file.path(path, "a/b/path.rds")))
  file.remove(file.path(path, "a/b/path.rds"))
})


test_that("script file must exist", {
  path <- withr::local_tempdir()
  init_quietly(path)
  path <- normalize_path(path) # issues on windows/mac gh runners
  expect_error(
    withr::with_dir(path, task_create_script("foo.R")),
    "Script file 'foo.R' does not exist")
})


test_that("script file must exist within hipercow root", {
  path <- withr::local_tempdir()
  init_quietly(path)
  path <- normalize_path(path) # issues on windows/mac gh runners
  script <- withr::local_tempfile()
  file.create(script)
  expect_error(
    withr::with_dir(path, task_create_script(script)),
    "Script file '.+' is not contained within hipercow root")
})


test_that("must evaluate from within hipercow root", {
  path <- withr::local_tempdir()
  init_quietly(path)
  path <- normalize_path(path) # issues on windows/mac gh runners
  file.create(file.path(path, "foo.R"))
  expect_error(
    task_create_script(file.path(path, "foo.R"), root = path),
    "Working directory is not a subdirectory of the hipercow root")
})
