test_that("Can create and run a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, hermod_task_create_explicit(sqrt(2)))
  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")
  expect_equal(hermod_task_status(id, root = path), "created")

  expect_true(hermod_task_eval(id, root = path))
  expect_equal(hermod_task_status(id, root = path), "success")
  expect_equal(hermod_task_result(id, root = path), sqrt(2))
})


test_that("can run a task that uses local variables", {
  env1 <- new.env()
  env2 <- new.env()
  env1$a <- 10
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    hermod_task_create_explicit(quote(sqrt(a)), export = "a", envir = env1))
  expect_true(hermod_task_eval(id, envir = env2, root = path))
  expect_equal(hermod_task_result(id, root = path), sqrt(10))
  expect_equal(names(env2), "a")
  expect_equal(env2$a, 10)
})


test_that("tasks cannot be run twice", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, hermod_task_create_explicit(sqrt(2)))
  expect_true(hermod_task_eval(id, root = path))
  expect_error(
    hermod_task_eval(id, root = path),
    "Can't start task '.+', which has status 'success'")
})


test_that("throw if result not available", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, hermod_task_create_explicit(sqrt(2)))
  expect_error(
    hermod_task_result(id, root = path),
    "Result for task '.+' not available, status is 'created'")
})


test_that("return missing status for nonexisting tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- ids::random_id()
  expect_equal(hermod_task_status(id, root = path), NA_character_)
})


test_that("can run failing tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    hermod_task_create_explicit(quote(readRDS("nofile.rds"))))
  suppressWarnings(expect_false(hermod_task_eval(id, root = path)))
  result <- hermod_task_result(id, root = path)
  expect_s3_class(result, "error")
  expect_s3_class(result$trace, "rlang_trace")
})


test_that("tasks are saved with a directory", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, hermod_task_create_explicit(getwd()))
  expect_true(hermod_task_eval(id, root = path))
  expect_equal(normalize_path(hermod_task_result(id, root = path)),
               normalize_path(path))
})


test_that("that tasks run in subdirectories propagate that forward when run", {
  root <- withr::local_tempdir()
  init_quietly(root)
  path <- file.path(root, "a", "b")
  fs::dir_create(path)
  path <- normalize_path(path)
  id <- withr::with_dir(path, hermod_task_create_explicit(getwd()))
  expect_true(hermod_task_eval(id, root = root))
  expect_equal(normalize_path(hermod_task_result(id, root = root)), path)
})


test_that("refuse to create a task outside of the root", {
  root <- withr::local_tempdir()
  init_quietly(root)
  expect_error(
    hermod_task_create_explicit(getwd(), root = root),
    "Working directory is not a subdirectory of the hermod root")
})


test_that("use cache to avoid looking up known terminal states", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, hermod_task_create_explicit(sqrt(2)))
  root <- hermod_root(path)
  root$cache$task_status_terminal[[id]] <- "failure"
  expect_equal(hermod_task_status(id, root = path), "failure")
})


test_that("hermod task status is vectorised", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id1 <- withr::with_dir(path, hermod_task_create_explicit(sqrt(1)))
  id2 <- withr::with_dir(path, hermod_task_create_explicit(sqrt(2)))
  hermod_task_eval(id2, root = path)

  expect_equal(hermod_task_status(character(), root = path), character(0))
  expect_equal(hermod_task_status(id1, root = path), "created")
  expect_equal(hermod_task_status(c(id1, id2), root = path),
               c("created", "success"))
  expect_equal(hermod_task_status(c(id1, id1), root = path),
               c("created", "created"))
})


test_that("protect against unknown task types", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, hermod_task_create_explicit(sqrt(2)))
  p <- file.path(path, "hermod", "tasks", id, "expr")
  d <- readRDS(p)
  d$type <- "magic"
  saveRDS(d, p)
  expect_false(
    hermod_task_eval(id, root = path),
    "Tried to evaluate unknown type of task 'magic'")
  result <- hermod_task_result(id, root = path)
  expect_s3_class(result, "error")
  expect_s3_class(result$trace, "rlang_trace")
  expect_equal(result$message,
               "Tried to evaluate unknown type of task 'magic'")
})
