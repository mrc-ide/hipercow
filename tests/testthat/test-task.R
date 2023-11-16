test_that("Can create and run a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- hermod_task_create_explicit(sqrt(2), root = path)
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
  id <- hermod_task_create_explicit(quote(sqrt(a)), export = "a", envir = env1,
                                    root = path)
  expect_true(hermod_task_eval(id, envir = env2, root = path))
  expect_equal(hermod_task_result(id, root = path), sqrt(10))
  expect_equal(names(env2), "a")
  expect_equal(env2$a, 10)
})


test_that("tasks cannot be run twice", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- hermod_task_create_explicit(sqrt(2), root = path)
  expect_true(hermod_task_eval(id, root = path))
  expect_error(
    hermod_task_eval(id, root = path),
    "Task '.+' has already been started")
})


test_that("throw if result not available", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- hermod_task_create_explicit(sqrt(2), root = path)
  expect_error(
    hermod_task_result(id, root = path),
    "Result for task '.+' not available, status is 'created'")
})


test_that("return missing status for nonexisting tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- ids::random_id()
  expect_equal(hermod_task_status(id, root = path), "missing")
})


test_that("can load packages in a task", {
  mock_library <- mockery::mock()
  mockery::stub(task_eval_explicit, "library", mock_library)

  path <- withr::local_tempdir()
  init_quietly(path)
  id <- hermod_task_create_explicit(sqrt(2), packages = c("foo", "bar"),
                                    root = path)
  envir <- new.env()
  root <- hermod_root(path)
  data <- readRDS(file.path(root$path$tasks, id, EXPR))
  result <- task_eval_explicit(data, envir, root)
  expect_equal(result, list(success = TRUE, value = sqrt(2)))
  mockery::expect_called(mock_library, 2)
  expect_equal(
    mockery::mock_args(mock_library),
    list(list("foo", character.only = TRUE),
         list("bar", character.only = TRUE)))
})
