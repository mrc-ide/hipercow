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
