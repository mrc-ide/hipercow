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
