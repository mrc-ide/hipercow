test_that("can submit a task via a driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()

  init_quietly(path_here)
  init_quietly(path_there)

  hermod_configure("elsewhere", path = path_there, root = path_here)

  id <- hermod_task_create_explicit(quote(getwd()), root = path_here)
  expect_equal(hermod_task_status(id, root = path_here), "created")

  withr::with_dir(path_here, hermod_task_submit(id))

  expect_equal(hermod_task_status(id, root = path_here), "submitted")
  expect_equal(
    readLines(file.path(path_here, "hermod", "tasks", id, "status-submitted")),
    "elsewhere")

  expect_true(file.exists(file.path(path_there, "hermod", "tasks", id)))
  expect_equal(dir(file.path(path_there, "hermod", "tasks", id)), "expr")
  expect_equal(readLines(file.path(path_there, "elsewhere.queue")), id)

  expect_true(withr::with_dir(path_there, hermod_task_eval(id)))
})


test_that("forbid additional arguments to submission, for now", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  hermod_configure("elsewhere", path = path_there, root = path_here)
  id <- hermod_task_create_explicit(quote(getwd()), root = path_here)
  expect_error(
    withr::with_dir(path_here, hermod_task_submit(id, cores = 2)),
    "Additional arguments to 'hermod_task_submit' not allowed")
})
