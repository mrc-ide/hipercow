test_that("can select tasks to purge by id", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(path,
                       suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  ids <- ids::random_id(5)

  root <- hipercow_root(path)
  expect_equal(task_select_by_id(b$ids, root, NULL), b$ids)
  expect_equal(task_select_by_id(b$ids[1:2], root, NULL), b$ids[1:2])
  expect_equal(task_select_by_id(character(), root, NULL), character())
  expect_equal(task_select_by_id(NULL, root, NULL), NULL)
  expect_error(task_select_by_id(ids, root, NULL),
               "5 / 5 tasks do not exist")
  expect_error(task_select_by_id(c(b$ids, ids), root, NULL),
               "5 / 10 tasks do not exist")
  expect_error(task_select_by_id(c(b$ids, ids[1]), root, NULL),
               "1 / 6 tasks do not exist")
})



test_that("can't purge some task types", {
  path <- withr::local_tempdir()
  init_quietly(path)

  expect_error(
    hipercow_purge(with_status = "submitted", root = path),
    "Cannot purge tasks with status: 'submitted'")
  expect_error(
    hipercow_purge(with_status = c("submitted", "running"), root = path),
    "Cannot purge tasks with statuses: 'submitted' and 'running'")
})


test_that("can purge", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(path,
                       suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  res <- evaluate_promise(hipercow_purge(in_bundle = "*", root = path))
  expect_equal(res$result, b$ids)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Purging 5 tasks")
  expect_match(res$messages[[2]], "Deleting 1 task bundle")
})


test_that("can report when no bundles were deleted", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path,
                        suppressMessages(task_create_call(sqrt, list(1))))
  res <- evaluate_promise(hipercow_purge(with_status = "created", root = path))
  expect_equal(res$result, id)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Purging 1 task")
  expect_match(res$messages[[2]], "No task bundles need deleting")
})


test_that("can tell the user when purge achieves nothing", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  res <- evaluate_promise(
    hipercow_purge(with_status = "cancelled", root = path))
  expect_equal(res$result, character())
  expect_length(res$messages, 1)
  expect_match(res$messages[[1]], "Nothing to purge")
})


test_that("disallow purging if running or submitted tasks selected", {
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  expect_error(
    hipercow_purge(in_bundle = b$name, root = path),
    "Can't purge; some tasks are submitted or running")
})


test_that("can purge with retries", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  for (id in b$ids) {
    task_eval(id, root = path)
  }
  suppressMessages(hipercow_bundle_retry(b, root = path))
  ids <- setdiff(unlist(lapply(b$ids, retry_chain, root)), b$ids)

  res <- evaluate_promise(
    hipercow_purge(task_ids = b$ids, root = path))
  expect_setequal(res$result, c(b$ids, ids))
  expect_length(res$messages, 3)
  expect_match(res$messages[[1]], "Also purging 5 tasks from retry chains")
})


test_that("disallow purging if running or submitted retried tasks selected", {
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  root <- hipercow_root(path)
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  for (id in b$ids) {
    task_eval(id, root = path)
  }
  suppressMessages(hipercow_bundle_retry(b, root = path))

  expect_error(
    hipercow_purge(task_ids = b$ids, root = path),
    "Can't purge; some tasks have submitted or running retries")
})


test_that("must use at least one filter type", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    hipercow_purge(root = path),
    "No filter selected")
})

test_that("can do a dry run purge", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(path,
                       suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  res <- evaluate_promise(hipercow_purge(in_bundle = "*", root = path,
                                         dry_run = TRUE))
  expect_equal(res$result, b$ids)
  expect_length(res$messages, 12)
  expect_match(res$messages[[1]], "Purging 5 tasks")
  expect_match(res$messages[[2]], "Dry run")
  expect_match(res$messages[[9]], "Deleting 1 task bundle")
  expect_match(res$messages[[10]], "Dry run")

  files <- gsub("[^a-zA-Z0-9:_/\\]", "",
             gsub(" recursively.", "",
               res$messages[c(3:7, 11)]))

  expect_true(all(file.exists(files)))
  suppressMessages(hipercow_purge(in_bundle = "*", root = path))
  expect_false(any(file.exists(files)))
})
