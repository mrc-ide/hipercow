test_that("can run provision script", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_user = mockery::mock(data.frame(ids = character())),
    status_job = mockery::mock("submitted", "running", "running", "success"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_provision_run, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config$windows
  args <- list(method = "script", environment = NULL, poll = 0)

  msg <- capture_messages(
    windows_provision_run(args, config, path_root))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())

  mockery::expect_called(mock_client$submit, 1)
  args <- mockery::mock_args(mock_client$submit)[[1]]
  expect_match(args[[2]], "^conan:[[:xdigit:]]{32}$")
  id <- args[[2]]
  batch_path <- windows_path_slashes(file.path(
    "//host.dide.ic.ac.uk/share/path/b/c/hipercow/provision",
    sub("^conan:", "", id),
    "provision.bat"))

  expect_length(args, 3)
  expect_identical(args[[1]], batch_path)
  expect_identical(args[[2]], id)
  expect_identical(args[[3]]$queue, "BuildQueue")

  mockery::expect_called(mock_client$status_job, 4)
  expect_equal(mockery::mock_args(mock_client$status_job),
               rep(list(list("1234")), 4))

  expect_match(msg, "Installation script finished successfully", all = FALSE)
})


test_that("error on provision script failure", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_user = mockery::mock(data.frame(ids = character())),
    status_job = mockery::mock("submitted", "running", "running", "failure"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_provision_run, "get_web_client", mock_get_client)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))
  path_root <- root$path$root
  config <- root$config$windows
  args <- list(method = "script", environment = NULL, poll = 0)
  expect_error(
    suppressMessages(
      windows_provision_run(args, config, path_root)),
    "Installation failed after")
})


test_that("can call provision_list using conan_list", {
  mock_conan_list <- mockery::mock()
  mockery::stub(windows_provision_list, "conan2::conan_list", mock_conan_list)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config$windows
  path_lib <- file.path(path_root, config$path_lib)
  path_script <- file.path(path_root, "provision.R")

  res <- windows_provision_list(NULL, config, path_root)
  mockery::expect_called(mock_conan_list, 1)
  expect_equal(mockery::mock_args(mock_conan_list)[[1]],
               list(path_lib, NULL))

  res <- windows_provision_list(list(method = "script"), config, path_root)
  mockery::expect_called(mock_conan_list, 2)
  expect_equal(mockery::mock_args(mock_conan_list)[[2]],
               list(path_lib, rlang::hash_file(path_script)))
})


test_that("camn can provision_compare using conan_compare", {
  mock_conan_compare <- mockery::mock()
  mockery::stub(windows_provision_compare, "conan2::conan_compare",
                mock_conan_compare)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")

  path_root <- root$path$root
  config <- root$config$windows
  path_lib <- file.path(path_root, config$path_lib)

  windows_provision_compare(0, -1, config, path_root)
  mockery::expect_called(mock_conan_compare, 1)
  expect_equal(mockery::mock_args(mock_conan_compare)[[1]],
               list(path_lib, 0, -1))
})


test_that("can fail to start if some jobs still running", {
  path_root <- withr::local_tempdir()
  ids <- ids::random_id(3)
  fs::dir_create(path_to_task_file(path_root, ids[c(1, 3, 5)], NULL))
  mock_menu <- mockery::mock("cancel")
  mockery::stub(check_running_before_install, "menu", mock_menu)
  client <- list(status_user = mockery::mock(
                   data.frame(status = "running", name = ids)))
  expect_error(
    suppressMessages(check_running_before_install(client, path_root)),
    "Installation cancelled, try again later")
  mockery::expect_called(mock_menu, 1)
  expect_equal(mockery::mock_args(mock_menu)[[1]],
               list(c("cancel", "wait", "install")))

  mockery::expect_called(client$status_user, 1)
  expect_equal(mockery::mock_args(client$status_user)[[1]],
               list("*"))
})


test_that("can continue anyway to start if some jobs still running", {
  path_root <- withr::local_tempdir()
  ids <- ids::random_id(5)
  fs::dir_create(path_to_task_file(path_root, ids[c(1, 3, 5)], NULL))

  mock_menu <- mockery::mock("install")
  mockery::stub(check_running_before_install, "menu", mock_menu)
  client <- list(status_user = mockery::mock(
                   data.frame(status = "running", name = ids)))
  res <- evaluate_promise(
    check_running_before_install(client, path_root))
  expect_length(res$messages, 8)
  expect_match(res$messages[[1]],
               "Looking for active tasks")
  expect_match(res$messages[[2]],
               "You have 3 current tasks queued or running")
  expect_match(res$messages[[3]],
               "Due to the way")
  expect_match(res$messages[[4]],
               "You have three courses of action here:")
  expect_match(res$messages[[5]],
               "Give up now")
  expect_match(res$messages[[6]],
               "I can wait")
  expect_match(res$messages[[7]],
               "Let's see how it goes")
  expect_match(res$messages[[8]],
               "Trying the installation anyway")
})


test_that("can wait for tasks to finish before installation", {
  path_root <- withr::local_tempdir()
  ids <- ids::random_id(5)
  fs::dir_create(path_to_task_file(path_root, ids[c(1, 3, 5)], NULL))
  mock_menu <- mockery::mock("wait")
  mock_bundle_wait <- mockery::mock()

  mockery::stub(check_running_before_install, "menu", mock_menu)
  mockery::stub(check_running_before_install, "hipercow::hipercow_bundle_wait",
                mock_bundle_wait)

  client <- list(status_user = mockery::mock(
                   data.frame(status = "running", name = ids)))

  res <- evaluate_promise(
    check_running_before_install(client, path_root))
  expect_length(res$messages, 10)
  expect_match(res$messages[[9]],
               "Waiting for your tasks to complete")
  expect_match(res$messages[[10]],
               "All tasks now finished")
})
