test_that("can run provision script for windows", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_user = mockery::mock(data.frame(ids = character()), cycle = TRUE),
    status_job = mockery::mock("submitted", "running", "running", "success"))

  mock_prep <- mockery::mock(list(
    poll = 0, id = ids::random_id(), show_log = TRUE,
    client = mock_client
  ))

  mockery::stub(dide_provision_run, "prepare_provision_run", mock_prep)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config[["dide-windows"]]
  args <- list(method = "script", environment = NULL, poll = 0)

  msg <- capture_messages(
    dide_provision_run(args, TRUE, config, path_root,
                       driver = "dide-windows"))

  mockery::expect_called(mock_prep, 1)
  mockery::expect_called(mock_client$submit, 1)
  args <- mockery::mock_args(mock_client$submit)[[1]]
  expect_match(args[[2]], "conan:[[:xdigit:]]{32}$")
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


test_that("can run provision script for linux", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_user = mockery::mock(data.frame(ids = character()), cycle = TRUE),
    status_job = mockery::mock("submitted", "running", "running", "success"))

  mock_prep <- mockery::mock(list(
    poll = 0, id = ids::random_id(), show_log = TRUE,
    client = mock_client
  ))

  mockery::stub(dide_provision_run, "prepare_provision_run", mock_prep)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config[["dide-linux"]]
  args <- list(method = "script", environment = NULL, poll = 0)

  msg <- capture_messages(
    dide_provision_run(args, TRUE, config, path_root,
                       driver = "dide-linux"))

  mockery::expect_called(mock_prep, 1)
  mockery::expect_called(mock_client$submit, 1)
  args <- mockery::mock_args(mock_client$submit)[[1]]
  expect_match(args[[2]], "conan:[[:xdigit:]]{32}$")
  id <- args[[2]]
  unc_wrap_path <- windows_path_slashes(file.path(
    "//host.dide.ic.ac.uk/share/path/b/c/hipercow/provision",
    sub("^conan:", "", id), "wrap_provision.sh"))

  node_wrap_path <- file.path("/test/path/b/c/hipercow/provision",
    sub("^conan:", "", id), "wrap_provision.sh")

  expect_length(args, 3)
  expect_identical(args[[1]], node_wrap_path)
  expect_identical(args[[2]], id)
  expect_identical(args[[3]]$queue, "LinuxNodes")

  mockery::expect_called(mock_client$status_job, 4)
  expect_equal(mockery::mock_args(mock_client$status_job),
               rep(list(list("1234")), 4))

  expect_match(msg, "Installation script finished successfully", all = FALSE)
})


test_that("error on provision script failure", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_user = mockery::mock(data.frame(ids = character()), cycle = TRUE),
    status_job = mockery::mock("submitted", "running", "running", "failure"))

  mock_prep <- mockery::mock(list(
    poll = 0, id = ids::random_id(), show_log = TRUE,
    client = mock_client
  ))

  mockery::stub(dide_provision_run, "prepare_provision_run", mock_prep)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))
  path_root <- root$path$root
  config <- root$config[["dide-windows"]]
  args <- list(method = "script", environment = NULL, poll = 0)
  expect_error(
    suppressMessages(
      dide_provision_run(args, TRUE, config, path_root,
                         driver = "dide-windows")),
    "Installation failed after")
})


test_that("can call provision_list using conan_list", {
  mock_conan_list <- mockery::mock()
  mockery::stub(dide_provision_list, "conan2::conan_list", mock_conan_list)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config[["dide-windows"]]
  path_lib <- file.path(path_root, config$path_lib)
  path_script <- file.path(path_root, "provision.R")

  res <- dide_provision_list(NULL, config, path_root)
  mockery::expect_called(mock_conan_list, 1)
  expect_equal(mockery::mock_args(mock_conan_list)[[1]],
               list(path_lib, NULL))

  res <- dide_provision_list(list(method = "script"), config, path_root)
  mockery::expect_called(mock_conan_list, 2)
  expect_equal(mockery::mock_args(mock_conan_list)[[2]],
               list(path_lib, rlang::hash_file(path_script)))
})


test_that("can can provision_compare using conan_compare", {
  mock_conan_compare <- mockery::mock()
  mockery::stub(dide_provision_compare, "conan2::conan_compare",
                mock_conan_compare)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")

  path_root <- root$path$root
  config <- root$config[["dide-windows"]]
  path_lib <- file.path(path_root, config$path_lib)

  dide_provision_compare(0, -1, config, path_root)
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
    data.frame(status = character(), name = character()),
    data.frame(status = "running", name = ids)))
  expect_error(
    suppressMessages(check_running_before_install(client, path_root)),
    "Installation cancelled, try again later")
  mockery::expect_called(mock_menu, 1)
  expect_equal(mockery::mock_args(mock_menu)[[1]],
               list(c("cancel", "wait", "install")))

  mockery::expect_called(client$status_user, 2)
  expect_equal(mockery::mock_args(client$status_user)[[2]],
               list("Running"))
})


test_that("can continue anyway to start if some jobs still running", {
  path_root <- withr::local_tempdir()
  ids <- ids::random_id(5)
  fs::dir_create(path_to_task_file(path_root, ids[c(1, 3, 5)], NULL))

  mock_menu <- mockery::mock("install")
  mockery::stub(check_running_before_install, "menu", mock_menu)
  client <- list(status_user = mockery::mock(
                   data.frame(status = character(), name = character()),
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
                   data.frame(status = character(), name = character()),
                   data.frame(status = "running", name = ids)))

  res <- evaluate_promise(
    check_running_before_install(client, path_root))
  expect_length(res$messages, 10)
  expect_match(res$messages[[9]],
               "Waiting for your tasks to complete")
  expect_match(res$messages[[10]],
               "All tasks now finished")
})


test_that("can skip preflight check", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_user = mockery::mock(data.frame(ids = character()), cycle = TRUE),
    status_job = mockery::mock("submitted", "running", "success"))
  mock_check <- mockery::mock()
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(prepare_provision_run, "get_web_client", mock_get_client)
  mockery::stub(prepare_provision_run, "check_running_before_install",
                mock_check)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config[["dide-windows"]]
  args <- list(method = "script", environment = NULL, poll = 0)
  msg <- capture_messages(
    prepare_provision_run(args, FALSE, config, path_root))
  mockery::expect_called(mock_check, 0)
})

test_that("check_running_tasks with no tasks running", {
  client <- list(
    status_user = function(x) NULL)

  msg <- capture_messages(
    check_running_before_install(client, ""))
  expect_true(grepl("Looking for active tasks", msg[1]))
  expect_true(grepl("No tasks running", msg[2]))
})


test_that("check_running_tasks arg is honoured", {
  mockery::stub(prepare_provision_run, "get_web_client", "")
  mockery::stub(prepare_provision_run, "check_old_versions", "")
  mockery::stub(prepare_provision_run, "rlang::inject", "")
  mockery::stub(prepare_provision_run, "conan2::conan_write", "")
  mock_check <- mockery::mock()
  mockery::stub(prepare_provision_run, "check_running_before_install",
                mock_check)


  res <- prepare_provision_run(list(), TRUE, NULL, NULL)
  mockery::expect_called(mock_check, 1)
})
