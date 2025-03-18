test_that("dide_check checks credentials, connection and path", {
  mock_credentials <- mockery::mock(TRUE, FALSE, cycle = TRUE)
  mock_connection <- mockery::mock(TRUE, TRUE, FALSE, FALSE, cycle = TRUE)
  mock_path <- mockery::mock(TRUE, TRUE, TRUE, TRUE,
                             FALSE, FALSE, FALSE, FALSE, cycle = TRUE)
  mock_project <- mockery::mock(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, cycle = TRUE)
  mock_versions <- mockery::mock(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, cycle = TRUE)
  mockery::stub(dide_check, "dide_check_credentials", mock_credentials)
  mockery::stub(dide_check, "dide_check_connection", mock_connection)
  mockery::stub(dide_check, "dide_check_path", mock_path)
  mockery::stub(dide_check, "dide_check_project", mock_project)
  mockery::stub(dide_check, "dide_check_versions", mock_versions)
  expect_true(dide_check())
  mockery::expect_called(mock_credentials, 1)
  expect_equal(mockery::mock_args(mock_credentials)[[1]], list())
  mockery::expect_called(mock_connection, 1)
  expect_equal(mockery::mock_args(mock_connection)[[1]], list())
  mockery::expect_called(mock_path, 1)
  expect_equal(mockery::mock_args(mock_path)[[1]], list(getwd()))
  mockery::expect_called(mock_project, 1)
  expect_equal(mockery::mock_args(mock_project)[[1]], list(getwd()))

  for (i in 2:32) {
    expect_false(dide_check())
  }

  mockery::expect_called(mock_credentials, 32)
  mockery::expect_called(mock_connection, 32)
  mockery::expect_called(mock_path, 32)
  mockery::expect_called(mock_project, 32)
  mockery::expect_called(mock_versions, 32)
})


test_that("dide_check_credentials checks found and perhaps connection", {
  creds <- credentials("alice", "pw")
  mock_found <- mockery::mock(NULL, creds, creds)
  mock_correct <- mockery::mock(TRUE, FALSE)
  mockery::stub(dide_check_credentials, "dide_check_credentials_found",
                mock_found)
  mockery::stub(dide_check_credentials, "dide_check_credentials_correct",
                mock_correct)

  expect_false(dide_check_credentials())
  mockery::expect_called(mock_found, 1)
  expect_equal(mockery::mock_args(mock_found)[[1]], list())
  mockery::expect_called(mock_correct, 0)

  expect_true(dide_check_credentials())
  mockery::expect_called(mock_found, 2)
  expect_equal(mockery::mock_args(mock_found)[[2]], list())
  mockery::expect_called(mock_correct, 1)
  expect_equal(mockery::mock_args(mock_correct)[[1]], list(creds))

  expect_false(dide_check_credentials())
  mockery::expect_called(mock_found, 3)
  expect_equal(mockery::mock_args(mock_found)[[3]], list())
  mockery::expect_called(mock_correct, 2)
  expect_equal(mockery::mock_args(mock_correct)[[2]], list(creds))
})


test_that("can check if credentials are found", {
  creds <- credentials("alice", "pw")
  mock_credentials <- mockery::mock(
    stop("Did not find your DIDE credentials"), creds)
  mockery::stub(dide_check_credentials_found, "dide_credentials",
                mock_credentials)

  res <- evaluate_promise(dide_check_credentials_found())
  expect_null(res$result)
  expect_match(res$messages, "Did not find your DIDE credentials")
  mockery::expect_called(mock_credentials, 1)
  expect_equal(mockery::mock_args(mock_credentials)[[1]], list())

  res <- evaluate_promise(dide_check_credentials_found())
  expect_equal(res$result, creds)
  expect_match(res$messages, "Found DIDE credentials for 'alice'")
  mockery::expect_called(mock_credentials, 2)
  expect_equal(mockery::mock_args(mock_credentials)[[2]], list())
})


test_that("can check that credentials are correct", {
  creds <- credentials("alice", "pw")
  mock_login <- mockery::mock(
    stop("failure"), NULL)
  mockery::stub(dide_check_credentials_correct, "api_client_login",
                mock_login)

  res <- evaluate_promise(dide_check_credentials_correct(creds))
  expect_false(res$result)
  expect_length(res$messages, 3)
  expect_match(res$messages[[1]], "Failed to log into the portal")
  expect_match(res$messages[[2]], "If your password has expired")
  expect_match(res$messages[[3]], "Original error message: failure")
  mockery::expect_called(mock_login, 1)
  expect_equal(mockery::mock_args(mock_login)[[1]],
               list(creds$username, creds$password))

  res <- evaluate_promise(dide_check_credentials_correct(creds))
  expect_true(res$result)
  expect_length(res$messages, 1)
  expect_match(res$messages[[1]], "DIDE credentials are correct")
  mockery::expect_called(mock_login, 2)
  expect_equal(mockery::mock_args(mock_login)[[2]],
               list(creds$username, creds$password))
})


test_that("can check connection works", {
  mock_head <- mockery::mock(stop("some error"), NULL)
  mockery::stub(dide_check_connection, "httr::HEAD", mock_head)

  res <- evaluate_promise(dide_check_connection())
  expect_false(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "Failed to make connection with private network")
  expect_match(res$messages[[2]], "Please check that you have ZScaler enabled")
  mockery::expect_called(mock_head, 1)
  expect_equal(mockery::mock_args(mock_head)[[1]],
               list("https://vault.dide.ic.ac.uk:8200", httr::timeout(1)))

  res <- evaluate_promise(dide_check_connection())
  expect_true(res$result)
  expect_length(res$messages, 1)
  expect_match(res$messages[[1]], "Connection to private network working")
  mockery::expect_called(mock_head, 2)
  expect_equal(mockery::mock_args(mock_head)[[2]],
               list("https://vault.dide.ic.ac.uk:8200", httr::timeout(1)))
})


test_that("can check path is ok", {
  tmp <- withr::local_tempfile()
  mounts <- cbind(local = file.path(tmp, c("a", "b", "c")),
                  remote = c("\\\\server-1\\path",
                             "\\\\server-2\\homes\\b",
                             "\\\\server-2\\homes\\c"))
  paths <- file.path(mounts[, "local"], c("sub", "dir"))
  fs::dir_create(paths)
  paths <- clean_path_local(paths)
  mounts[, "local"] <- clean_path_local(mounts[, "local"])

  res <- evaluate_promise(dide_check_path(paths[[1]], mounts))
  expect_true(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Path looks like it is on a network share")
  expect_match(res$messages[[2]], "Using")

  res <- evaluate_promise(dide_check_path(getwd(), mounts))
  expect_false(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Failed to map path to a network share")
  expect_match(res$messages[[2]], "You need to work with your hipercow")
})



test_that("chide user for not using a project", {
  path <- withr::local_tempdir()
  mock_available <- mockery::mock(FALSE, TRUE)
  mock_project <- mockery::mock(NULL)
  mockery::stub(dide_check_project, "rstudioapi::isAvailable",
                mock_available)
  mockery::stub(dide_check_project, "rstudioapi::getActiveProject",
                mock_project)

  res <- evaluate_promise(dide_check_project(path))
  expect_true(res$result)
  expect_equal(res$messages, character())
  mockery::expect_called(mock_available, 1)
  expect_equal(mockery::mock_args(mock_available)[[1]], list())
  mockery::expect_called(mock_project, 0)

  res <- evaluate_promise(dide_check_project(path))
  expect_true(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "You are not using an RStudio project")
  expect_match(res$messages[[2]], "Using a project greatly")
  mockery::expect_called(mock_available, 2)
  expect_equal(mockery::mock_args(mock_available)[[2]], list())
  mockery::expect_called(mock_project, 1)
  expect_equal(mockery::mock_args(mock_project)[[1]], list())
})


test_that("validate users project location", {
  path <- withr::local_tempdir()
  fs::dir_create(file.path(path, "a", "b"))

  mock_available <- mockery::mock(TRUE, cycle = TRUE)
  mock_project <- mockery::mock(file.path(path, "a"), cycle = TRUE)
  mockery::stub(dide_check_project, "rstudioapi::isAvailable",
                mock_available)
  mockery::stub(dide_check_project, "rstudioapi::getActiveProject",
                mock_project)

  res <- evaluate_promise(dide_check_project(file.path(path, "a")))
  expect_true(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "You are using an RStudio project")
  expect_match(res$messages[[2]],
               "Your working directory is at the project root")

  res <- evaluate_promise(dide_check_project(file.path(path, "a", "b")))
  expect_true(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "You are using an RStudio project")
  expect_match(res$messages[[2]],
               "Your working directory is a subdirectory of the project")

  res <- evaluate_promise(dide_check_project(path))
  expect_false(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "You are using an RStudio project")
  expect_match(res$messages[[2]],
               "Your working directory is not within your project, somehow")

  mockery::expect_called(mock_available, 3)
  expect_equal(mockery::mock_args(mock_available), rep(list(list()), 3))
  mockery::expect_called(mock_project, 3)
  expect_equal(mockery::mock_args(mock_project), rep(list(list()), 3))
})


test_that("can check silently when versions agree", {
  v1 <- numeric_version("1.2.3")
  expect_silent(dide_check_versions(v1, v1, TRUE))
  expect_message(dide_check_versions(v1, v1, FALSE),
                 "'hipercow' and 'hipercow.dide' versions agree (1.2.3)",
                 fixed = TRUE)
  expect_true(dide_check_versions(v1, v1, TRUE))
})


test_that("report if versions differ", {
  v1 <- numeric_version("1.2.3")
  v2 <- numeric_version("1.2.4")
  res <- evaluate_promise(dide_check_versions(v1, v2, TRUE))
  expect_false(res$result)
  expect_length(res$messages, 3)
  expect_match(
    res$messages[[1]],
    "Your 'hipercow' (1.2.3) and 'hipercow.dide' (1.2.4) versions differ",
    fixed = TRUE)
  expect_match(
    res$messages[[2]],
    "You should install both again",
    fixed = TRUE)
  expect_match(
    res$messages[[3]],
    "install.packages(",
    fixed = TRUE)
})
