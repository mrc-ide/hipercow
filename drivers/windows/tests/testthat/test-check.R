test_that("windows_check checks credentials, connection and path", {
  mock_credentials <- mockery::mock(TRUE, FALSE, cycle = TRUE)
  mock_connection <- mockery::mock(TRUE, TRUE, FALSE, FALSE, cycle = TRUE)
  mock_path <- mockery::mock(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  mockery::stub(windows_check, "windows_check_credentials", mock_credentials)
  mockery::stub(windows_check, "windows_check_connection", mock_connection)
  mockery::stub(windows_check, "windows_check_path", mock_path)
  expect_true(windows_check())
  mockery::expect_called(mock_credentials, 1)
  expect_equal(mockery::mock_args(mock_credentials)[[1]], list())
  mockery::expect_called(mock_connection, 1)
  expect_equal(mockery::mock_args(mock_connection)[[1]], list())
  mockery::expect_called(mock_path, 1)
  expect_equal(mockery::mock_args(mock_path)[[1]], list(getwd()))

  expect_false(windows_check()) # 2
  expect_false(windows_check()) # 3
  expect_false(windows_check()) # 4
  expect_false(windows_check()) # 5
  expect_false(windows_check()) # 6
  expect_false(windows_check()) # 7
  expect_false(windows_check()) # 8

  mockery::expect_called(mock_credentials, 8)
  mockery::expect_called(mock_connection, 8)
  mockery::expect_called(mock_path, 8)
})


test_that("windows_check_credentials checks found and perhaps connection", {
  creds <- credentials("alice", "pw")
  mock_found <- mockery::mock(NULL, creds, creds)
  mock_correct <- mockery::mock(TRUE, FALSE)
  mockery::stub(windows_check_credentials, "windows_check_credentials_found",
                mock_found)
  mockery::stub(windows_check_credentials, "windows_check_credentials_correct",
                mock_correct)

  expect_false(windows_check_credentials())
  mockery::expect_called(mock_found, 1)
  expect_equal(mockery::mock_args(mock_found)[[1]], list())
  mockery::expect_called(mock_correct, 0)

  expect_true(windows_check_credentials())
  mockery::expect_called(mock_found, 2)
  expect_equal(mockery::mock_args(mock_found)[[2]], list())
  mockery::expect_called(mock_correct, 1)
  expect_equal(mockery::mock_args(mock_correct)[[1]], list(creds))

  expect_false(windows_check_credentials())
  mockery::expect_called(mock_found, 3)
  expect_equal(mockery::mock_args(mock_found)[[3]], list())
  mockery::expect_called(mock_correct, 2)
  expect_equal(mockery::mock_args(mock_correct)[[2]], list(creds))
})


test_that("can check if credentials are found", {
  creds <- credentials("alice", "pw")
  mock_credentials <- mockery::mock(
    stop("Did not find your DIDE credentials"), creds)
  mockery::stub(windows_check_credentials_found, "windows_credentials",
                mock_credentials)

  res <- evaluate_promise(windows_check_credentials_found())
  expect_null(res$result)
  expect_match(res$messages, "Did not find your DIDE credentials")
  mockery::expect_called(mock_credentials, 1)
  expect_equal(mockery::mock_args(mock_credentials)[[1]], list())

  res <- evaluate_promise(windows_check_credentials_found())
  expect_equal(res$result, creds)
  expect_match(res$messages, "Found DIDE credentials for 'alice'")
  mockery::expect_called(mock_credentials, 2)
  expect_equal(mockery::mock_args(mock_credentials)[[2]], list())
})


test_that("can check that credentials are correct", {
  creds <- credentials("alice", "pw")
  mock_login <- mockery::mock(
    stop("failure"), NULL)
  mockery::stub(windows_check_credentials_correct, "api_client_login",
                mock_login)

  res <- evaluate_promise(windows_check_credentials_correct(creds))
  expect_false(res$result)
  expect_length(res$messages, 3)
  expect_match(res$messages[[1]], "Failed to log into the portal")
  expect_match(res$messages[[2]], "If your password has expired")
  expect_match(res$messages[[3]], "Original error message: failure")
  mockery::expect_called(mock_login, 1)
  expect_equal(mockery::mock_args(mock_login)[[1]],
               list(creds$username, creds$password))

  res <- evaluate_promise(windows_check_credentials_correct(creds))
  expect_true(res$result)
  expect_length(res$messages, 1)
  expect_match(res$messages[[1]], "DIDE credentials are correct")
  mockery::expect_called(mock_login, 2)
  expect_equal(mockery::mock_args(mock_login)[[2]],
               list(creds$username, creds$password))
})


test_that("can check connection works", {
  mock_head <- mockery::mock(stop("some error"), NULL)
  mockery::stub(windows_check_connection, "httr::HEAD", mock_head)

  res <- evaluate_promise(windows_check_connection())
  expect_false(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "Failed to make connection with private network")
  expect_match(res$messages[[2]], "Please check that you have ZScaler enabled")
  mockery::expect_called(mock_head, 1)
  expect_equal(mockery::mock_args(mock_head)[[1]],
               list("https://vault.dide.ic.ac.uk:8200", httr::timeout(1)))

  res <- evaluate_promise(windows_check_connection())
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

  res <- evaluate_promise(windows_check_path(paths[[1]], mounts))
  expect_true(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Path looks like it is on a network share")
  expect_match(res$messages[[2]], "Using")

  res <- evaluate_promise(windows_check_path(getwd(), mounts))
  expect_false(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Failed to map path to a network share")
  expect_match(res$messages[[2]], "You need to work with your hipercow")
})
