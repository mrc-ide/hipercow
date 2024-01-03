test_that("windows_check checks credentials and connection", {
  mock_credentials <- mockery::mock()
  mock_connection <- mockery::mock()
  mockery::stub(windows_check, "windows_check_credentials", mock_credentials)
  mockery::stub(windows_check, "windows_check_connection", mock_connection)
  expect_silent(windows_check())
  mockery::expect_called(mock_credentials, 1)
  expect_equal(mockery::mock_args(mock_credentials)[[1]], list())
  mockery::expect_called(mock_connection, 1)
  expect_equal(mockery::mock_args(mock_connection)[[1]], list())
})


test_that("windows_check_credentials checks found and perhaps connection", {
  creds <- credentials("alice", "pw")
  mock_found <- mockery::mock(NULL, creds)
  mock_correct <- mockery::mock()
  mockery::stub(windows_check_credentials, "windows_check_credentials_found",
                mock_found)
  mockery::stub(windows_check_credentials, "windows_check_credentials_correct",
                mock_correct)

  expect_silent(windows_check_credentials())
  mockery::expect_called(mock_found, 1)
  expect_equal(mockery::mock_args(mock_found)[[1]], list())
  mockery::expect_called(mock_correct, 0)

  expect_silent(windows_check_credentials())
  mockery::expect_called(mock_found, 2)
  expect_equal(mockery::mock_args(mock_found)[[2]], list())
  mockery::expect_called(mock_correct, 1)
  expect_equal(mockery::mock_args(mock_correct)[[1]], list(creds))
})


test_that("can check if credentials are found", {
  creds <- credentials("alice", "pw")
  mock_credentials <- mockery::mock(
    stop("Did not find your DIDE credentials"), creds)
  mockery::stub(windows_check_credentials_found, "windows_credentials",
                mock_credentials)

  res1 <- evaluate_promise(windows_check_credentials_found())
  expect_null(res1$result)
  expect_match(res1$messages, "Did not find your DIDE credentials")
  mockery::expect_called(mock_credentials, 1)
  expect_equal(mockery::mock_args(mock_credentials)[[1]], list())

  res2 <- evaluate_promise(windows_check_credentials_found())
  expect_equal(res2$result, creds)
  expect_match(res2$messages, "Found DIDE credentials for 'alice'")
  mockery::expect_called(mock_credentials, 2)
  expect_equal(mockery::mock_args(mock_credentials)[[2]], list())
})


test_that("can check that credentials are correct", {
  creds <- credentials("alice", "pw")
  mock_login <- mockery::mock(
    stop("failure"), NULL)
  mockery::stub(windows_check_credentials_correct, "api_client_login",
                mock_login)

  msg <- capture_messages(windows_check_credentials_correct(creds))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Failed to log into the portal")
  expect_match(msg[[2]], "If your password has expired")
  expect_match(msg[[3]], "Original error message: failure")
  mockery::expect_called(mock_login, 1)
  expect_equal(mockery::mock_args(mock_login)[[1]],
               list(creds$username, creds$password))

  msg <- capture_messages(windows_check_credentials_correct(creds))
  expect_length(msg, 1)
  expect_match(msg[[1]], "DIDE credentials are correct")
  mockery::expect_called(mock_login, 2)
  expect_equal(mockery::mock_args(mock_login)[[2]],
               list(creds$username, creds$password))
})


test_that("can check connection works", {
  mock_head <- mockery::mock(stop("some error"), NULL)
  mockery::stub(windows_check_connection, "httr::HEAD", mock_head)

  msg <- capture_messages(windows_check_connection())
  expect_length(msg, 2)
  expect_match(msg[[1]], "Failed to make connection with private network")
  expect_match(msg[[2]], "Please check that you have ZScaler enabled")
  mockery::expect_called(mock_head, 1)
  expect_equal(mockery::mock_args(mock_head)[[1]],
               list("https://vault.dide.ic.ac.uk:8200", httr::timeout(1)))

  msg <- capture_messages(windows_check_connection())
  expect_length(msg, 1)
  expect_match(msg[[1]], "Connection to private network working")
  mockery::expect_called(mock_head, 2)
  expect_equal(mockery::mock_args(mock_head)[[2]],
               list("https://vault.dide.ic.ac.uk:8200", httr::timeout(1)))
})
