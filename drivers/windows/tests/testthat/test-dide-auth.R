test_that("Require a sensible name", {
  expect_error(check_username(""), "Invalid empty username")
  expect_equal(check_username("bob"), "bob")
  expect_equal(check_username("DIDE\\bob"), "bob")
})


test_that("hide passwords on print", {
  pw <- structure("secret", class = "password")
  expect_equal(as.character(pw), "*******************")
  expect_output(print(pw), "*******************", fixed = TRUE)
})


test_that("informative error if credentials are not set", {
  mock_key_get <- mockery::mock(stop("not found"))
  mockery::stub(windows_credentials, "keyring::key_get", mock_key_get)
  expect_error(
    windows_credentials(),
    "Did not find your DIDE credentials, please run 'windows_authenticate()'",
    fixed = TRUE)
  mockery::expect_called(mock_key_get, 1)
  expect_equal(mockery::mock_args(mock_key_get)[[1]],
               list("hipercow/dide/username"))
})


test_that("can fetch dide credentials", {
  mock_key_get <- mockery::mock("alice", "secret")
  mockery::stub(windows_credentials, "keyring::key_get", mock_key_get)
  res <- windows_credentials()
  expect_equal(res$username, "alice")
  expect_equal(res$password, structure("secret", class = "password"))
  mockery::expect_called(mock_key_get, 2)
  expect_equal(
    mockery::mock_args(mock_key_get),
    list(list("hipercow/dide/username"),
         list("hipercow/dide/password", username = "alice")))
})


test_that("can fetch dide username", {
  mock_credentials <- mockery::mock(credentials("alice", "pw"))
  mockery::stub(windows_username, "windows_credentials", mock_credentials)
  expect_equal(windows_username(), "alice")
  mockery::expect_called(mock_credentials, 1)
  args <- mockery::mock_args(mock_credentials)[[1]]
  expect_length(args, 1)
  expect_equal(names(args), "call")
  expect_null(args[[1]])
})


test_that("can store credentials in keychain", {
  ## This is pretty grim, but I've not seen another approach to this.
  mock_keyring_is_locked <- mockery::mock(TRUE)
  mock_keyring_unlock <- mockery::mock()
  mock_guess <- mockery::mock("bob")
  mock_readline <- mockery::mock("alice")
  mock_key_set <- mockery::mock()
  mock_key_get <- mockery::mock("secret")
  mock_login <- mockery::mock(NULL)
  mock_key_set_with_value <- mockery::mock()

  mockery::stub(windows_authenticate, "keyring::keyring_is_locked",
                mock_keyring_is_locked)
  mockery::stub(windows_authenticate, "keyring::keyring_unlock",
                mock_keyring_unlock)
  mockery::stub(windows_authenticate, "windows_guess_username", mock_guess)
  mockery::stub(windows_authenticate, "readline_with_default", mock_readline)
  mockery::stub(windows_authenticate, "keyring::key_set", mock_key_set)
  mockery::stub(windows_authenticate, "keyring::key_get", mock_key_get)
  mockery::stub(windows_authenticate, "api_client_login", mock_login)
  mockery::stub(windows_authenticate, "keyring::key_set_with_value",
                mock_key_set_with_value)

  result <- testthat::evaluate_promise(windows_authenticate())

  expect_equal(result$result, credentials("alice", "secret"))
  expect_match(result$messages, "Please enter your DIDE credentials",
               all = FALSE)

  mockery::expect_called(mock_keyring_is_locked, 1)
  mockery::expect_called(mock_keyring_unlock, 1)
  mockery::expect_called(mock_guess, 1)
  mockery::expect_called(mock_readline, 1)
  expect_equal(mockery::mock_args(mock_readline)[[1]],
               list("DIDE username", "bob"))
  mockery::expect_called(mock_key_set, 1)
  expect_equal(mockery::mock_args(mock_key_set)[[1]],
               list("hipercow/dide/password", username = "alice"))
  mockery::expect_called(mock_key_get, 1)
  expect_equal(mockery::mock_args(mock_key_get)[[1]],
               list("hipercow/dide/password", username = "alice"))
  mockery::expect_called(mock_login, 1)
  expect_equal(mockery::mock_args(mock_login)[[1]],
               list("alice", "secret"))
  mockery::expect_called(mock_key_set_with_value, 1)
  expect_equal(mockery::mock_args(mock_key_set_with_value)[[1]],
               list("hipercow/dide/username", password = "alice"))
})

test_that("invalid username rejected", {
  mock_keyring_is_locked <- mockery::mock(FALSE)
  mock_guess <- mockery::mock("bob")
  mock_readline <- mockery::mock("# I pasted this")
  mock_login <- mockery::mock(stop("invalid credentials"))

  mockery::stub(windows_authenticate, "keyring::keyring_is_locked",
                mock_keyring_is_locked)
  mockery::stub(windows_authenticate, "windows_guess_username", mock_guess)
  mockery::stub(windows_authenticate, "readline_with_default", mock_readline)
  mockery::stub(windows_authenticate, "api_client_login", mock_login)

  err <- expect_error(
    suppressMessages(windows_authenticate()),
    "The username you provided does not look valid")
  expect_equal(
    err$body,
    c(x = "It contains 3 spaces and 1 hash",
      i = "I tried to login as user # I pasted this",
      i = "Please try again with 'windows_authenticate()'"))

})

test_that("delete username on error", {
  mock_keyring_is_locked <- mockery::mock(FALSE)
  mock_guess <- mockery::mock("bob")
  mock_readline <- mockery::mock("alice")
  mock_key_set <- mockery::mock()
  mock_key_get <- mockery::mock("secret")
  mock_login <- mockery::mock(stop("invalid credentials"))
  mock_key_delete <- mockery::mock()
  mock_key_set_with_value <- mockery::mock()

  mockery::stub(windows_authenticate, "keyring::keyring_is_locked",
                mock_keyring_is_locked)
  mockery::stub(windows_authenticate, "windows_guess_username", mock_guess)
  mockery::stub(windows_authenticate, "readline_with_default", mock_readline)
  mockery::stub(windows_authenticate, "keyring::key_set", mock_key_set)
  mockery::stub(windows_authenticate, "keyring::key_get", mock_key_get)
  mockery::stub(windows_authenticate, "api_client_login", mock_login)
  mockery::stub(windows_authenticate, "keyring::key_delete", mock_key_delete)
  mockery::stub(windows_authenticate, "keyring::key_set_with_value",
                mock_key_set_with_value)

  err <- expect_error(
    suppressMessages(windows_authenticate()),
    "That username/password combination did not work, I'm afraid")
  expect_equal(
    err$body,
    c(x = "invalid credentials",
      i = "The username provided was alice",
      i = "Please try again with 'windows_authenticate()'"))

  mockery::expect_called(mock_keyring_is_locked, 1)
  mockery::expect_called(mock_guess, 1)
  mockery::expect_called(mock_readline, 1)
  expect_equal(mockery::mock_args(mock_readline)[[1]],
               list("DIDE username", "bob"))
  mockery::expect_called(mock_key_set, 1)
  expect_equal(mockery::mock_args(mock_key_set)[[1]],
               list("hipercow/dide/password", username = "alice"))
  mockery::expect_called(mock_key_get, 1)
  expect_equal(mockery::mock_args(mock_key_get)[[1]],
               list("hipercow/dide/password", username = "alice"))
  mockery::expect_called(mock_login, 1)
  expect_equal(mockery::mock_args(mock_login)[[1]],
               list("alice", "secret"))
  mockery::expect_called(mock_key_delete, 1)
  expect_equal(mockery::mock_args(mock_key_delete)[[1]],
               list("hipercow/dide/password", username = "alice"))
  mockery::expect_called(mock_key_set_with_value, 0)
})


test_that("guess username", {
  mock_list <- mockery::mock(list(service = "x"),
                             list(service = c("x", "hipercow/dide/username")))
  mock_get <- mockery::mock("alice")
  mockery::stub(windows_guess_username, "keyring::key_list", mock_list)
  mockery::stub(windows_guess_username, "keyring::key_get", mock_get)

  expect_equal(windows_guess_username(), get_system_username())
  mockery::expect_called(mock_list, 1)
  mockery::expect_called(mock_get, 0)

  expect_equal(windows_guess_username(), "alice")
  mockery::expect_called(mock_list, 2)
  mockery::expect_called(mock_get, 1)
  expect_equal(mockery::mock_args(mock_get)[[1]],
               list("hipercow/dide/username"))

  expect_equal(mockery::mock_args(mock_list), list(list(), list()))
})
