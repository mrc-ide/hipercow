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
  mockery::stub(dide_credentials, "keyring::key_get", mock_key_get)
  expect_error(
    dide_credentials(),
    "Did not find your DIDE credentials, please run 'dide_authenticate()'",
    fixed = TRUE)
  mockery::expect_called(mock_key_get, 1)
  expect_equal(mockery::mock_args(mock_key_get)[[1]],
               list("hermod/dide/username"))
})
