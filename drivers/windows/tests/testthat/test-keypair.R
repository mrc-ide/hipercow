test_that("can generate a keypair", {
  tmp <- withr::local_tempdir()

  mock_username <- mockery::mock("bob")
  mock_key_get <- mockery::mock(stop("no key"))
  mock_key_set <- mockery::mock()
  mock_resolve <- mockery::mock(tmp)

  mockery::stub(windows_generate_keypair, "windows_username",
                mock_username)
  mockery::stub(windows_generate_keypair, "keyring::key_get",
                mock_key_get)
  mockery::stub(windows_generate_keypair, "keyring::key_set_with_value",
                mock_key_set)
  mockery::stub(windows_generate_keypair, "dide_locally_resolve_unc_path",
                mock_resolve)

  res <- evaluate_promise(windows_generate_keypair())

  expect_null(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Created new private key at")
  expect_match(res$messages[[2]], "Saved public key into your keychain")

  expect_true(file.exists(file.path(tmp, ".hipercow", "key")))
  key <- openssl::read_key(file.path(tmp, ".hipercow", "key"))
  ## Derive public key
  pub <- openssl::write_ssh(as.list(key)$pubkey)

  mockery::expect_called(mock_key_get, 1)
  expect_equal(mockery::mock_args(mock_key_get)[[1]],
               list("hipercow/dide/pubkey", username = "bob"))

  mockery::expect_called(mock_resolve, 1)
  expect_equal(mockery::mock_args(mock_resolve)[[1]],
               list("//fi--san03.dide.ic.ac.uk/homes/bob"))

  mockery::expect_called(mock_key_set, 1)
  expect_equal(
    mockery::mock_args(mock_key_set)[[1]],
    list("hipercow/dide/pubkey", username = "bob", password = pub))
})


test_that("don't generate keypair if home drive inaccessible", {
  mock_username <- mockery::mock("bob")
  mock_key_get <- mockery::mock(stop("no key"))
  mock_key_set <- mockery::mock()
  mock_resolve <- mockery::mock(NULL)

  mockery::stub(windows_generate_keypair, "windows_username",
                mock_username)
  mockery::stub(windows_generate_keypair, "keyring::key_get",
                mock_key_get)
  mockery::stub(windows_generate_keypair, "keyring::key_set_with_value",
                mock_key_set)
  mockery::stub(windows_generate_keypair, "dide_locally_resolve_unc_path",
                mock_resolve)

  expect_error(
    windows_generate_keypair(),
    "Can't generate a keypair as I failed to find your home drive")

  mockery::expect_called(mock_key_set, 0)
})


test_that("don't generate keypair if not needed", {
  tmp <- withr::local_tempdir()
  mock_username <- mockery::mock("bob", cycle = TRUE)
  mock_key_get <- mockery::mock(TRUE, cycle = TRUE)
  mock_key_set <- mockery::mock()
  mock_resolve <- mockery::mock(tmp, cycle = TRUE)

  mockery::stub(windows_generate_keypair, "windows_username",
                mock_username)
  mockery::stub(windows_generate_keypair, "keyring::key_get",
                mock_key_get)
  mockery::stub(windows_generate_keypair, "keyring::key_set_with_value",
                mock_key_set)
  mockery::stub(windows_generate_keypair, "dide_locally_resolve_unc_path",
                mock_resolve)

  res <- evaluate_promise(windows_generate_keypair())
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "Not generating a new keypair as existing keypair detected")
  expect_match(res$messages[[2]],
               "If you have deleted")
  mockery::expect_called(mock_key_set, 0)
  mockery::expect_called(mock_resolve, 0)

  res <- evaluate_promise(windows_generate_keypair(update = TRUE))
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "Created new private key")
  expect_match(res$messages[[2]],
               "Saved public key")
  mockery::expect_called(mock_key_set, 1)
  mockery::expect_called(mock_resolve, 1)
})
