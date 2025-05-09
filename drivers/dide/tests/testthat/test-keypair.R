test_that("can generate a keypair", {
  tmp <- withr::local_tempdir()

  mock_username <- mockery::mock("bob")
  mock_key_get <- mockery::mock(stop("no key"))
  mock_key_set <- mockery::mock()
  mock_resolve <- mockery::mock(tmp)

  mockery::stub(dide_generate_keypair, "dide_username",
                mock_username)
  mockery::stub(dide_generate_keypair, "keyring::key_get",
                mock_key_get)
  mockery::stub(dide_generate_keypair, "keyring::key_set_with_value",
                mock_key_set)
  mockery::stub(dide_generate_keypair, "dide_locally_resolve_unc_path",
                mock_resolve)

  res <- evaluate_promise(dide_generate_keypair())

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
               list("//qdrive.dide.ic.ac.uk/homes/bob"))

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

  mockery::stub(dide_generate_keypair, "dide_username",
                mock_username)
  mockery::stub(dide_generate_keypair, "keyring::key_get",
                mock_key_get)
  mockery::stub(dide_generate_keypair, "keyring::key_set_with_value",
                mock_key_set)
  mockery::stub(dide_generate_keypair, "dide_locally_resolve_unc_path",
                mock_resolve)

  expect_error(
    dide_generate_keypair(),
    "Can't generate a keypair as I failed to find your home drive")

  mockery::expect_called(mock_key_set, 0)
})


test_that("don't generate keypair if not needed", {
  tmp <- withr::local_tempdir()
  mock_username <- mockery::mock("bob", cycle = TRUE)
  mock_key_get <- mockery::mock(TRUE, cycle = TRUE)
  mock_key_set <- mockery::mock()
  mock_resolve <- mockery::mock(tmp, cycle = TRUE)

  mockery::stub(dide_generate_keypair, "dide_username",
                mock_username)
  mockery::stub(dide_generate_keypair, "keyring::key_get",
                mock_key_get)
  mockery::stub(dide_generate_keypair, "keyring::key_set_with_value",
                mock_key_set)
  mockery::stub(dide_generate_keypair, "dide_locally_resolve_unc_path",
                mock_resolve)

  res <- evaluate_promise(dide_generate_keypair())
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "Not generating a new keypair as existing keypair detected")
  expect_match(res$messages[[2]],
               "If you have deleted")
  mockery::expect_called(mock_key_set, 0)
  mockery::expect_called(mock_resolve, 0)

  res <- evaluate_promise(dide_generate_keypair(update = TRUE))
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "Created new private key")
  expect_match(res$messages[[2]],
               "Saved public key")
  mockery::expect_called(mock_key_set, 1)
  mockery::expect_called(mock_resolve, 1)
})


test_that("can fetch keypair", {
  pubkey <- "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1iRc3clrEeZNPaaPrTL"
  mock_key_get <- mockery::mock(stop("some error"), pubkey)
  mock_username <- mockery::mock("bob", cycle = TRUE)

  mockery::stub(dide_keypair, "keyring::key_get", mock_key_get)
  mockery::stub(dide_keypair, "dide_username", mock_username)

  err <- expect_error(dide_keypair(config, path_root),
                      "Did not find your DIDE public key")
  expect_equal(conditionMessage(err$parent), "some error")
  expect_equal(
    err$body,
    c(i = "Please run 'dide_keypair_generate()' to generate a keypair"))
  mockery::expect_called(mock_username, 1)
  mockery::expect_called(mock_key_get, 1)
  expect_equal(mockery::mock_args(mock_key_get)[[1]],
               list("hipercow/dide/pubkey", username = "bob"))

  res <- dide_keypair(config, path_root)
  mockery::expect_called(mock_username, 2)
  mockery::expect_called(mock_key_get, 2)
  expect_equal(mockery::mock_args(mock_key_get)[[2]],
               mockery::mock_args(mock_key_get)[[1]])

  expect_equal(
    res,
    list(pub = pubkey,
         key = "//qdrive.dide.ic.ac.uk/homes/bob/.hipercow/key"))
})
