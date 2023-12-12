test_that("can run bootstrap", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  mock_hermod_provision <- mockery::mock()
  mockery::stub(bootstrap_update, "hermod::hermod_provision",
                mock_hermod_provision)

  bootstrap_update(root)
  mockery::expect_called(mock_hermod_provision, 1)
  expect_true(file.exists(
    file.path(root$path$root, "hermod", "bootstrap-windows.R")))
  expect_equal(
    mockery::mock_args(mock_hermod_provision)[[1]],
    list("script", script = "hermod/bootstrap-windows.R", root = root))
})
