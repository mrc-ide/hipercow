test_that("can run bootstrap", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  mock_hipercow_provision <- mockery::mock()
  mockery::stub(bootstrap_update, "hipercow::hipercow_provision",
                mock_hipercow_provision)

  bootstrap_update(root)
  mockery::expect_called(mock_hipercow_provision, 1)
  expect_true(file.exists(
    file.path(root$path$root, "hipercow", "bootstrap-windows.R")))
  expect_equal(
    mockery::mock_args(mock_hipercow_provision)[[1]],
    list("script", script = "hipercow/bootstrap-windows.R", root = root))
})
