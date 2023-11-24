test_that("Can create configuration", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  fs::dir_create(path)
  shares <- path_mapping("home", mount, "//host/share/path", "X:")
  config <- hermod_config_create(path, shares, "4.3.0")
  expect_s3_class(config, "hermod_config")
  expect_setequal(
    names(config),
    c("cluster", "template", "shares", "r_version"))
  expect_equal(config$cluster, "wpia-hn")
  expect_equal(config$template, "AllNodes")
  expect_equal(config$shares, list(shares))
  expect_equal(config$r_version, numeric_version("4.3.0"))
})


test_that("Select a sensible r version", {
  v <- r_versions()
  vmax <- max(v)
  vmid <- v[length(v) - 3]
  expect_equal(select_r_version(vmax), vmax)
  expect_error(select_r_version("3.6.0"),
               "Unsupported R version: 3.6.0")
  expect_equal(select_r_version(NULL, vmid), vmid)
  expect_equal(select_r_version(NULL, "4.1.0"), numeric_version("4.1.3"))
})


test_that("can configure a root", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- init_quietly(path)
  shares <- path_mapping("home", mount, "//host/share/path", "X:")
  result <- withr::with_dir(
    path,
    withVisible(hermod_configure(shares = shares)))
  expect_false(result$visible)
  expect_equal(result$value, hermod_config_create(path, shares, NULL))

  expect_equal(hermod_config(path), result$value)
  rm(list = "roots", envir = cache)
  expect_equal(hermod_config(path), result$value)
})


test_that("roots don't start with a configuration", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- init_quietly(path)
  err <- expect_error(hermod_config(path),
                      "This hermod root is not configured")
  expect_equal(err$body, c(i = "Please run 'hermod_configure()'"))
})
