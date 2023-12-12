test_that("Can create configuration", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  fs::dir_create(path)
  shares <- path_mapping("home", mount, "//host/share/path", "X:")
  config <- withr::with_dir(path, windows_configure(shares, "4.3.0"))
  expect_setequal(
    names(config),
    c("cluster", "template", "shares", "r_version",
      "path_lib", "path_bootstrap"))
  expect_equal(config$cluster, "wpia-hn")
  expect_equal(config$template, "AllNodes")
  expect_equal(config$shares, list(shares))
  expect_equal(config$r_version, numeric_version("4.3.0"))
  expect_equal(config$path_lib, "hermod/lib/windows/4.3.0")
  expect_equal(config$path_bootstrap, "I:/bootstrap/4.3.0")
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
  root <- suppressMessages(hermod::hermod_init(path))
  shares <- path_mapping("home", mount, "//host/share/path", "X:")
  cmp <- withr::with_dir(path, windows_configure(shares, "4.3.0"))

  hermod::hermod_configure(driver = "windows",
                           shares = shares,
                           r_version = "4.3.0",
                           root = root)
  expect_equal(root$config$windows, cmp)
})
