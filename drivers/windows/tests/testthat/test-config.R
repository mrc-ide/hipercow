test_that("Can create configuration", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- suppressMessages(hipercow::hipercow_init(path))
  shares <- windows_path(mount, "//host/share/path", "X:")
  config <- withr::with_dir(path, windows_configure(shares, "4.3.0"))
  expect_setequal(
    names(config),
    c("cluster", "shares", "r_version", "path_lib"))
  expect_equal(config$cluster, "wpia-hn")
  expect_equal(config$shares, structure(list(shares), class = "dide_shares"))
  expect_equal(config$r_version, numeric_version("4.3.0"))
  expect_equal(config$path_lib, "hipercow/lib/windows/4.3.0")
  expect_equal(
    format(config$shares),
    c("1 configured:",
      ">" = as.character(config$shares[[1]])))
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
  root <- suppressMessages(hipercow::hipercow_init(path))
  shares <- windows_path(mount, "//host/share/path", "X:")
  cmp <- withr::with_dir(path, windows_configure(shares, "4.3.0"))

  suppressMessages(
    hipercow::hipercow_configure(driver = "windows",
                                 shares = shares,
                                 r_version = "4.3.0",
                                 root = root))
  expect_equal(root$config$windows, cmp)
})
