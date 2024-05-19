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
  v <- numeric_version(c("4.0.5", "4.1.3", "4.2.3", "4.3.0"))
  vmax <- numeric_version("4.3.0")
  vmid <- numeric_version("4.2.3")
  expect_equal(select_r_version(vmax, valid = v), vmax)
  expect_error(select_r_version(numeric_version("3.6.0"), valid = v),
               "Unsupported R version: 3.6.0")
  expect_equal(select_r_version(NULL, ours = vmid, valid = v), vmid)
  expect_equal(
    select_r_version(NULL, ours = numeric_version("4.2.0"), valid = v),
    numeric_version("4.2.3"))
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


test_that("can warn about old R versions", {
  valid <- numeric_version(
    c("4.2.3", "4.3.0", "4.3.2", "4.3.3", "4.4.0"))
  current <- numeric_version("4.4.0")
  ok <- numeric_version("4.3.2")
  old <- numeric_version("4.2.3")
  ancient <- numeric_version("4.1.3")

  expect_silent(
    check_old_versions(valid, current, current))
  expect_silent(
    check_old_versions(valid, ok, ok))
  expect_silent(
    check_old_versions(valid, ok, old))

  res <- evaluate_promise(check_old_versions(valid, old, old))
  expect_length(res$messages, 4)
  expect_match(
    res$messages[[1]],
    "Selected an old R version '4.2.3'")
  expect_match(
    res$messages[[3]],
    "minimum recommended version is '4.3.0'")
  expect_match(res$messages[[4]], "4.4.0", fixed = TRUE)

  res2 <- evaluate_promise(check_old_versions(valid, old, ancient))
  expect_length(res2$messages, 5)
  expect_equal(res2$messages[1:4], res$messages)
  expect_match(
    res2$messages[[5]],
    "Your local R installation is very old")
})
