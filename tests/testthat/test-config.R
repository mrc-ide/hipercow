test_that("defaults are sensible", {
  res <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config_defaults())
  non_null <- c("cluster", "use_java")
  null <- c("home", "temp", "shares", "template", "cores",
            "workdir", "wholenode", "r_version", "java_home")
  i <- vlapply(res, is.null)
  expect_true("credentials" %in% names(res)) # might be here or not
  expect_setequal(setdiff(names(res)[i], "credentials"), null)
  expect_setequal(setdiff(names(res)[!i], "credentials"), non_null)
  expect_equal(res$cluster, "fi--dideclusthn") # old cluster
  expect_false(res$use_java)
})

test_that("use didehpc.username if needed and available", {
  blank <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config_defaults())
  res1 <- withr::with_options(
    tmp_options_didehpc(didehpc.credentials = "bob"),
    didehpc_config_defaults())
  res2 <- withr::with_options(
    tmp_options_didehpc(didehpc.username = "bob"),
    didehpc_config_defaults())
  res3 <- withr::with_options(
    tmp_options_didehpc(didehpc.username = "alice",
                        didehpc.credentials = "bob"),
    didehpc_config_defaults())

  expect_equal(res1$credentials, "bob")
  expect_mapequal(res1[names(res1) != "credentials"],
                  res2[names(res2) != "credentials"])
  expect_equal(res2, res1)
  expect_equal(res3, res1)
})


test_that("Try and get the username out of windows", {
  mock_is_windows <- mockery::mock(FALSE, TRUE)
  mockery::stub(didehpc_config_defaults, "is_windows", mock_is_windows)

  withr::with_envvar(c(USERNAME = "bob"), {
    res_other <- withr::with_options(
      tmp_options_didehpc(),
      didehpc_config_defaults())
    res_win <- withr::with_options(
      tmp_options_didehpc(),
      didehpc_config_defaults())
  })
  expect_null(res_other$credentials)
  expect_equal(res_win$credentials, "bob")
})


test_that("valid clusters is correct", {
  expect_equal(valid_clusters(), c("fi--dideclusthn", "fi--didemrchnb",
                                   "wpia-hn"))
})


test_that("Check that resources are acceptable", {
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", 1, FALSE),
    list(template = "GeneralNodes", count = 1L, type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, NULL),
    list(template = "GeneralNodes", count = 1L, type = "Cores"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, TRUE),
    list(template = "GeneralNodes", count = 1L, type = "Nodes"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", NULL, TRUE),
    list(template = "GeneralNodes", count = 1L, type = "Nodes"))
  expect_equal(
    check_resources("fi--dideclusthn", "GeneralNodes", 8, FALSE),
    list(template = "GeneralNodes", count = 8L, type = "Cores"))
  expect_error(
    check_resources("fi--dideclusthn", "GeneralNodes", 9001, FALSE),
    "Maximum number of cores for fi--dideclusthn is 24")
  expect_error(
    check_resources("fi--didemrchnb", "GeneralNodes", 9001, FALSE),
    "Maximum number of cores for fi--didemrchnb is 64")

  expect_error(
    check_resources("fi--dideclusthn", "GeneralNodes", 2, TRUE),
    "Cannot specify both wholenode and cores")
})


test_that("default core/node choice consistent across clusters", {
  expect_mapequal(
    check_resources("fi--didemrchnb", "GeneralNodes", NULL, NULL),
    list(template = "GeneralNodes", count = 1L, type = "Cores"))
  expect_mapequal(
    check_resources("fi--didemrchnb", "32Core", NULL, NULL),
    list(template = "32Core", count = 1L, type = "Cores"))
  expect_mapequal(
    check_resources("wpia-hn", "AllNodes", NULL, NULL),
    list(template = "AllNodes", count = 1L, type = "Cores"))
})


test_that("Build config", {
  root <- tempfile()
  mounts <- example_mounts(root)
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  workdir <- file.path(root, "home", "sub")
  cfg <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config(credentials = list(username = "bob", password = "secret"),
                   workdir = workdir))
  mockery::expect_called(mock_detect_mount, 1L)
  expect_equal(
    mockery::mock_args(mock_detect_mount), list(list()))

  expect_s3_class(cfg, "didehpc_config")
  str <- capture.output(print(cfg))
  expect_match(str, "<didehpc_config>", all = FALSE)
  expect_match(str, " - username: bob", all = FALSE)
  expect_match(str, " - password: \\*+$", all = FALSE)
  expect_match(str, "    - type: Cores", all = FALSE)
})


test_that("Build config", {
  root <- tempfile()
  mounts <- example_mounts(root)
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  workdir <- file.path(root, "home", "sub")
  cfg <- withr::with_options(
    tmp_options_didehpc(),
    didehpc_config(credentials = "bob", workdir = workdir))
  mockery::expect_called(mock_detect_mount, 1L)
  expect_equal(
    mockery::mock_args(mock_detect_mount), list(list()))

  expect_s3_class(cfg, "didehpc_config")
  str <- capture.output(print(cfg))
  expect_match(str, "<didehpc_config>", all = FALSE)
  expect_match(str, " - username: bob", all = FALSE)
  expect_match(str, "    - type: Cores", all = FALSE)
})



test_that("config getter requires sensible args", {
  expect_error(
    as_didehpc_config("bob"),
    "Expected a didehpc_config for 'config'")
})


test_that("config getter tries to construct options", {
  root <- tempfile()
  config <- example_config()
  mock_do_call <- mockery::mock(config)
  mockery::stub(as_didehpc_config, "do.call", mock_do_call)
  cmp <- withr::with_options(
    tmp_options_didehpc(),
    as_didehpc_config(list(credentials = "bob")))
  expect_equal(cmp, config)
  mockery::expect_called(mock_do_call, 1L)
  expect_equal(
    mockery::mock_args(mock_do_call)[[1]],
    list("didehpc_config", list(credentials = "bob")))
})


test_that("java options", {
  root <- tempfile()
  cfg1 <- example_config(root = tempfile())
  expect_false(cfg1$use_java)
  expect_null(cfg1$java_home)

  cfg2 <- example_config(use_java = TRUE, root = tempfile())
  expect_true(cfg2$use_java)
  expect_equal(cfg2$java_home, "")

  cfg3 <- example_config(use_java = TRUE, java_home = "T:/java",
                         root = tempfile())
  expect_true(cfg3$use_java)
  expect_equal(cfg3$java_home, "T:/java")
})


test_that("fetch r versions", {
  testthat::skip_if_offline()
  dat <- r_versions()
  expect_s3_class(dat, "numeric_version")
  expect_true(numeric_version("4.3.0") %in% dat)
  expect_true(length(dat) > 3)
})


test_that("Select a sensible r version", {
  testthat::skip_if_offline()
  v <- r_versions()
  vmax <- max(v)
  vmid <- v[length(v) - 3]
  expect_equal(select_r_version(vmax), vmax)
  expect_error(select_r_version("3.6.0"),
               "Unsupported R version: 3.6.0")
  expect_equal(select_r_version(NULL, vmid), vmid)
  expect_equal(select_r_version(NULL, "4.1.0"), numeric_version("4.1.3"))
})


test_that("workdir must exist", {
  root <- tempfile()
  mounts <- example_mounts(root)
  workdir <- file.path(root, "home", "sub")
  mock_detect_mount <- mockery::mock(mounts)
  mockery::stub(didehpc_config, "detect_mount", mock_detect_mount)
  withr::with_options(
    tmp_options_didehpc(),
    expect_error(
      didehpc_config(credentials = "bob", workdir = tempfile()),
      "workdir must be an existing directory"))
})
