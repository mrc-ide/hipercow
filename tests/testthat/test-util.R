test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("can ensure we have a package", {
  ns <- ensure_package("testthat")
  expect_identical(ns, getNamespace("testthat"))
})


test_that("can fail if namespace not available", {
  withr::local_options(hipercow.auto_install_missing_packages = FALSE)
  err <- expect_error(
    ensure_package("hipercow.area51"),
    "Package 'hipercow.area51' is not available")
  expect_length(err$body, 2)
  expect_match(
    err$body[[1]],
    "Please try installing 'hipercow.area51' by running")
  expect_match(
    err$body[[2]],
    "To automatically install missing packages, set options")
})


test_that("can install missing packages if wanted", {
  withr::local_options(hipercow.auto_install_missing_packages = NULL)

  mock_require_namespace <- mockery::mock(FALSE, TRUE)
  mock_install_packages <- mockery::mock()
  mock_get_namespace <- mockery::mock()
  mockery::stub(ensure_package, "requireNamespace", mock_require_namespace)
  mockery::stub(ensure_package, "utils::install.packages",
                mock_install_packages)
  mockery::stub(ensure_package, "getNamespace", mock_get_namespace)

  msg <- capture_messages(ensure_package("hipercow.area51"))
  expect_match(msg, "Trying to install 'hipercow.area51'", all = FALSE)
  expect_match(msg, "To prevent this, set options", all = FALSE)
  expect_match(msg, "Installation of 'hipercow.area51' successful", all = FALSE)

  mockery::expect_called(mock_require_namespace, 2)
  expect_equal(mockery::mock_args(mock_require_namespace),
               rep(list(list("hipercow.area51", quietly = TRUE)), 2))

  mockery::expect_called(mock_install_packages, 1)
  expect_equal(
    mockery::mock_args(mock_install_packages)[[1]],
    list("hipercow.area51", repos = c("https://mrc-ide.r-universe.dev",
                                    CRAN = "https://cloud.r-project.org")))

  mockery::expect_called(mock_get_namespace, 1)
  expect_equal(mockery::mock_args(mock_get_namespace),
               list(list("hipercow.area51")))
})


test_that("can error if missing package installation fails", {
  withr::local_options(hipercow.auto_install_missing_packages = NULL)

  mock_require_namespace <- mockery::mock(FALSE, FALSE)
  mock_install_packages <- mockery::mock()
  mock_get_namespace <- mockery::mock()
  mockery::stub(ensure_package, "requireNamespace", mock_require_namespace)
  mockery::stub(ensure_package, "utils::install.packages",
                mock_install_packages)
  mockery::stub(ensure_package, "getNamespace", mock_get_namespace)

  err <- expect_error(
    suppressMessages(ensure_package("hipercow.area51")),
    "Installation of 'hipercow.area51' failed!")
  expect_length(err$body, 1)
  expect_match(err$body, "Please try installing 'hipercow.area51' by running")

  mockery::expect_called(mock_require_namespace, 2)
  expect_equal(mockery::mock_args(mock_require_namespace),
               rep(list(list("hipercow.area51", quietly = TRUE)), 2))

  mockery::expect_called(mock_install_packages, 1)
  expect_equal(
    mockery::mock_args(mock_install_packages)[[1]],
    list("hipercow.area51", repos = c("https://mrc-ide.r-universe.dev",
                                    CRAN = "https://cloud.r-project.org")))

  mockery::expect_called(mock_get_namespace, 0)
})


test_that("saverds_if_different updates files when different", {
  path <- withr::local_tempfile()
  expect_true(saverds_if_different(list(1, "two"), path))
  expect_equal(readRDS(path), list(1, "two"))

  expect_true(saverds_if_different(list(1, "two", 3L), path))
  expect_equal(readRDS(path), list(1, "two", 3L))
})


test_that("saverds_if_different does not update file when not different", {
  path <- withr::local_tempfile()
  saveRDS(list(1, "two"), path)
  mock_saverds <- mockery::mock()
  mockery::stub(saverds_if_different, "saveRDS", mock_saverds)
  expect_false(saverds_if_different(list(1, "two"), path))
  mockery::expect_called(mock_saverds, 0)
})


test_that("can format bytes", {
  expect_equal(format_bytes(100), "100 bytes")
  expect_equal(format_bytes(999), "999 bytes")
  expect_equal(format_bytes(1000), "1 kB")
  expect_equal(format_bytes(999999), "999.999 kB")
  expect_equal(format_bytes(1000000), "1 MB")
  expect_equal(format_bytes(1000000000), "1000 MB")
})


test_that("can select reasonable progress defaults", {
  withr::with_options(list(hipercow.progress = NULL), {
    expect_true(show_progress(TRUE))
    expect_false(show_progress(FALSE))
    expect_true(rlang::with_interactive(show_progress(NULL), TRUE))
    expect_false(rlang::with_interactive(show_progress(NULL), FALSE))
  })
  withr::with_options(list(hipercow.progress = TRUE), {
    expect_true(show_progress(TRUE))
    expect_false(show_progress(FALSE))
    expect_true(rlang::with_interactive(show_progress(NULL), TRUE))
    expect_true(rlang::with_interactive(show_progress(NULL), FALSE))
  })
  withr::with_options(list(hipercow.progress = FALSE), {
    expect_true(show_progress(TRUE))
    expect_false(show_progress(FALSE))
    expect_false(rlang::with_interactive(show_progress(NULL), TRUE))
    expect_false(rlang::with_interactive(show_progress(NULL), FALSE))
  })
})


test_that("deparse long expressions nicely", {
  expr <- quote(some_func(arg1, long_arg, another_arg, and_another,
                          and_one_more, plus_more, and_then_more,
                          and_more_again))
  expect_gt(length(deparse(expr)), 1)
  res <- deparse_simple(expr)
  expect_length(res, 1)
  expect_match(res, "^some_func\\(arg1, long_arg, .+\\[\\.{3}\\]$")
  expect_lt(nchar(res), 65)
})


test_that("can summarise warnings", {
  expect_silent(show_collected_warnings(NULL))
  expect_silent(show_collected_warnings(list()))

  w <- lapply(c("a", "b", "c"), function(i) simpleWarning(strrep(i, 3)))

  msg <- capture_messages(show_collected_warnings(w[1]))
  expect_length(msg, 2)
  expect_match(msg[[1]], "1 warning found:\n")
  expect_match(msg[[2]], "\\baaa\n")

  msg <- capture_messages(show_collected_warnings(w[c(1, 1, 1)]))
  expect_length(msg, 2)
  expect_match(msg[[1]], "3 warnings found:\n")
  expect_match(msg[[2]], "\\baaa \\(3 times\\)\n")

  msg <- capture_messages(show_collected_warnings(w[1:2]))
  expect_length(msg, 3)
  expect_match(msg[[1]], "2 warnings found:\n")
  expect_match(msg[[2]], "\\baaa\n")
  expect_match(msg[[3]], "\\bbbb\n")

  msg <- capture_messages(show_collected_warnings(w[c(1, 1, 1, 2, 1, 1, 2, 2)]))
  expect_length(msg, 5)
  expect_match(msg[[1]], "8 warnings found:\n")
  expect_match(msg[[2]], "\\baaa \\(3 times\\)\n")
  expect_match(msg[[3]], "\\bbbb\n")
  expect_match(msg[[4]], "\\baaa \\(2 times\\)\n")
  expect_match(msg[[5]], "\\bbbb \\(2 times\\)\n")

  msg <- withr::with_options(
    list(nwarnings = 2),
    capture_messages(show_collected_warnings(w[c(1, 1, 1, 2, 1, 1, 2, 2)])))
  expect_length(msg, 4)
  expect_match(msg[[1]], "8 warnings found:\n")
  expect_match(msg[[2]], "\\baaa \\(2 times\\)\n")
  expect_match(msg[[3]], "\\bbbb \\(2 times\\)\n")
  expect_match(msg[[4]], "\\Only last 2 distinct warnings shown\n")
})
