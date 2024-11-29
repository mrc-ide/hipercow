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


test_that("can select reasonable timeout defaults", {
  withr::with_options(list(hipercow.timeout = NULL), {
    expect_equal(timeout_value(NULL), Inf)
    expect_equal(timeout_value(100), 100)
  })
  withr::with_options(list(hipercow.timeout = 10), {
    expect_equal(timeout_value(NULL), 10)
    expect_equal(timeout_value(100), 100)
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


test_that("nice set_names recycling", {
  expect_equal(set_names(c(1, 2), "x"), c(x = 1, x = 2))
  expect_equal(set_names(1, "x"), c(x = 1))
  expect_equal(set_names(numeric(0), "x"),
               structure(numeric(), names = character()))
})


test_that("can convert numbers to ordinals", {
  expect_equal(ordinal(1), "1st")
  expect_equal(ordinal(2), "2nd")
  expect_equal(ordinal(3), "3rd")
  expect_equal(ordinal(4), "4th")
  expect_equal(ordinal(11), "11th")
  expect_equal(ordinal(12), "12th")
  expect_equal(ordinal(13), "13th")
  expect_equal(ordinal(14), "14th")
  expect_equal(ordinal(21), "21st")
  expect_equal(ordinal(22), "22nd")
  expect_equal(ordinal(23), "23rd")
  expect_equal(ordinal(24), "24th")
  expect_equal(ordinal(43221), "43221st")
})



test_that("fall back on simple time ago", {
  mock_require_ns <- mockery::mock(FALSE, cycle = TRUE)
  mockery::stub(time_ago, "requireNamespace", mock_require_ns)
  expect_match(time_ago(Sys.time() - 10), "10 secs ago")
})


test_that("fall back on simple pretty_dt", {
  mock_require_ns <- mockery::mock(FALSE, cycle = TRUE)
  mockery::stub(pretty_dt, "requireNamespace", mock_require_ns)
  dt <- structure(10, class = "difftime", units = "secs")
  expect_match(pretty_dt(dt), "10 secs")
})


test_that("fall back missing value in pretty_dt, time_ago", {
  expect_equal(pretty_dt(NA), "???")
  expect_equal(time_ago(NA), "unknown time ago")
})

test_that("Duration to minutes works", {
  expect_equal(duration_to_minutes(35), 35)
  expect_equal(duration_to_minutes("35"), 35)
  expect_equal(duration_to_minutes("1h"), 60)
  expect_equal(duration_to_minutes("1h1m"), 61)
  expect_equal(duration_to_minutes("1h2m"), 62)
  expect_equal(duration_to_minutes("3h0m"), 180)
  expect_equal(duration_to_minutes("1d3h"), 1620)
  expect_equal(duration_to_minutes("2d"), 2880)
  expect_equal(duration_to_minutes("13h"), 780)
  expect_equal(duration_to_minutes("40d"), 57600)
  expect_equal(duration_to_minutes("11d33h22m"), 17842)
  expect_equal(duration_to_minutes("11D33H22M"), 17842)
})


test_that("report failure in duration to minutes nicely", {
  err <- expect_error(duration_to_minutes("tonight"),
                      "Invalid value for 'testing': tonight")
  expect_equal(err$body[[1]], "Failed to parse string into XhYdZm format")

  err <- expect_error(duration_to_minutes(pi),
                      "Invalid value for 'testing': 3.141")
  expect_equal(err$body[[1]], "'testing' is a non-integer number of minutes")

  err <- expect_error(duration_to_minutes(-5),
                      "Invalid value for 'testing': -5")
  expect_equal(err$body[[1]], "'testing' is a negative number of minutes")

  err <- expect_error(duration_to_minutes(TRUE),
                      "Invalid value for 'testing': TRUE")
  expect_equal(err$body[[1]],
               "'testing' must be a number or a string representing a duration")

  err <- expect_error(duration_to_minutes("0"),
                      "Invalid value for 'testing': 0")
  expect_equal(err$body[[1]],
               "'testing' is zero minutes")
})


test_that("Tonight special works", {
  now <- as_time("2024-01-14 18:31:00")
  ton <- special_time("tonight", now)
  expect_identical(ton, as_time("2024-01-14 19:00:00"))

  now <- as_time("2024-01-15 02:59:00")
  ton <- special_time("tonight", now)
  expect_identical(ton, as_time("2024-01-15 02:59:00"))

  now <- as_time("2024-01-15 03:00:00")
  ton <- special_time("tonight", now)
  expect_identical(ton, as_time("2024-01-15 19:00:00"))
})

test_that("Midnight special works", {
  now <- as_time("2024-01-14 18:31:00")
  ton <- special_time("midnight", now)
  expect_identical(ton, as_time("2024-01-15 00:00:00"))

  now <- as_time("2024-01-15 02:59:00")
  ton <- special_time("midnight", now)
  expect_identical(ton, as_time("2024-01-15 02:59:00"))

  now <- as_time("2024-01-15 03:00:00")
  ton <- special_time("midnight", now)
  expect_identical(ton, as_time("2024-01-16 00:00:00"))
})

test_that("Weekend special works", {
  # Friday night - run at midnight Sat.
  now <- as_time("2024-01-12 18:31:00")
  ton <- special_time("weekend", now)
  expect_identical(ton, as_time("2024-01-13 00:00:00"))

  # Still Sat. You can run now.
  now <- as_time("2024-01-13 18:31:00")
  ton <- special_time("weekend", now)
  expect_identical(ton, as_time("2024-01-13 18:31:00"))

  # Sunday after 6pm... ok...
  now <- as_time("2024-01-14 18:31:00")
  ton <- special_time("weekend", now)
  expect_identical(ton, as_time("2024-01-14 18:31:00"))

  # Monday. Wait til the weekend
  now <- as_time("2024-01-15 18:31:00")
  ton <- special_time("weekend", now)
  expect_identical(ton, as_time("2024-01-20 00:00:00"))
})


test_that("Invalid special causes error", {
  expect_error(special_time("banana"), "Unrecognised special time banana")
})


test_that("can find names in simple expressions", {
  expect_equal(find_vars(quote(f(1))), character(0))
  expect_equal(find_vars(quote(f(x))), "x")
  expect_setequal(find_vars(quote(f(x, y, 2, z))), c("x", "y", "z"))
  expect_equal(find_vars(quote(cls$new(x))), "x")
})


test_that("can find names in multiline expressions with assignments", {
  expect_equal(
    find_vars(quote({
      a <- 1
      f(a)
    })),
    character(0))
  expect_equal(
    find_vars(quote({
      a <- 1
      f(a, x)
    })),
    "x")
  expect_setequal(
    find_vars(quote({
      a <- a + 1
      f(a, x)
    })),
    c("a", "x"))
})


test_that("ignore function args when finding variables", {
  expect_equal(find_vars(quote(f(a, function(x) x + 1))), "a")
  expect_equal(find_vars(quote(f(a, function(x) x + a / b))), c("a", "b"))
  expect_equal(find_vars(quote(f(a, function(x, b = 2) x + a / b))), "a")
})


test_that("is_linux false on windows, mac", {
  testthat::skip_on_os(c("linux", "solaris"))
  expect_false(is_linux())
})


test_that("is_linux true on linux", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  expect_true(is_linux())
})


test_that("hipercow_file works", {
  expect_equal(basename(hipercow_file("comms/moo")),
               "moo")
  expect_error(hipercow_file("comms/baa"))
})


test_that("readlines_if_exists returns NULL if file missing", {
  tmp <- withr::local_tempfile()
  expect_null(readlines_if_exists(tmp))
  writeLines(letters, tmp)
  expect_equal(readlines_if_exists(tmp), letters)
})


test_that("Descend failure", {
  path <- withr::local_tempdir()
  path <- normalize_path(path)
  expect_null(find_directory_descend("foo", tempdir(), path))
  expect_null(find_directory_descend("foo", "/", path))
  expect_null(find_directory_descend("foo", "/", "/"))
})


test_that("Descend success", {
  path <- withr::local_tempdir()
  path <- normalize_path(path)
  fs::dir_create(file.path(path, "foo"))
  fs::dir_create(file.path(path, "a/b/c"))

  expect_equal(find_directory_descend("foo", path, "/"),
               path)
  expect_equal(find_directory_descend("foo", file.path(path, "a"), "/"),
               path)
  expect_equal(find_directory_descend("foo", file.path(path, "a/b/c"), "/"),
               path)
})


test_that("can break a df into rows", {
  d <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_equal(df_rows(d), list(list(x = 1, y = "a"),
                                list(x = 2, y = "b"),
                                list(x = 3, y = "c")))
  expect_equal(df_rows(d[1]), list(list(x = 1),
                                   list(x = 2),
                                   list(x = 3)))
})


test_that("can break a df with list columns into rows", {
  d <- data.frame(x = 1:3,
                  y = c("a", "b", "c"),
                  z = I(lapply(1:3, seq_len)))
  expect_equal(df_rows(d), list(list(x = 1, y = "a", z = seq_len(1)),
                                list(x = 2, y = "b", z = seq_len(2)),
                                list(x = 3, y = "c", z = seq_len(3))))
})


test_that("can find appropriate library with packages", {
  mock_has_package <- mockery::mock(c(FALSE, TRUE), c(TRUE, TRUE))
  mockery::stub(find_library_with, "has_package", mock_has_package)

  res <- find_library_with(c("a", "b"), c("path/1", "path/2", "path/3"))
  expect_equal(res, "path/2")
  mockery::expect_called(mock_has_package, 2)
  expect_equal(mockery::mock_args(mock_has_package)[[1]],
               list(c("a", "b"), "path/1"))
  expect_equal(mockery::mock_args(mock_has_package)[[2]],
               list(c("a", "b"), "path/2"))
})


test_that("can find appropriate library with packages", {
  mock_has_package <- mockery::mock(FALSE, FALSE, FALSE)
  mockery::stub(find_library_with, "has_package", mock_has_package)
  expect_error(
    find_library_with("a", c("path/1", "path/2", "path/3")),
    "Failed to find library containing 'a'")
  mockery::expect_called(mock_has_package, 3)
  expect_equal(mockery::mock_args(mock_has_package)[[1]],
               list("a", "path/1"))
  expect_equal(mockery::mock_args(mock_has_package)[[2]],
               list("a", "path/2"))
  expect_equal(mockery::mock_args(mock_has_package)[[3]],
               list("a", "path/3"))
})


test_that("can check package version", {
  mockery::stub(check_package_version, "package_version_if_installed", NULL)
  expect_error(
    check_package_version("foo", "0.2"),
    "Package foo is not installed. Version 0.2 or greater is required.")

  mockery::stub(check_package_version, "package_version_if_installed",
                numeric_version("0.3"))
  expect_error(
    check_package_version("foo", "0.4"),
    "Version 0.3 of foo is installed, but version 0.4 or greater is required.")
  expect_no_error(check_package_version("foo", "0.3"))
  expect_no_error(check_package_version("foo", "0.2"))
})


test_that("can get environment variables", {
  withr::with_envvar(c("ENV_A" = "a", "ENV_B" = NA_character_), {
    expect_equal(Sys_getenv("ENV_A"), "a")
    expect_error(Sys_getenv("ENV_B"),
                 "Environment variable '$ENV_B' was not set",
                 fixed = TRUE)
  })
})


test_that("detect if we are on github actions", {
  withr::with_envvar(c("CI" = NA_character_, "GITHUB_ACTION" = "foo"), {
    expect_false(on_github_actions())
  })
  withr::with_envvar(c("CI" = "true", "GITHUB_ACTION" = NA_character_), {
    expect_false(on_github_actions())
  })
  withr::with_envvar(c("CI" = "true", "GITHUB_ACTION" = "foo"), {
    expect_true(on_github_actions())
  })
})


test_that("can create a temporary directory path on non-gha machine", {
  testthat::local_mocked_bindings(on_github_actions = function() FALSE)
  p <- hipercow_temporary_directory_path()
  expect_equal(normalize_path(dirname(p)), normalize_path(tempdir()))
  expect_equal(dirname(hipercow_temporary_directory_path("foo")), "foo")
})


test_that("can create a temporary directory path on gha machine", {
  testthat::local_mocked_bindings(on_github_actions = function() TRUE)
  withr::local_envvar(c("RUNNER_TEMP" = "some/path"))
  expect_equal(dirname(hipercow_temporary_directory_path()), "some/path")
  expect_equal(dirname(hipercow_temporary_directory_path("foo")), "foo")
})
