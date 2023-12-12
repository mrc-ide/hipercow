test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("sys_which throws on unknown exe", {
  expect_error(sys_which("unknowncommand"),
               "unknowncommand not found in $PATH",
               fixed = TRUE)
})


test_that("system_intern_check copes with R's weirdnesses", {
  sys <- function(outcome) {
    if (outcome == "success") {
      "result"
    } else if (outcome == "failure1") {
      warning("failure")
      structure("result", status = 1)
    } else if (outcome == "failure2") {
      stop("failure")
    }
  }

  mock_system <- mockery::mock(sys("success"),
                               sys("failure1"),
                               sys("failure2"))
  mockery::stub(system_intern_check, "system", mock_system)
  expect_equal(system_intern_check("some command"), "result")
  expect_error(system_intern_check("some command"), "Error running command")
  expect_error(system_intern_check("some command"), "failure")

  mockery::expect_called(mock_system, 3)
  expect_equal(mockery::mock_args(mock_system),
               rep(list(list("some command", intern = TRUE)), 3))
})


test_that("hermod file errors if files are not found", {
  expect_equal(basename(hermod_windows_file("templates/task_run.bat")),
               "task_run.bat")
  expect_error(hermod_windows_file("template/task_run.bat"))
})


test_that("can look up system username", {
  mock_is_windows <- mockery::mock(TRUE, FALSE, cycle = TRUE)
  mockery::stub(get_system_username, "is_windows", mock_is_windows)

  withr::with_envvar(c(USERNAME = NA, USER = NA), {
    expect_equal(get_system_username(), NA_character_)
    expect_equal(get_system_username(), NA_character_)
  })

  withr::with_envvar(c(USERNAME = "alice", USER = NA), {
    expect_equal(get_system_username(), "alice")
    expect_equal(get_system_username(), NA_character_)
  })

  withr::with_envvar(c(USERNAME = NA, USER = "bob"), {
    expect_equal(get_system_username(), NA_character_)
    expect_equal(get_system_username(), "bob")
  })

  withr::with_envvar(c(USERNAME = "alice", USER = "bob"), {
    expect_equal(get_system_username(), "alice")
    expect_equal(get_system_username(), "bob")
  })
})


test_that("can validate interactive parameters", {
  mock_readline <- mockery::mock("alice", "", cycle = TRUE)
  mockery::stub(readline_with_default, "readline", mock_readline)
  expect_equal(
    readline_with_default("enter username", "bob"),
    "alice")
  expect_equal(
    readline_with_default("enter username", "bob"),
    "bob")
  expect_equal(
    readline_with_default("enter username", NA_character_),
    "alice")
  expect_error(
    readline_with_default("enter username", NA_character_),
    "A value must be provided")

  mockery::expect_called(mock_readline, 4)
  expect_equal(mockery::mock_args(mock_readline)[[1]],
               list("enter username (default: bob) > "))
  expect_equal(mockery::mock_args(mock_readline)[[2]],
               list("enter username (default: bob) > "))
  expect_equal(mockery::mock_args(mock_readline)[[3]],
               list("enter username > "))
  expect_equal(mockery::mock_args(mock_readline)[[4]],
               list("enter username > "))
})


test_that("can transform a string", {
  template <- "hello {{input}} world"
  expect_equal(glue_whisker(template, list(input = "glue")),
               "hello glue world")
  expect_equal(glue_whisker(template, list(input = NULL)),
               "hello  world")
})


test_that("readlines from file if exists returns null if file missing", {
  path <- withr::local_tempfile()
  expect_null(readlines_if_exists(path))
  writeLines(c("a", "b"), path)
  expect_equal(readlines_if_exists(path), c("a", "b"))
})


test_that("writelines_if_not_exists updates files when different", {
  path <- withr::local_tempfile()
  writelines_if_different(c("a", "b"), path)
  expect_equal(readLines(path), c("a", "b"))

  writelines_if_different(c("a", "b", "c"), path)
  expect_equal(readLines(path), c("a", "b", "c"))
})


test_that("writelines_if_not_exists does not update file when not different", {
  path <- withr::local_tempfile()
  writeLines(c("a", "b"), path)
  mock_writelines <- mockery::mock()
  mockery::stub(writelines_if_different, "writeLines", mock_writelines)
  writelines_if_different(c("a", "b"), path)
  mockery::expect_called(mock_writelines, 0)
})
