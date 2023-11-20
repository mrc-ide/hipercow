test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("modify_list updates a list", {
  x <- list(a = 1, b = 2, c = 3)
  expect_equal(modify_list(x, list(b = 20)),
               list(a = 1, b = 20, c = 3))
  expect_error(modify_list(x, list(x = 20)),
               "Unknown elements in .+: x")
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
  expect_equal(basename(hermod_file("templates/task_run.bat")), "task_run.bat")
  expect_error(hermod_file("template/task_run.bat"))
})
