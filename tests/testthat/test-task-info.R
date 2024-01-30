test_that("can format a call", {
  expect_equal(
    task_info_call_fn(list(name = NULL, namespace = NULL)),
    "(anonymous)")
  expect_equal(
    task_info_call_fn(list(name = "foo", namespace = NULL)),
    "foo")
  expect_equal(
    task_info_call_fn(list(name = "foo", namespace = "ns")),
    "ns::foo")
})


test_that("can format args", {
  expect_equal(
    task_info_call_args(list()),
    "(none)")
  expect_equal(
    task_info_call_args(list(1, 2, 3)),
    "1, 2, 3")
  expect_equal(
    task_info_call_args(list(1, b = 2, x = letters)),
    "1, b = 2, x = <chr: \"a\", \"b\", \"c\", \"d\", \"e\", ...>")
})
