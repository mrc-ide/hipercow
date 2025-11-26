# Wait for a bundle to complete

Wait for tasks in a bundle to complete. This is the generalisation of
[task_wait](https://mrc-ide.github.io/hipercow/reference/task_wait.md)
for a bundle.

## Usage

``` r
hipercow_bundle_wait(
  bundle,
  timeout = NULL,
  poll = 1,
  fail_early = TRUE,
  progress = NULL,
  follow = TRUE,
  root = NULL
)
```

## Arguments

- bundle:

  Either a `hipercow_bundle` object, or the name of a bundle.

- timeout:

  The time to wait for the task to complete. The default is to wait
  forever.

- poll:

  Time, in seconds, used to throttle calls to the status function. The
  default is 1 second

- fail_early:

  Logical, indicating if we should fail as soon as the first task has
  failed. In this case, the other running tasks continue running, but we
  return and indicate that the final result will not succeed. If
  `fail_early = FALSE` we keep running until all tasks have passed or
  failed, even though we know we will return `FALSE`; but upon return
  [`hipercow_bundle_result()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_result.md)
  can be called and all results/errors returned.

- progress:

  Logical value, indicating if a progress spinner should be used. The
  default `NULL` uses the option `hipercow.progress`, and if unset
  displays a progress bar in an interactive session.

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A scalar logical value; `TRUE` if *all* tasks complete successfully and
`FALSE` otherwise

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'unangry_krill' with 5 tasks
hipercow_bundle_wait(bundle)
#> [1] TRUE
hipercow_bundle_status(bundle)
#> [1] "success" "success" "success" "success" "success"

cleanup()
#> ℹ Cleaning up example
```
