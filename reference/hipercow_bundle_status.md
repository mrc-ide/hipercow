# Bundle status

Fetch status for all tasks in a bundle.

## Usage

``` r
hipercow_bundle_status(bundle, reduce = FALSE, follow = TRUE, root = NULL)
```

## Arguments

- bundle:

  Either a `hipercow_bundle` object, or the name of a bundle.

- reduce:

  Reduce the status across all tasks in the bundle. This means we return
  a single value with the "worst" status across the bundle. We only
  return `success` if *all* tasks have succeeded, and will return
  `failed` if any task has failed.

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A character vector the same length as the number of tasks in the bundle,
or length 1 if `reduce` is `TRUE`.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'engaged_iguana' with 5 tasks
# Immediately after submission, tasks may not all be complete so
# we may get a mix of statuses.  In that case the reduced status
# will be "submitted" or "running", even though some tasks may be
# "success"
hipercow_bundle_status(bundle)
#> [1] "submitted" "submitted" "submitted" "submitted" "submitted"
hipercow_bundle_status(bundle, reduce = TRUE)
#> [1] "submitted"

# After completion all tasks have status "success", as does the
# reduction.
hipercow_bundle_wait(bundle)
#> [1] TRUE
hipercow_bundle_status(bundle)
#> [1] "success" "success" "success" "success" "success"
hipercow_bundle_status(bundle, reduce = TRUE)
#> [1] "success"

cleanup()
#> ℹ Cleaning up example
```
