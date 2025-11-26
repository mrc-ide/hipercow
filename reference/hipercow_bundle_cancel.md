# Cancel bundle tasks

Cancel all tasks in a bundle. This wraps
[task_cancel](https://mrc-ide.github.io/hipercow/reference/task_cancel.md)
for all the ids.

## Usage

``` r
hipercow_bundle_cancel(bundle, follow = TRUE, root = NULL)
```

## Arguments

- bundle:

  Either a `hipercow_bundle` object, or the name of a bundle.

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A logical vector the same length as `id` indicating if the task was
cancelled. This will be `FALSE` if the job was already completed, not
running, etc.

## Examples

``` r
cleanup <- hipercow_example_helper(runner = FALSE)
#> ℹ This example uses a special helper

bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'indistinct_indianspinyloach' with 5 tasks
hipercow_bundle_cancel(bundle)
#> ✔ Successfully cancelled 5 tasks
#> [1] TRUE TRUE TRUE TRUE TRUE
hipercow_bundle_status(bundle)
#> [1] "cancelled" "cancelled" "cancelled" "cancelled" "cancelled"

cleanup()
#> ℹ Cleaning up example
```
