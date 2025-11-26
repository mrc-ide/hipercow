# Cancel tasks

Cancel one or more tasks

## Usage

``` r
task_cancel(id, follow = TRUE, root = NULL)
```

## Arguments

- id:

  The task id or task ids to cancel

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A logical vector the same length as `id` indicating if the task was
cancelled. This will be `FALSE` if the task was already completed, not
running, etc.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

ids <- c(task_create_expr(Sys.sleep(2)), task_create_expr(runif(1)))
#> ✔ Submitted task '1f688244040901f1cf292fbc0e525dbc' using 'example'
#> ✔ Submitted task '7069de69ab4f445a1a487cfcc3e305b8' using 'example'

# The first task may or not be cancelled (depends on if it was
# started already) but the second one will almost certainly be
# cancelled:
task_cancel(ids)
#> ✔ Successfully cancelled 2 tasks
#> [1] TRUE TRUE

cleanup()
#> ℹ Cleaning up example
```
