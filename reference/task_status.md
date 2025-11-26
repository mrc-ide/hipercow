# Get task status

Get the status of a task. See Details for the lifecycle.

## Usage

``` r
task_status(id, follow = TRUE, root = NULL)
```

## Arguments

- id:

  The task identifier

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A string with the task status. Tasks that do not exist will have a
status of `NA`.

## Details

A task passes through a lifecycle:

- `created`

- `submitted`

- `running`

- `success`, `failure`, `cancelled`

These occur in increasing order and the result of this function is the
furthest through this list.

Later, we will introduce other types to cope with tasks that are blocked
on dependencies (or have become impossible due to failed dependencies).

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

ids <- c(task_create_expr(runif(1)), task_create_expr(runif(1)))
#> ✔ Submitted task '3af7ec06abb0df7550720f5d5f93ce98' using 'example'
#> ✔ Submitted task '3d940b61fbe8ae2ea1b53c81ec256ae3' using 'example'
# Depending on how fast these tasks get picked up they will be one
# of 'submitted', 'running' or 'success':
task_status(ids)
#> [1] "submitted" "submitted"

# Wait until both tasks are complete
task_wait(ids[[1]])
#> [1] TRUE
task_wait(ids[[2]])
#> [1] TRUE
# And both are success now
task_status(ids)
#> [1] "success" "success"

cleanup()
#> ℹ Cleaning up example
```
