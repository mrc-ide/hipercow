# Get task result

Get the task result. This might be an error if the task has failed.

## Usage

``` r
task_result(id, follow = TRUE, root = NULL)
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

The value of the queued expression

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

# Typical usage
id <- task_create_expr(runif(1))
#> ✔ Submitted task 'ed54482e03540150331720d072eff3d9' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> [1] 0.9612752

# Tasks that error return error values as results
id <- task_create_expr(readRDS("nosuchfile.rds"))
#> ✔ Submitted task '57f10ac0b0982935db3210f5ac0f38f3' using 'example'
task_wait(id)
#> [1] FALSE
task_result(id)
#> <simpleError in gzfile(file, "rb"): cannot open the connection>

cleanup()
#> ℹ Cleaning up example
```
