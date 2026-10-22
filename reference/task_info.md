# Fetch task information

Fetch information about a task. This is much more detailed than the
information in `task_status`. If a task is running we also fetch the
true status via its driver, which can be slower.

## Usage

``` r
task_info(id, follow = TRUE, root = NULL)
```

## Arguments

- id:

  A single task id to fetch information for

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

An object of class `hipercow_task_info`, which will print nicely. This
is just a list with elements:

- `id`: the task identifier

- `status`: the retrieved status

- `driver`: the driver used to run the task (or NA)

- `data`: the task data (depends on the type of task)

- `times`: a vector of times

- `retry_chain`: the retry chain (or `NULL`)

You can see and access these elements more easily by running
[`unclass()`](https://rdrr.io/r/base/class.html) on the result of
`task_info()`.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
id <- task_create_expr(runif(1))
#> ✔ Submitted task '1f1663467fc7e37a4327df3cdf77fac3' using 'example'
task_wait(id)
#> [1] TRUE

# Task information at completion includes times:
task_info(id)
#> 
#> ── task 1f1663467fc7e37a4327df3cdf77fac3 (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: runif(1)
#>   • Locals: (none)
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20261022-1a6a49e8f62/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20261022-1a6a49e8f62/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2026-10-22 12:05:59.53653 (moments ago)
#> ℹ Started at 2026-10-22 12:05:59.619656 (moments ago; waited 84ms)
#> ℹ Finished at 2026-10-22 12:05:59.705154 (moments ago; ran for 86ms)

# If you need to work with these times, use the "times" element:
task_info(id)$times
#>                   created                   started                  finished 
#> "2026-10-22 12:05:59 UTC" "2026-10-22 12:05:59 UTC" "2026-10-22 12:05:59 UTC" 

# If a task is retried, this information appears as a retry chain:
id2 <- task_retry(id)
#> ✔ Submitted task '390db39ccf0d0acaabd5ce1361ee4243' using 'example'
task_info(id2, follow = FALSE)
#> 
#> ── task 390db39ccf0d0acaabd5ce1361ee4243 (submitted) ───────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: runif(1)
#>   • Locals: (none)
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20261022-1a6a49e8f62/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20261022-1a6a49e8f62/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2026-10-22 12:05:59.53653 (moments ago)
#> ! Not started yet (waiting for 1.1s)
#> ! Not finished yet (waiting to start)
#> ℹ Last of a chain of a task retried 1 time
task_info(id2)
#> 
#> ── task 390db39ccf0d0acaabd5ce1361ee4243 (submitted) ───────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: runif(1)
#>   • Locals: (none)
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20261022-1a6a49e8f62/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20261022-1a6a49e8f62/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2026-10-22 12:05:59.53653 (moments ago)
#> ! Not started yet (waiting for 1.2s)
#> ! Not finished yet (waiting to start)
#> ℹ Last of a chain of a task retried 1 time

cleanup()
#> ℹ Cleaning up example
```
