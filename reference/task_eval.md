# Run a task

Run a task that has been created by a `task_create_*` function, e.g.,
[`task_create_explicit()`](https://mrc-ide.github.io/hipercow/reference/task_create_explicit.md),
[`task_create_expr()`](https://mrc-ide.github.io/hipercow/reference/task_create_expr.md).
Generally users should not run this function directly.

## Usage

``` r
task_eval(id, envir = .GlobalEnv, verbose = FALSE, root = NULL)
```

## Arguments

- id:

  The task identifier

- envir:

  An environment in which to evaluate the expression. For non-testing
  purposes, generally ignore this, the global environment will be likely
  the expected environment.

- verbose:

  Logical, indicating if we should print information about what we do as
  we do it.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

Logical indicating success (`TRUE`) or failure (`FALSE`)

## Examples

``` r
cleanup <- hipercow_example_helper(runner = FALSE)
#> ℹ This example uses a special helper
id <- task_create_expr(runif(1), driver = FALSE)
# Status is only 'created', not 'submitted', as we did not submit
# task.  This task can never run.
task_status(id)
#> [1] "created"

# Explicitly evaluate the task:
task_eval(id, verbose = TRUE)
#> 
#> ── hipercow 1.1.9 running at '/home/runner/work/_temp/hv-20261022-1a6a59a53695' 
#> ℹ library paths:
#>   • /home/runner/work/_temp/Library
#>   • /opt/R/4.6.1/lib/R/site-library
#>   • /opt/R/4.6.1/lib/R/library
#> ℹ id: c82faebfc8adbd1fb41c2cea271c00c0
#> ℹ starting at: 2026-10-22 12:05:59.0789
#> ℹ Task type: expression
#>   • Expression: runif(1)
#>   • Locals: (none)
#>   • Environment: default
#>     R_GC_MEM_GROW: 3
#> ───────────────────────────────────────────────────────────────── task logs ↓ ──
#> 
#> ───────────────────────────────────────────────────────────────── task logs ↑ ──
#> ✔ status: success
#> ℹ finishing at: 2026-10-22 12:05:59.0789 (elapsed: 0.07429 secs)
task_result(id)
#> [1] 0.2613714

cleanup()
#> ℹ Cleaning up example
```
