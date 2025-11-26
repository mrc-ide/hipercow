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
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-186c27be9c' ──
#> ℹ library paths:
#>   • /home/runner/work/_temp/Library
#>   • /opt/R/4.5.2/lib/R/site-library
#>   • /opt/R/4.5.2/lib/R/library
#> ℹ id: 2ec2adf674a658691957c2207c03582e
#> ℹ starting at: 2025-11-26 12:56:25.080928
#> ℹ Task type: expression
#>   • Expression: runif(1)
#>   • Locals: (none)
#>   • Environment: default
#>     R_GC_MEM_GROW: 3
#> ───────────────────────────────────────────────────────────────── task logs ↓ ──
#> 
#> ───────────────────────────────────────────────────────────────── task logs ↑ ──
#> ✔ status: success
#> ℹ finishing at: 2025-11-26 12:56:25.080928 (elapsed: 0.07717 secs)
task_result(id)
#> [1] 0.8190824

cleanup()
#> ℹ Cleaning up example
```
