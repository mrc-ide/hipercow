# Validate a `hipercow_resources` list for a driver.

Query a driver to find information about the cluster, and then validate
a
[hipercow_resources](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
list against that driver to see if the resources requested could be
satisfied.

## Usage

``` r
hipercow_resources_validate(resources, driver = NULL, root = NULL)
```

## Arguments

- resources:

  A
  [hipercow_resources](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
  list returned by
  [hipercow_resources](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md),
  or `NULL`

- driver:

  The name of the driver to use, or you can leave blank if only one is
  configured (this will be typical).

- root:

  The hipercow root

## Value

TRUE if the resources are compatible with this driver.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
hipercow_resources_validate(hipercow_resources(cores = 1))
#> 
#> ── hipercow resource control (hipercow_resources) ──────────────────────────────
#> • cores: 1
#> • exclusive: FALSE
#> • queue: alltasks
#> Unset: 'max_runtime', 'hold_until', 'memory_per_node', 'memory_per_process',
#> 'requested_nodes', and 'priority'

# This example does not allow more than one core
tryCatch(
  hipercow_resources_validate(hipercow_resources(cores = 32)),
  error = identity)
#> <error/rlang_error>
#> Error in `validate_cluster_cores()`:
#> ! 32 is too many cores for this cluster.
#> ℹ The largest node has 4 cores.
#> ---
#> Backtrace:
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             ├─base::tryCatch(...)
#>  34.                             │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  35.                             │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  36.                             │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  37.                             └─hipercow::hipercow_resources_validate(hipercow_resources(cores = 32))
#>  38.                               └─hipercow:::resources_validate(resources, driver, root)
#>  39.                                 └─hipercow:::validate_cluster_cores(resources$cores, cluster_resources$max_cores)

cleanup()
#> ℹ Cleaning up example
```
