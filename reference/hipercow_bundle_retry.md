# Retry task bundle

Retry tasks in a bundle. This has slightly different semantics to
[`task_retry()`](https://mrc-ide.github.io/hipercow/reference/task_retry.md),
which errors if a retry is not possible. Here, we anticipate that much
of the time you will be interested in retrying some fraction of your
bundle and so don't need to wait until all tasks have finished in order
to retry failed tasks.

## Usage

``` r
hipercow_bundle_retry(bundle, if_status_in = NULL, driver = NULL, root = NULL)
```

## Arguments

- bundle:

  Either a `hipercow_bundle` object, or the name of a bundle.

- if_status_in:

  Optionally, a character vector of task statuses for which we should
  retry tasks. For example, pass
  `if_status_in = c("cancelled", "failure")` to retry cancelled and
  failed tasks. Can only be terminal statuses (`cancelled`, `failure`,
  `success`).

- driver:

  Name of the driver to use to submit the task. The default (`NULL`)
  depends on your configured drivers; if you have no drivers configured
  no submission happens (or indeed is possible). If you have exactly one
  driver configured we'll submit your task with it. If you have more
  than one driver configured, then we will error, though in future
  versions we may fall back on a default driver if you have one
  configured. If you pass `FALSE` here, submission is prevented even if
  you have no driver configured.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

Invisibly, a logical vector, indicating which of the tasks within the
bundle were retried. This means that it's not immediately obvious how
you can get the new id back from the tasks, but typically that is
unimportant, as all bundle functions follow retries by default.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
bundle <- task_create_bulk_expr(rnorm(1, x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'parochialist_argentinehornedfrog' with 5 tasks
hipercow_bundle_wait(bundle)
#> [1] TRUE

retried <- hipercow_bundle_retry(bundle)
#> ℹ Retrying 5 / 5 tasks
#> ✔ Submitted 5 tasks using 'example'
retried
#> [1] TRUE TRUE TRUE TRUE TRUE
hipercow_bundle_wait(bundle)
#> [1] TRUE
hipercow_bundle_result(bundle, follow = FALSE)
#> [[1]]
#> [1] 1.557145
#> 
#> [[2]]
#> [1] 0.3147931
#> 
#> [[3]]
#> [1] 4.52348
#> 
#> [[4]]
#> [1] 3.820966
#> 
#> [[5]]
#> [1] 6.226995
#> 
hipercow_bundle_result(bundle, follow = TRUE)
#> [[1]]
#> [1] 1.20548
#> 
#> [[2]]
#> [1] 1.559676
#> 
#> [[3]]
#> [1] 1.520325
#> 
#> [[4]]
#> [1] 1.906779
#> 
#> [[5]]
#> [1] 4.526516
#> 

cleanup()
#> ℹ Cleaning up example
```
