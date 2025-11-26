# Fetch bundle results

Fetch all bundle results

## Usage

``` r
hipercow_bundle_result(bundle, follow = TRUE, root = NULL)
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

An unnamed list, with each element being the result for each a task in
the bundle, in the same order.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'semimonarchical_horseshoecrab' with 5 tasks
hipercow_bundle_wait(bundle)
#> [1] TRUE
hipercow_bundle_result(bundle)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1.414214
#> 
#> [[3]]
#> [1] 1.732051
#> 
#> [[4]]
#> [1] 2
#> 
#> [[5]]
#> [1] 2.236068
#> 

cleanup()
#> ℹ Cleaning up example
```
