# List existing bundles

List existing bundles

## Usage

``` r
hipercow_bundle_list(root = NULL)
```

## Arguments

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A [data.frame](https://rdrr.io/r/base/data.frame.html) with columns
`name` and `time`, ordered by time (most recent first)

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

# With no bundles present
hipercow_bundle_list()
#> [1] name time
#> <0 rows> (or 0-length row.names)

# With a bundle
bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'raging_llama' with 5 tasks
hipercow_bundle_list()
#>           name                time
#> 1 raging_llama 2025-11-26 12:53:47

cleanup()
#> ℹ Cleaning up example
```
