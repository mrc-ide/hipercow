# Delete task bundles

Delete one or more hipercow task bundles. Note that this does not delete
the underlying tasks, which is not yet supported.

## Usage

``` r
hipercow_bundle_delete(name, root = NULL)
```

## Arguments

- name:

  Character vectors of names to delete

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

Nothing, called for its side effect

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'frantic_grub' with 5 tasks
hipercow_bundle_list()
#>           name                time
#> 1 frantic_grub 2025-11-26 12:53:46

# Retaining the ids, delete bundle
ids <- bundle$ids
hipercow_bundle_delete(bundle$name)
hipercow_bundle_list()
#> [1] name time
#> <0 rows> (or 0-length row.names)

# The tasks still exist:
task_status(ids)
#> [1] "submitted" "submitted" "submitted" "submitted" "submitted"

cleanup()
#> ℹ Cleaning up example
```
