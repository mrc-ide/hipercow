# Load existing bundle

Load an existing saved bundle by name. This is intended for where you
have created a long-running bundle and since closed down your session.
See
[hipercow_bundle_list](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_list.md)
for finding names of bundles.

## Usage

``` r
hipercow_bundle_load(name, root = NULL)
```

## Arguments

- name:

  Name of the bundle to load

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A `hipercow_bundle` object

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:5))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'intercorporate_oriole' with 5 tasks
name <- bundle$name

# Delete the bundle object; the bundle exists still in hipercow's store.
rm(bundle)

# With the name we can load the bundle and fetch its status
bundle <- hipercow_bundle_load(name)
hipercow_bundle_status(bundle)
#> [1] "submitted" "submitted" "submitted" "submitted" "submitted"

# In fact, you can use just the name if you prefer:
hipercow_bundle_status(name)
#> [1] "submitted" "submitted" "submitted" "submitted" "submitted"

cleanup()
#> ℹ Cleaning up example
```
