# Create task bundle

Create a bundle of tasks. This is simply a collection of tasks that
relate together in some way, and we provide some helper functions for
working with them that save you writing lots of loops. Each bundle has a
name, which will be randomly generated if you don't provide one, and a
set of task ids.

## Usage

``` r
hipercow_bundle_create(
  ids,
  name = NULL,
  validate = TRUE,
  overwrite = TRUE,
  root = NULL
)
```

## Arguments

- ids:

  A character vector of task ids

- name:

  A string, the name for the bundle. If not given, then a random name is
  generated. Names can contain letters, numbers, underscores and
  hyphens, but cannot contain other special characters.

- validate:

  Logical, indicating if we should check that the task ids exist. We
  always check that the task ids are plausible.

- overwrite:

  Logical, indicating that we should overwrite any existing bundle with
  the same name.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A task bundle object

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

# Two task that were created separately:
id1 <- task_create_expr(sqrt(1))
#> ✔ Submitted task '527c4f3df1fdd719aa0b552e699ea470' using 'example'
id2 <- task_create_expr(sqrt(2))
#> ✔ Submitted task '46f60bd400cc4315865794797457551c' using 'example'

# Combine these tasks together in a bundle:
bundle <- hipercow_bundle_create(c(id1, id2))
#> ✔ Created bundle 'frosted_dungbeetle' with 2 tasks

# Now we can use bundle operations:
hipercow_bundle_status(bundle)
#> [1] "submitted" "submitted"
hipercow_bundle_wait(bundle)
#> [1] TRUE
hipercow_bundle_result(bundle)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1.414214
#> 

cleanup()
#> ℹ Cleaning up example
```
