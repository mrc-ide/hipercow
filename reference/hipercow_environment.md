# Manage environments

Create, update, list, view and delete environments.

## Usage

``` r
hipercow_environment_create(
  name = "default",
  packages = NULL,
  sources = NULL,
  globals = NULL,
  overwrite = TRUE,
  check = TRUE,
  root = NULL
)

hipercow_environment_list(root = NULL)

hipercow_environment_delete(name = "default", root = NULL)

hipercow_environment_show(name = "default", root = NULL)

hipercow_environment_exists(name = "default", root = NULL)
```

## Arguments

- name:

  Name of the environment. The name `default` is special; this is the
  environment that will be used by default (hence the name!).
  Environment names can contain letters, numbers, hyphens and
  underscores.

- packages:

  Packages to be *attached* before starting a task. These will be loaded
  with [`library()`](https://rdrr.io/r/base/library.html), in the order
  provided, before the `sources` are sourced. If you need to attach a
  package *after* a script for some reason, just call `library` yourself
  within one of your `source` files.

- sources:

  Files to source before starting a task. These will be sourced into the
  global (or execution) environment of the task. The paths must be
  relative to the hipercow root, not the working directory. Files are
  sourced in order.

- globals:

  Names of global objects that we can assume exist within this
  environment. This might include function definitions or large data
  objects. The special value `TRUE` triggers automatic detection of
  objects within your environment (this takes a few seconds and requires
  that the environment is constructable on your local machine too, so is
  not currently enabled by default).

- overwrite:

  On environment creation, replace an environment with the same name.

- check:

  Logical, indicating if we should check the source files for issues.
  Pass `FALSE` here if you need to bypass these checks but beware the
  consequences that may await you.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

Nothing, all are called for their side effects.

## See also

A longer introduction in
[`vignette("environments")`](https://mrc-ide.github.io/hipercow/articles/environments.md)

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

# Suppose you have a file with some functions you want to use in
# your task:
writeLines("simulation <- function(n) cumsum(rnorm(n))", "myfuns.R")

# Update the default environment to include these functions (or in
# this example, just this one function)
hipercow_environment_create(sources = "myfuns.R")
#> ✔ Created environment 'default'

# You can now use this function in your tasks:
id <- task_create_expr(simulation(5))
#> ✔ Submitted task 'f8d82118e40f509542ee0a8d20f79f66' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> [1] -0.8096977 -1.5656146 -1.9508663 -1.2660940  1.1156957

cleanup()
#> ℹ Cleaning up example
```
