# Create an rrq controller

Create an rrq controller for your queue, and set it as the default
controller. Use this to interact with workers created with
[`hipercow_rrq_workers_submit()`](https://mrc-ide.github.io/hipercow/reference/hipercow_rrq_workers_submit.md).
Proper docs forthcoming, all interfaces are subject to some change.

## Usage

``` r
hipercow_rrq_controller(
  ...,
  set_as_default = TRUE,
  driver = NULL,
  queue_id = NULL,
  root = NULL
)
```

## Arguments

- ...:

  Additional arguments passed through to
  [`rrq::rrq_controller()`](https://mrc-ide.github.io/rrq/reference/rrq_controller.html);
  currently this is `follow` and `timeout_task_wait`.

- set_as_default:

  Set the rrq controller to be the default; this is usually what you
  want.

- driver:

  Name of the driver to use. The default (`NULL`) depends on your
  configured drivers; if you have no drivers configured we will error as
  we lack information required to proceed. If you have exactly one
  driver configured we'll submit your task with it. If you have more
  than one driver configured, then we will error, though in future
  versions we may fall back on a default driver if you have one
  configured.

- queue_id:

  The rrq queue id to use. This parameter is used internally by
  hipercow. You shouldn't ever need to pass a value for this.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

An
[rrq::rrq_controller](https://mrc-ide.github.io/rrq/reference/rrq_controller.html)
object.
