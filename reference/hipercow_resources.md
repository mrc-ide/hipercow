# Hipercow Resources

Specify what resources a task requires to run. This creates a validated
list of resources that can be passed in as the `resources` argument to
[task_create_expr](https://mrc-ide.github.io/hipercow/reference/task_create_expr.md)
or other task creation functions.

## Usage

``` r
hipercow_resources(
  cores = 1L,
  exclusive = FALSE,
  max_runtime = NULL,
  hold_until = NULL,
  memory_per_node = NULL,
  memory_per_process = NULL,
  requested_nodes = NULL,
  priority = NULL,
  queue = NULL
)
```

## Arguments

- cores:

  The number of cores your task requires. This is 1 by default. Setting
  to `Inf` will request any single node, however many cores that node
  has.

- exclusive:

  Set this to `TRUE` to ensure no other tasks will be concurrently run
  on the node while it runs your task. This is done implicitly if
  `cores` is `Inf`. This might be useful for a single core task that
  uses a very large amount of memory, or for multiple tasks that for
  some reason cannot co-exist on the same node.

- max_runtime:

  Set this to specify a time limit for running your job. Acceptable
  formats are either an integer number of minutes, or strings specifying
  any combination of hours (h), days (d) and minutes (m). Example valid
  values: `60`, `"1h30m`", `"5h"`, or `"40d"`.

- hold_until:

  Specify your task should wait in the queue until a certain time, or
  for a certain period. For the former, this can be a
  [POSIXt](https://rdrr.io/r/base/DateTimeClasses.html) (i.e., a date
  and time in the future), a [Date](https://rdrr.io/r/base/Dates.html)
  (midnight on a day in the future), the special strings "tonight"
  (7pm), "midnight", or "weekend" (midnight Saturday morning). To delay
  for a period, you can specify an integer number of minutes, or strings
  specifying any combination of hours (h), days (d) and minutes (m).
  Example valid values: `60`, `"1h30m`", `"5h"`, or `"3d"`.

- memory_per_node:

  Specify your task can only run on a node with at least the specified
  memory. This is an integer assumed to be gigabytes, or a string in
  gigabytes or terabytes written as `"64G"` or `"1T"` for example.

- memory_per_process:

  If you can provide an estimate of how much RAM your task requires,
  then the cluster can ensure the total memory required by running
  multiple tasks on a node does not exceed how much memory the node has.
  Specify this as an integer number of gigabytes, or characters such as
  `"10G"`

- requested_nodes:

  If you have been in touch with us or DIDE IT, and you need to run your
  task on a selection of named compute nodes, then specify this here as
  a vector of strings for the node names.

- priority:

  If the tasks you are launching are low priority, you can allow other
  queuing tasks to jump over them, by setting the priority to to `low`;
  otherwise, the default is `normal`. These are the only acceptable
  values.

- queue:

  Specify a particular queue to submit your tasks to. This is in
  development as we decide over time what queues we best need for DIDE's
  common workflows. See the Details for more information, and the queues
  available on each cluster.

## Value

If the function succeeds, it returns a `hipercow_resources` list of
parameters which is syntactically valid, although not yet validated
against a particular driver to see if the resources can be satisfied. If
the function fails, it will return information about why the arguments
could not be validated. Do not modify the return value.

## Windows cluster (`wpia-hn`)

- Cores at present must be between 1 and 32

- Memory per node (or per task) can be 512Gb at most.

- The available queues are `AllNodes` and `Testing` - the latter has a
  maximum runtime of 30 minutes; jobs will be aborted if they exceed
  this.

- The node names are between `wpia-001` and `wpia-089`, excluding 41,
  42, 49 and 50.

## Linux cluster (hermod)

Coming Soon.

## Examples

``` r
# The default set of resources
hipercow_resources()
#> 
#> ── hipercow resource control (hipercow_resources) ──────────────────────────────
#> • cores: 1
#> • exclusive: FALSE
#> Unset: 'max_runtime', 'hold_until', 'memory_per_node', 'memory_per_process',
#> 'requested_nodes', 'priority', and 'queue'

# A more complex case:
hipercow_resources(
  cores = 32,
  exclusive = TRUE,
  priority = "low")
#> 
#> ── hipercow resource control (hipercow_resources) ──────────────────────────────
#> • cores: 32
#> • exclusive: TRUE
#> • priority: low
#> Unset: 'max_runtime', 'hold_until', 'memory_per_node', 'memory_per_process',
#> 'requested_nodes', and 'queue'

# (remember that in order to change resources you would pass the
# return value here into the "resources" argument of
# task_create_expr() or similar)
```
