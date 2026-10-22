# Specify parallel use of cores

Set parallel options. Having requested more than one core using
[hipercow_resources](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md),
here hipercow can start up a local cluster on the node you are running
on, using either the `future` or `parallel` package.

## Usage

``` r
hipercow_parallel(
  method = NULL,
  cores_per_process = 1L,
  environment = NULL,
  use_rrq = FALSE
)
```

## Arguments

- method:

  The parallel method that hipercow will prepare. Three options are
  available: the `future` package, the `parallel` package, or `NULL`,
  the default, will do nothing. See the details for examples.

- cores_per_process:

  The number of cores allocated to each process when launching a local
  cluster using one of the parallel methods. By default, this will be
  `1`. See details.

- environment:

  The name of the environment to load into your parallel workers. The
  default is to use the environment that you submit your task with
  (which defaults to `default`), which means that each worker gets the
  same environment as your main process. This is often what you want,
  but can mean that you load too much into each worker and incur a speed
  or memory cost. In that case you may want to create a new environment
  ([hipercow_environment_create](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md))
  that contains fewer packages or sources fewer functions and specify
  that here. If you want to suppress loading any packages into the
  workers you can use the `empty` environment, which always exists.

- use_rrq:

  Logical, indicating if you intend to use `rrq`-based workers from your
  tasks, in which case we will set a default controller. Enabling this
  requires that you have configured a rrq controller via
  [`hipercow_rrq_controller()`](https://mrc-ide.github.io/hipercow/reference/hipercow_rrq_controller.md)
  before submitting the task (we check this before submission) and that
  you have submitted some workers via
  [`hipercow_rrq_workers_submit()`](https://mrc-ide.github.io/hipercow/reference/hipercow_rrq_workers_submit.md)
  (we don't check this because you will want them running at the time
  that your task starts, so you may want to launch them later depending
  on your workflow. We'll document this more in `vignete("rrq")`.

## Value

A list containing your parallel configuration.

## Details

Here, hipercow automatically does some setup work for the supported
methods, to initialise a local cluster of processes that can be used
with `future_map` or `clusterApply`, depending on your method.

By default, hipercow initialises a cluster with the same number of
processes as the number of cores you requested using
`hipercow_resources`. Each process here would be use a single core.

You can also call `hipercow_parallel` with `cores_per_process`, to make
hipercow launch as many processes as it can with each process having the
number of cores you request, with the total cores being at most what you
requested with `hipercow_resources`.

For example, you could request 32 cores with `hipercow_resources`, and
then call `hipercow_parallel` with `cores_per_process = 4`, and hipercow
will create a local cluster with 8 processes, each of which reporting
`4` cores if that process calls `hipercow_parallel_get_cores`.

If you did the same with `cores_per_process = 5`, hipercow would create
6 local processes, each reporting `5` cores, and two cores would be
effectively unallocated.

Here are some brief examples; see
[`vignette("parallel")`](https://mrc-ide.github.io/hipercow/articles/parallel.md)
for more details. In each example, we are looking up the process id (to
show that different processes are being launched), and asking each
process how many cores it should be using.

For using the `future` package:

    resources <- hipercow_resources(cores = 4)
    id <- task_create_expr(
      furrr::future_map(1:4,
        ~c(Sys.getpid(), hipercow_parallel_get_cores()),
      parallel = hipercow_parallel("future"),
      resources = resources)

where `furrr` must be provisioned using
[hipercow_provision](https://mrc-ide.github.io/hipercow/reference/hipercow_provision.md).
Here is an equivalent example with `parallel`:

    resources <- hipercow_resources(cores = 4)
    id <- task_create_expr(
      parallel::clusterApply(NULL, 1:4, function(x)
        c(Sys.getpid(), hipercow_parallel_get_cores()),
      parallel = hipercow_parallel("parallel"),
      resources = resources)
