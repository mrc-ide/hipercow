# Parallel Tasks

## Task-level parallelism

Never underestimate the performance gain you might get by simple running
many tasks at the same time. If your code is written in a way that makes
it easy to run many instances of it, with different parameters for
example, then consider using
[`task_create_bulk_expr()`](https://mrc-ide.github.io/hipercow/reference/task_create_bulk_expr.md)
to simply run those tasks, without making any coding changes.

If however, you want to use multiple cores at the same time within a
task, or if your task has special requirements regarding the compute
nodes it can run on, then read this vignette.

As we go, we’ll be using an example cluster; the results that you’ll get
back from a real cluster will differ, but the principles should be the
same.

``` r
hipercow_init(".", driver = "example")
#> ✔ Initialised hipercow at '.' (/home/runner/work/_temp/hv-20251126-2d473b403cb6)
#> ✔ Configured hipercow to use 'example'
```

## What resources does the cluster have?

At present, we have one Windows-based cluster, but in the future we plan
for others. Our aim is that use of
[`hipercow_resources()`](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
and
[`hipercow_parallel()`](https://mrc-ide.github.io/hipercow/reference/hipercow_parallel.md)
will be the same across the clusters we will support - yet the clusters
are likely to have different resources and queues.

To look up information about the cluster you are currently configured to
use, call
[`hipercow_cluster_info()`](https://mrc-ide.github.io/hipercow/reference/hipercow_cluster_info.md) -
a real example of this is in `vignette("windows")`.

For the purposes of this vignette, we will be using a virtual cluster
called `example` that has a single 4-core node, and can run the simple
examples below.

To create a task that uses more than one core, we need to request the
resources using
[`hipercow_resources()`](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md),
and then specify how we want the cores to be used, using
[`hipercow_parallel()`](https://mrc-ide.github.io/hipercow/reference/hipercow_parallel.md).

## Specifying multi-core resources

The `cores` and `exclusive` arguments to
[`hipercow_resources()`](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
are the important ones here.

- If `cores` is an integer, then as soon as a node has sufficient cores
  free, your task will launch on that node. Task submission will fail if
  no node has that many cores. This is the most common way people
  increase the resources allocated to their tasks in practice.

- If `cores` is `Inf`, then your task will run on the first node that
  becomes completely free; this node could have any number of cores. At
  present our nodes all have the same number of cores. When that
  changes, then this will be useful for throughput if you have a bulk
  number of tasks that benefit from parallel execution, and you don’t
  particularly mind whether some run on 20-core machines, and others on
  32-core machines.

- Setting `exclusive` to `TRUE`, is similar to setting `cores` as `Inf`,
  in that your task will get a whole node to itself; the difference is
  that the number of cores reported to `hipercow` will be the number of
  cores you request, which may be less than the number of cores the node
  has. This is useful if your task cannot co-exist on the same node with
  another of your tasks, or perhaps anyone else’s tasks; for example, a
  single-core node that uses all the memory a node has, or a task that
  does some network API access that would fail if multiple requests came
  from the same IP address.

## Running parallel tasks

Requesting a number of cores through `hipercow_resoures` will cause a
number of environment variables to be set when the task starts running.
These are `MC_CORES`, `OMP_NUM_THREADS`, `OMP_THREAD_LIMIT` and
`R_DATATABLE_NUM_THREADS`, along with `HIPERCOW_CORES` which we use
internally, and their value is the number of cores you requested.

Some packages (such as [`dust`](https://mrc-ide.github.io/dust/) or
[Stan](https://mc-stan.org/)) can use these environment variables and
run in parallel without you having to do anything further. There are
also ways you can explicitly say how many threads you would like to
use - see below.

However, if you’re not using any packages that look up these environment
variables, then requesting the resources with
[`hipercow_resources()`](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
alone will not change the behaviour or performance of your code; it will
only affect the resources the cluster reserves and allocates to your
task, as it decides what tasks to run on which nodes.

Hipercow provides more ways of making use of the cores we reserved. The
[`hipercow_parallel()`](https://mrc-ide.github.io/hipercow/reference/hipercow_parallel.md)
function at present supports two methods for running different code on
the different cores you have reserved. One is the `parallel` package,
and the other is the `future` package. In each case, `hipercow` handles
the setup of the parallel cluster for us, as we’ll describe next.

### Using the Parallel package

In this example, we reserve two cores on the cluster, and then call
`hipercow_parallel("parallel")` which sets up a team of workers (two in
this case), each of which use one of the allocated cores.

The `parallel` package is built into R and provides a simple, if
somewhat eccentric, approach to multi-process parallelism. There is an
introductory vignette in
[`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf).
The general strategy when using `parallel` is to write code that you
could run with [`lapply()`](https://rdrr.io/r/base/lapply.html), then
use
[`parallel::clusterApply()`](https://rdrr.io/r/parallel/clusterApply.html)
to run it in parallel instead, with no other changes needed.

``` r
resources <- hipercow_resources(cores = 2)
id <- task_create_expr(
  parallel::clusterApply(NULL, 1:2, function(x) Sys.sleep(5)),
  parallel = hipercow_parallel("parallel"),
  resources = resources)
#> ✔ Submitted task 'ba2cf05e23552b8796db473967293a21' using 'example'
task_wait(id)
#> [1] TRUE
task_info(id)
#> 
#> ── task ba2cf05e23552b8796db473967293a21 (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#> • Expression: parallel::clusterApply(NULL, 1:2, function(x) Sys.sleep(5))
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-2d473b403cb6/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-2d473b403cb6/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:20.697884 (moments ago)
#> ℹ Started at 2025-11-26 12:57:20.99219 (moments ago; waited 295ms)
#> ℹ Finished at 2025-11-26 12:57:26.574395 (moments ago; ran for 5.6s)
```

By specifying the `parallel` argument here, `hipercow` will start up a
“cluster” within your job for you, so that the
[`parallel::clusterApply`](https://rdrr.io/r/parallel/clusterApply.html)
command runs across two processes.

### Using the `future` package

The [`future`](https://future.futureverse.org/) package, is similar in
use to `parallel`, and some prefer the way of using it such as with the
[`furrr`](https://furrr.futureverse.org/) package, as it offers very
high-level interfaces that match closely those in the
[`purrr`](https://purrr.tidyverse.org/) package.

``` r
resources <- hipercow_resources(cores = 2)
id <- task_create_expr(
  furrr::future_map(1:2, ~Sys.sleep(5)),
  parallel = hipercow_parallel("future"),
  resources = resources)
#> ✔ Submitted task '5491ed4c6d2e8edb91df21d78daf96e4' using 'example'
task_wait(id)
#> [1] TRUE
task_info(id)
#> 
#> ── task 5491ed4c6d2e8edb91df21d78daf96e4 (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#> • Expression: furrr::future_map(1:2, ~ Sys.sleep(5))
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-2d473b403cb6/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-2d473b403cb6/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:26.894147 (moments ago)
#> ℹ Started at 2025-11-26 12:57:27.11503 (moments ago; waited 221ms)
#> ℹ Finished at 2025-11-26 12:57:33.303163 (moments ago; ran for 6.2s)
```

In our testing though, `furrr` has much higher overheads than than
`parallel`. In the test above, `future` usually takes close to 8
seconds, whereas `parallel` above takes just over 5. So perhaps test
your code at an early stage to see whether the difference matters to
you, compared to which package you prefer writing code with. We expect
this overhead will reduce in impact as the amount of work you do in each
parallel task increases (if the overhead is 3s but your parallelised
task takes 10 minutes, this overhead is negligible, especially if you
find it easier to use).

In this example, we would also need the `furrr` package to be
provisioned using
[`hipercow_provision()`](https://mrc-ide.github.io/hipercow/reference/hipercow_provision.md).

### Specifying more work than there are cores

With the `future_map` and `clusterApply` examples above, we provided a
vector of work to do - in this case simply `1:2`. In these examples,
this exactly matches the number of cores we requested using
`hipercow_resources`. The amount of work could be larger, for example
`1:4`, but in both methods, only 2 processes will be run concurrently,
because this is what we requested, and is how `hipercow_parallel`
initialised the cluster. The extra processes have to queue until an
allocated core is free. For <example:->

``` r
resources <- hipercow_resources(cores = 2)
id <- task_create_expr(
  parallel::clusterApply(NULL, 1:3, function(x) Sys.sleep(2)),
  parallel = hipercow_parallel("parallel"),
  resources = resources)
#> ✔ Submitted task '20ae77a555fdacdc8bdaa6015881bf9c' using 'example'
task_wait(id)
#> [1] TRUE
task_info(id)
#> 
#> ── task 20ae77a555fdacdc8bdaa6015881bf9c (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#> • Expression: parallel::clusterApply(NULL, 1:3, function(x) Sys.sleep(2))
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-2d473b403cb6/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-2d473b403cb6/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:34.063341 (moments ago)
#> ℹ Started at 2025-11-26 12:57:34.301356 (moments ago; waited 239ms)
#> ℹ Finished at 2025-11-26 12:57:38.873963 (moments ago; ran for 4.6s)
```

Here we reserve 2 cores, and then map 3 processes onto the cluster, each
of which will take 2 seconds. It takes more than 4 seconds in all,
because we can’t run the 3 processes at the same time; one of them has
to wait for a free core.

### How many cores should each process use?

The number of cores available to a process can be looked up with
`hipercow_parallel_get_cores`. For the main process, this will be the
same as the number of cores requested using `hipercow_resources`, but
for the workers created by the `future` or `parallel` clusters, the
result will be 1, as we initialise a separate process per core.

``` r
resources <- hipercow_resources(cores = 4)
id <- task_create_expr({
    unlist(c(hipercow::hipercow_parallel_get_cores(),
      parallel::clusterApply(
        NULL,
        1:4,
        function(x) hipercow::hipercow_parallel_get_cores())))
  },
  parallel = hipercow_parallel("parallel"),
  resources = resources)
#> ✔ Submitted task '8cf3d44a50b0253b19b878b2d40c47df' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> [1] 4 1 1 1 1
```

### Multiple cores per process

In the previous example, we created a cluster that could run 4 processes
at the same time, but each of those 4 processes was a single-core task.
We could not do any nested parallelism within those 4 processes. If we
want to do that - to have nested parallelism - we can use the
`cores_per_process` argument to `hipercow_parallel`, and create a number
of processes as before, but each of which might have a number of cores
allocated to it.

This would be useful if, for example, we requested 32 cores, and we
wanted to run 4 concurrent tasks using `future_map` or `clusterApply`,
each of which would have 8 cores to do something parallel with, perhaps
using `Stan`, or `dust`. Our example cluster is rather smaller, but here
we create a pair of 2-core process using `parallel`.

``` r
resources <- hipercow_resources(cores = 4)
id <- task_create_expr({
    unlist(c(hipercow::hipercow_parallel_get_cores(),
      parallel::clusterApply(
        NULL,
        1:2,
        function(x) hipercow::hipercow_parallel_get_cores())))
  },
  parallel = hipercow_parallel("parallel", cores_per_process = 2),
  resources = resources)
#> ✔ Submitted task 'd41d06abda048f01039d41696d2cfab9' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> [1] 4 2 2
```

Now each process knows it has 2 cores allocated, so we could update the
function `clusterApply` is calling, to pass the result of
`hipercow_parallel_get_cores` into some other function that supports
parallel processing. We could also use `x`, which here will be `1` or
`2` on the pair of processes, to cause different behaviour on each
process.

### Other ways of using cores

There are other packages that can use multiple cores, and often the
number of cores they use can be set with environment variables. Hipercow
automatically sets some useful variables to indicate how many cores the
cluster has allocated to your task - even if you don’t call
`hipercow_parallel`.

These will be visible to all packages that use them. For example, the
`parallel` package uses `MC_CORES`, C++ code using OpenMP will look up
`OMP_NUM_THREADS` when the function `omp_get_max_threads()` is called.
Here are a couple of examples using the `dust` package (which again
would need to be provisioned if you run this on a real cluster).

``` r
resources <- hipercow_resources(cores = 2)
id <- task_create_expr({
  res <- dust::dust_openmp_support()
  c(res[["num_procs"]], res[["OMP_NUM_THREADS"]], res[["MC_CORES"]],
    dust::dust_openmp_threads())
},
resources = resources)
#> ✔ Submitted task '44f16ddc11ab54397bba332093783b6d' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> [1] 4 2 2 2
```

Here, the `num_procs` value dust gives us, is the number of cores the
machine has, not all of which may have been allocated to our job. In
this case, only two cores are for us to use, which is what the other
environment variables report, and what `dust` is going to use. Below,
we’ll see dust generating random numbers for us with different numbers
of threads. Note the final column `total_time` decreases as we do the
same amount of work with more threads.

``` r
resources <- hipercow_resources(cores = 4)
id <- task_create_expr({
  rng <- dust::dust_rng$new(seed = 1, n_streams = 32)
  bench::mark(
    one = rng$random_normal(1000000, n_threads = 1),
    two = rng$random_normal(1000000, n_threads = 2),
    four = rng$random_normal(1000000, n_threads = 4),
    check = FALSE,
    time_unit = "s")
  }, resources = resources)
#> ✔ Submitted task 'b337b196fb9b140158b4c54342211a55' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> # A tibble: 3 × 13
#>   expression   min median `itr/sec` mem_alloc  `gc/sec` n_itr  n_gc total_time
#>   <bnch_xpr> <dbl>  <dbl>     <dbl> <bnch_byt>    <dbl> <int> <dbl>      <dbl>
#> 1 <language> 1.01   1.01      0.986 256003144     0.986     1     1      1.01 
#> 2 <language> 0.490  0.491     2.04  256000048     2.04      2     2      0.981
#> 3 <language> 0.370  0.370     2.70  256000048     2.70      2     2      0.740
#> # ℹ 4 more variables: result <list>, memory <list>, time <list>, gc <list>
```

You may want to set the environment variables so that dust and other
packages use a different number of cores. For example, perhaps you have
acquired a whole 32 core node because of memory reasons, but your
parallel algorithms are not able to use that many cores optimally, and a
smaller number is better. (There are examples where this is the case).
Here, you could call `hipercow_parallel_set_cores` with the number of
cores you want, and all the environment variables will take that value.

A better way of solving that problem though, is to specify a memory
requirement:

``` r
resources <- hipercow_resources(memory_per_node = "256G",
                                exclusive = TRUE,
                                cores = 8)
```

(and passing this in as the `resources` argument to a task creation
function). See below for more details.

## Specifying which nodes should run your tasks

We’ve already set the number of cores your task needs, so that is one
way that might limit the nodes capable of running your task.
Additionally, if you have specific memory requirements for your tasks, a
specific queue to run your tasks on, or even specific nodes they should
be run on, these can be specified with the arguments to
`hipercow_resources` in several different ways, which we outline below.

### Memory requests

Two methods are currently provided for specifying memory usage. These
can be specified as an integer number of gigabytes, or alternative as
strings such as `"64G`” or `"1T"` to represent 64Gb, or 1Tb
respectively.

- The `memory_per_node` specifies very simply that your task should only
  be run on a node that has at least that much memory. Remember that the
  node’s memory will be shared between all the tasks running on that
  node, so you could also consider specifying `cores = Inf`, or
  `exclusive = TRUE` if you think you’ll need the whole node’s memory to
  yourself.

- If you are launching many tasks, and know the maximum memory your task
  needs, then you can specify `memory_per_process` to tell the cluster
  about that. The cluster will then avoid allocating too many of your
  tasks to the same node, if the combined memory needed by those tasks
  will exceed what the node has. This can’t really be guaranteed, unless
  everyone agrees to set `memory_per_process`, but it should help in the
  common case where your own tasks might be stacked up on a node
  together.

### Running on specific nodes

At present, the nodes on the new cluster are all very similar to each
other, and there is no partitioning of nodes between users or groups. It
is a free-for-all, with little variation in specification for the nodes.
This may change over time, as the cluster grows, and as the user base
grows.

In the future, there may be some nodes, or queues of nodes, that are
more appropriate for your tasks than others, either because of their
specification, or because of some groups having priority access to nodes
they may have purchased, for example.

We’ve already noted that specifying cores or memory requirements will
cause your tasks to run on a node meeting those requirements.
Additionally, we can explicitly say that tasks should be submitted to a
particular *queue*, or that tasks should be run only on particular
*named nodes*. See below.

#### Selecting by queue

At present, the new cluster only has one queue for general use, called
`AllNodes` containing, as it says, all the available compute nodes. At
other times though, during workshops for example we have run a
`Training` queue, with strict limits, to ensure we’ve had capacity to
demonstrate cluster use in a live setting.

It also may be necessary in the future to partition the set of nodes,
either by their capabilities if that becomes significant to some users,
or by which research group might have purchased them, or to allow a
particular group more protected access for a period.

Here’s how to see the queues, and choose one, using the `example`
cluster.

``` r
hipercow_cluster_info()$resources$queues
#> [1] "alltasks" "bigmem"   "fast"
resources <- hipercow_resources(queue = "bigmem")
```

then as before, we pass `resources` to any of the `task_create_`
functions.

#### Selecting by node names

Even more rarely, you may have a particular named node you want to run
on. In the past, for instance, we have had specific nodes with unique
hardware (such as very large RAM or large disks). Or occasionally, we
may want to try and replicate a failure by rerunning a task using the
same node on which the failure occurred.

Again, using the `example` cluster, we can set the `requested_nodes`
argument, :-

``` r
hipercow_cluster_info()$resources$nodes
#> [1] "node-1" "node-2" "gpu-3"  "gpu-4"
resources <- hipercow_resources(
  requested_nodes = c("gpu-3", "gpu-4"))
```

and again, `resources` gets passed to one of the `task_create_`
functions.

## Task time limits and scheduling

Our clusters over the years have essentially be run on the basis of good
will, rather than having too many limits over how long tasks can run
for, or how much resource they can use. For our fairly small department,
this is a nice way to work, meaning you can usually get the resources
you need, even if your needs are quite demanding for a period of time.
Usage fluctuates depending on deadlines and the development cycle of
projects. It is relatively rare that many people or projects have
demanding needs at the same time, such that capacity of the cluster
becomes problematic. But if needs do coincide, we resolve them mainly by
communication, rather than cluster rules.

That said, `hipercow` offers a few options for limiting how long your
tasks can run for, specifying when your tasks can run on the cluster,
and also allows you to politely allow other tasks to take priority over
yours. If we know how long tasks will take, then we have the potential
in the future to priorities smaller faster tasks ahead of larger slower
ones.

### The maximum runtime

If you know how long your task should take, and you’d like to abort if
it takes longer, then use the `max_runtime` argument when requesting
your resources. You can specify an integer number of minutes, or strings
involving the letters `d`, `h` and `m`, for days, hours and minutes,
such as `"40d`” or `"1h30"`.

This might be useful if you have stochastic fitting tasks that might not
be converging, and you’d like a time limit after which the tasks are
aborted. Or perhaps you have a task that you’d only like to run for a
while to check that the early stages look good.

### Delaying tasks starting

If you are about to launch a large number of very time consuming tasks,
that are not crucially urgent, it may be helpful to others if they could
start running on the cluster outside of working hours. You can do this
by setting the `hold_until` argument for `hipercow_resources`. A number
of formats are allowed:-

- An integer represents a number of minutes.
- Strings in the form `"5h"` or `"1h30"` or `"2d"` can delay for hours,
  minutes or days.
- R’s `Date` type can be used to indicate midnight on the given date.
- R’s `POSIXt` type a date and time to be represented.
- `"tonight"` makes a task wait until after 7pm this evening before
  starting.
- `"midnight"` delays a task until tomorrow begins.
- `"weekend"` delays a task until midnight on Saturday.

### Lowering your priority

If you are about to launch a large number of tasks, another way of being
polite to your colleagues is to set `priority = "low"` in
`hipercow_resources`. This allows tasks lower down the queue with
`normal` priority to overtake your `low` priority tasks and run on
available resources first. Effectively, it means you can launch large
volumes of tasks without annoying people, getting all the available
resources, but without holding others up very much if they also need
something to run.

This never causes your running tasks to get cancelled; it is only
relevant when there are available resources on a node for the scheduler
to consider which tasks to allocate those resources too. Therefore, low
priority works best if additionally your tasks don’t take too long to
run, so there are reasonably frequent opportunities for the scheduler to
decide what to do.
