# hipercow

Parallel computing on a cluster can be more challenging than running
things locally because it’s often the first time that you need to
package up code to run elsewhere, and when things go wrong it’s more
difficult to get information on why things failed.

Much of the difficulty of getting things running involves working out
what your code depends on, and getting that installed in the right place
on a computer that you can’t physically poke at. The next set of
problems is dealing with the ballooning set of files that end up being
created - templates, scripts, output files, etc.

The `hipercow` package aims to remove some of this pain, with the aim
that running a task on the cluster should be (almost) as straightforward
as running things locally, at least once some basic setup is done.

This manual is structured in escalating complexity, following the chain
of things that a hypothetical user might encounter as they move from
their first steps on the cluster through to running enormous batches of
tasks.

## Clusters and Platforms

In this documentation, by *cluster*, we mean really a head-node. We
support one of these at the moment: the DIDE cluster. In the future, we
might also support the PBS cluster that ICT run, or we might create a
pure Linux cluster with a SLURM headnode for instance. But not yet.

By *platform*, we mean operating system. You can use `hipercow` on your
own computer, whether it runs Windows, Linux or MacOS. There are
differences between these operating systems, mostly about how to refer
to your DIDE network paths as they appear on your computer (ie, drive
letters or mountpoints), and `hipercow` tries to handle those for you
without any fuss.

Until recently, your jobs would then be run on Windows cluster nodes.
However, unusually (and perhaps interestingly), our DIDE cluster runs
Microsoft HPC, which can have both Linux-based and Windows-based nodes
attached to it, using the same basic launch tools for either. This is
interesting, since it can allow Linux jobs to be run, but with a launch
mechanism extremely similar to what you might already be familiar with
from `hipercow`.

So throughout this documentation, we will refer to one *cluster*, (the
DIDE cluster), supporting two *target platforms* - compute nodes running
Windows or Linux. Please do feedback your experiences especially on the
Linux nodes, as this is new and we’d like to know what workflows work
well for you.

## Installing prerequisites

Install the required packages from our “r-universe”. Be sure to run this
in a fresh session.

``` r
install.packages(
  "hipercow",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

Once installed you can load the package with

``` r
library(hipercow)
```

or use the package by prefixing the calls below with `hipercow::`, as
you prefer.

Follow any cluster-specific instructions in `vignettes("<cluster>")`;
this will depend on the cluster you intend to use - at present just our
DIDE cluster.

- The DIDE Cluster:
  [`vignette("dide-cluster")`](https://mrc-ide.github.io/hipercow/articles/dide-cluster.md)

## Filesystems and paths

We need a concept of a “root”; the point in the filesystem we can think
of everything relative to. This will feel familiar to you if you have
used git or orderly, as these all have a root (and this root will be a
fine place to put your cluster work). Typically all paths will be
*within* this root directory, and paths above it, or absolute paths in
general, effectively cease to exist. If your project works this way then
it’s easy to move around, which is exactly what we need to do in order
to run it on the cluster.

If you are using RStudio, then we strongly recommend using an [RStudio
project](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects).

## Initialising

Run

``` r
hipercow_init()
#> ✔ Initialised hipercow at '.' (/home/runner/work/_temp/hv-20251126-2c2e7897ab86)
#> ℹ Next, call 'hipercow_configure()'
```

which will write things to a new path `hipercow/` within your working
directory.

After initialisation you will typically want to configure a “driver”,
which controls how tasks are sent to clusters. There are two options for
this: `dide-windows` and `dide-linux` which will target nodes running
either operating system on our DIDE cluster. For example:

``` r
hipercow_configure(driver = "dide-windows")
```

however, for this vignette we will use a special “example” driver which
simulates what the cluster will do (don’t use this for anything
yourself, it really won’t help):

``` r
hipercow_configure(driver = "example")
#> ✔ Configured hipercow to use 'example'
```

You can run initialisation and configuration in one step by running, for
example:

``` r
hipercow_init(driver = "dide-linux")
```

Whether you want to target Windows or Linux, by setting the driver to
`dide-windows` or `dide-linux` respectively, all the `hipercow` commands
that follow will be the same.

After initialisation and configuration you can see the computed
configuration by running
[`hipercow_configuration()`](https://mrc-ide.github.io/hipercow/reference/hipercow_configuration.md):

``` r
hipercow_configuration()
#> 
#> ── hipercow root at /home/runner/work/_temp/hv-20251126-2c2e7897ab86 ───────────
#> ✔ Working directory '.' within root
#> ℹ R version 4.5.2 on Linux (runner@runnervmg1sw1)
#> 
#> ── Packages ──
#> 
#> ℹ This is hipercow 1.1.8
#> ℹ Installed: conan2 (1.9.104), logwatch (0.1.1), rrq (0.7.23)
#> ✖ hipercow.dide is not installed
#> 
#> ── Environments ──
#> 
#> ── default
#> • packages: (none)
#> • sources: (none)
#> • globals: (none)
#> 
#> ── empty
#> • packages: (none)
#> • sources: (none)
#> • globals: (none)
#> 
#> ── Drivers ──
#> 
#> ✔ 1 driver configured ('example')
#> 
#> ── example
#> (unconfigurable)
```

Here, you can see versions of important packages, information about
where you are working, and information about how you intend to interact
with the cluster. See
[`vignette("dide-cluster")`](https://mrc-ide.github.io/hipercow/articles/dide-cluster.md)
for example output you might expect to see, which includes information
about mapping of your paths onto those of the cluster, the version of R
you will use, and other information.

If you have issues with `hipercow` we will always want to see the output
of
[`hipercow_configuration()`](https://mrc-ide.github.io/hipercow/reference/hipercow_configuration.md).

## Running your first task

The first time you use the tools (ever, in a while, or on a new machine)
we recommend sending off a tiny task to make sure that everything is
working as expected:

``` r
id <- task_create_expr(sessionInfo())
#> ✔ Submitted task '5ac7aaa10eea76911082ad08b30a0bb7' using 'example'
```

This creates a new task that will run the expression
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) on the
cluster. The
[`task_create_expr()`](https://mrc-ide.github.io/hipercow/reference/task_create_expr.md)
function works by so-called [“non standard
evaluation”](https://adv-r.hadley.nz/metaprogramming.html) and the
expression is not evaluated from your R session, but sent to run on
another machine.

The `id` returned is just an ugly hex string:

``` r
id
#> [1] "5ac7aaa10eea76911082ad08b30a0bb7"
```

Many other functions accept this `id` as an argument. You can get the
status of the task, which will have finished now because it really does
not take very long:

``` r
task_status(id)
#> [1] "success"
```

Once the task has completed you can inspect the result:

``` r
task_result(id)
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#> [1] compiler_4.5.2 cli_3.6.5      withr_3.0.2    hipercow_1.1.8 rlang_1.1.6
```

Because we are using the “example” driver here, this is the same as the
result that you’d get running
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) directly,
just with more steps. See
[`vignette("dide-cluster")`](https://mrc-ide.github.io/hipercow/articles/dide-cluster.md)
for examples running on the DIDE cluster.

## Using functions you have written

It’s unlikely that the code you want to run on the cluster is one of the
functions built into R itself; more likely you have written a simulation
or similar and you want to run *that* instead. In order to do this, we
need to tell the cluster where to find your code. There are two broad
places where code that you want to run is likely to be found **script
files** and **packages**; we start with the former here, and deal with
packages in much more detail in
[`vignette("packages")`](https://mrc-ide.github.io/hipercow/articles/packages.md).

Suppose you have a file `simulation.R` containing some simulation:

``` r
random_walk <- function(x, n_steps) {
  ret <- numeric(n_steps)
  for (i in seq_len(n_steps)) {
    x <- rnorm(1, x)
    ret[[i]] <- x
  }
  ret
}
```

We can’t run this on the cluster immediately, because the cluster does
not know about the new function:

``` r
id <- task_create_expr(random_walk(0, 10))
#> ✔ Submitted task '682c061405c87026024acf4f681207ba' using 'example'
task_wait(id)
#> [1] FALSE
task_status(id)
#> [1] "failure"
task_result(id)
#> <simpleError in random_walk(0, 10): could not find function "random_walk">
```

(See
[`vignette("troubleshooting")`](https://mrc-ide.github.io/hipercow/articles/troubleshooting.md)
for more on failures.)

We need to tell `hipercow` to
[`source()`](https://rdrr.io/r/base/source.html) the file `simulation.R`
before running the task. To do this we use
[`hipercow_environment_create()`](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
to create an “environment” (not to be confused with R’s environments) in
which to run things:

``` r
hipercow_environment_create(sources = "simulation.R")
#> ✔ Created environment 'default'
```

Now we can run our simulation:

``` r
id <- task_create_expr(random_walk(0, 10))
#> ✔ Submitted task 'c248c8d90080aaeb0cc0b3203741d0ec' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#>  [1]  0.68985087  1.78805756  0.64603243  1.89653737  3.60321873  0.05188865
#>  [7] -0.36023142  0.42364646  0.74961744  1.74978662
```

- You can have multiple environments and each task can be set to run in
  a different environment
- Each environment can source any number of source files, and load any
  number of packages
- This will become the mechanism by which environments on parallel
  workers (via `parallel`, `future` or `rrq`) will set up their
  environments

Read more about environments in
[`vignette("environments")`](https://mrc-ide.github.io/hipercow/articles/environments.md)

## Getting information about tasks

Once you have created (and submitted) tasks, they will be queued by the
cluster and eventually run. The hope is that we surface enough
information to make it easy for you to see how things are going and what
has gone wrong.

### Fetching information with `task_info()`

The primary function for fetching information about a task is
[`task_info()`](https://mrc-ide.github.io/hipercow/reference/task_info.md):

``` r
task_info(id)
#> 
#> ── task c248c8d90080aaeb0cc0b3203741d0ec (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: random_walk(0, 10)
#>   • Locals: (none)
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:00.527466 (moments ago)
#> ℹ Started at 2025-11-26 12:57:00.62706 (moments ago; waited 100ms)
#> ℹ Finished at 2025-11-26 12:57:00.628453 (moments ago; ran for 2ms)
```

This prints out core information about the task; its identifier
(`c248c8d90080aaeb0cc0b3203741d0ec`) and status (`success`), along with
information about what sort of task it was, what expression it had,
variables it used, the environment it executed in and the time that key
events happened for the task (when it was created, started and
finished).

This display is meant to be friendly; if you need to compute on this
information, you can access the times by reading the `$times` element of
the
[`task_info()`](https://mrc-ide.github.io/hipercow/reference/task_info.md)
return value:

``` r
task_info(id)$times
#>                   created                   started                  finished 
#> "2025-11-26 12:57:00 UTC" "2025-11-26 12:57:00 UTC" "2025-11-26 12:57:00 UTC"
```

Likewise, the information about the task itself is within `$data`. To
work with the underling data you might just unclass the object to see
the structure:

``` r
unclass(task_info(id))
#> $id
#> [1] "c248c8d90080aaeb0cc0b3203741d0ec"
#> 
#> $status
#> [1] "success"
#> 
#> $data
#> $data$type
#> [1] "expression"
#> 
#> $data$id
#> [1] "c248c8d90080aaeb0cc0b3203741d0ec"
#> 
#> $data$time
#> [1] "2025-11-26 12:57:00 UTC"
#> 
#> $data$path
#> [1] "."
#> 
#> $data$environment
#> [1] "default"
#> 
#> $data$envvars
#>            name
#> 1      USER_KEY
#> 2   USER_PUBKEY
#> 3 R_GC_MEM_GROW
#>                                                                       value
#> 1     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key
#> 2 /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key.pub
#> 3                                                                         3
#>   secret
#> 1  FALSE
#> 2  FALSE
#> 3  FALSE
#> 
#> $data$parallel
#> NULL
#> 
#> $data$expr
#> random_walk(0, 10)
#> 
#> $data$variables
#> NULL
#> 
#> 
#> $driver
#> [1] "example"
#> 
#> $times
#>                   created                   started                  finished 
#> "2025-11-26 12:57:00 UTC" "2025-11-26 12:57:00 UTC" "2025-11-26 12:57:00 UTC" 
#> 
#> $retry_chain
#> NULL
```

but note that the exact structure is subject to (infrequent) change.

### Fetching logs with `task_log_show`

Every task will produce some logs, and these can be an important part of
understanding what they did and why they went wrong.

You can view the log with
[`task_log_show()`](https://mrc-ide.github.io/hipercow/reference/task_log.md)

``` r
task_log_show(id)
#> ✖ No logs for task 'c248c8d90080aaeb0cc0b3203741d0ec' (yet?)
```

This prints the contents of the logs to the screen; you can access the
values directly with `task_log_value(id)`. The format of the logs will
be generally the same for all tasks; after the header saying where we
are running, some information about the task will be printed (its
identifier, the time, details about the task itself), then any logs that
come from calls to [`message()`](https://rdrr.io/r/base/message.html)
and [`print()`](https://rdrr.io/r/base/print.html) within the queued
function (within the “task logs” section; here that is empty because our
task prints nothing). Finally, a summary will be printed with the final
status, final time (and elapsed time), then any warnings that were
produced will be flushed (see
[`vignette("troubleshooting")`](https://mrc-ide.github.io/hipercow/articles/troubleshooting.md)
for more on warnings).

There is a second log too, the “outer” log, which is generally less
interesting so it is not the default. These logs come from the cluster
scheduler itself and show the startup process that leads up to (and
after) the code that `hipercow` itself runs. It will differ from driver
to driver. In addition, this log may not be available forever; the DIDE
cluster retains it only for a couple of weeks:

``` r
task_log_show(id, outer = TRUE)
#> ✖ No logs for task 'c248c8d90080aaeb0cc0b3203741d0ec' (yet?)
```

The logs returned by `task_log_show(id, outer = FALSE)` are the logs
generated by the statement containing `Rscript -e`.

### Watching logs with `task_log_watch`

If your task is still running, you can stream logs to your computer
using
[`task_log_watch()`](https://mrc-ide.github.io/hipercow/reference/task_log.md);
this will print new logs line-by-line as they arrive (with a delay of up
to 1s by default). This can be useful while debugging something to give
the illusion that you’re running it locally.

Using `Ctrl-C` (or `ESC` in RStudio) to escape will only stop log
streaming and not the underlying task.

## Running many tasks at once

Running one task on the cluster is nice, because it takes the load off
your laptop, but it’s generally not why you’re going through this
process. More likely, you have **many, similar, tasks** that you want to
set running at once. You might be:

- Fitting a model to a series of countries
- Exploring uncertainty in a parameter
- Running a series of stochastic processes

In all these cases, you would want to submit a group of **related**
tasks, sharing a common function, but differing in the data passed into
that function. We call this the “bulk interface”, and it is the simplest
and usually most effective way of getting started with parallel
computing.

This sort of problem is referred to as [“embarrassingly
parallel”](https://en.wikipedia.org/wiki/Embarrassingly_parallel); this
is not a pejorative, it just means that your work decomposes into a
bunch of independent chunks and all we have to do is start them. You are
already familiar with things that can be run this way: anything that can
be run with R’s `lapply` could be parallelised.

There are two similar bulk creation functions, which differ based on the
way the data you have are structured:

- `task_create_bulk_call` is used where you have a `list`, where each
  element represents the key input to some computation (this is similar
  to [`lapply()`](https://rdrr.io/r/base/lapply.html))
- `task_create_bulk_expr` is used where you have a `data.frame`, where
  each **row** represents the inputs to some computation (this is a
  little similar to `dplyr`’s
  [`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html)
  support)

### Bulk call, or “parallel map”

The bulk call interface is the one that might feel most familiar to you;
it is modelled on ideas from functions like `lapply` (or, if you use
`purrr`, its `map()` function). The idea is simple, we have a list of
data and we apply some function to each element within it.

We’ll start with a reminder of how `lapply` works, then adapt this to
run in parallel on a cluster. Imagine that we want to run some simple
simulation with a different parameter. In this example we simulate `n`
samples from a normal distribution and compute the observed mean and
variance:

``` r
mysim <- function(mu, sd = 1, n = 1000) {
  r <- rnorm(n, mu, sd)
  c(mean(r), var(r))
}
```

We can run locally it like this:

``` r
source("simulation-bulk.R")
mysim(0, 1)
#> [1] 0.004339615 1.000001295
```

Suppose that we have a vector of means to run this with:

``` r
mu <- c(0, 1, 2, 3, 4)
```

We can apply `mysim` to each of the elements of `mu` by writing:

``` r
lapply(mu, mysim, sd = 1)
#> [[1]]
#> [1] -0.01626336  0.93585134
#> 
#> [[2]]
#> [1] 1.0384556 0.9996111
#> 
#> [[3]]
#> [1] 1.9584970 0.9689123
#> 
#> [[4]]
#> [1] 3.057711 1.006066
#> 
#> [[5]]
#> [1] 4.0052268 0.9658853
```

Of note here:

- Only the `mu` argument was iterated over
- We provided a `sd` argument that was passed through to every call to
  `mysim`
- We get back a list in return, the same length as `mu`, with each
  element the result of applying `mysim` to that element.

Nothing in the above said anything about the *order* in which these
calculations were carried out; one might assume that we applied `myfun`
to the first element of `mu` at once, then the second, but that is just
conjecture. This last point seems a bit silly, but is a useful condition
to think about when considering what can be parallelised; if you can run
a “loop” backwards and get the same answer (ignoring things like the
specific draws from random number generating functions) then your
problem is well suited to being parallelised.

Our function `mysim` is in a file called `simulation-bulk.R`, which
we’ll add to our environment so that it’s available on the cluster
(alongside `random_walk` from above):

``` r
hipercow_environment_create(sources = c("simulation.R", "simulation-bulk.R"))
#> ✔ Updated environment 'default'
```

We can then submit tasks to the cluster using `task_create_bulk_call`:

``` r
bundle <- task_create_bulk_call(mysim, mu, args = list(sd = 1))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'rearmost_chrysalis' with 5 tasks
bundle
#> → <hipercow_bundle 'rearmost_chrysalis' with 5 tasks>
```

This creates a **task bundle** which groups together related tasks.
There is a whole set of functions for working with bundles that behave
similarly to the task query functions. So where
[`task_status()`](https://mrc-ide.github.io/hipercow/reference/task_status.md)
retrieves the status for a single task, we can get the status for a
bundle by running

``` r
hipercow_bundle_status(bundle)
#> [1] "success"   "success"   "submitted" "submitted" "submitted"
```

which returns a vector over all tasks included in the bundle. You can
also “reduce” this status to the “worst” status over all tasks:

``` r
hipercow_bundle_status(bundle, reduce = TRUE)
#> [1] "success"
```

Similarly, you can wait for a whole bundle to complete

``` r
hipercow_bundle_wait(bundle)
#> [1] TRUE
```

And then get the results as a list

``` r
hipercow_bundle_result(bundle)
#> [[1]]
#> [1] -0.01651931  1.01005770
#> 
#> [[2]]
#> [1] 0.9820795 0.9469256
#> 
#> [[3]]
#> [1] 2.013578 1.084181
#> 
#> [[4]]
#> [1] 3.041790 1.075379
#> 
#> [[5]]
#> [1] 4.0323540 0.9662693
```

This flow (`create`, `wait`, `result`) is equivalent to `lapply` and
produces data of the same shape in return, but the tasks will be carried
out in parallel! Each task is submitted to the cluster and picked up by
the first available node. You might submit 100 tasks and if the cluster
is quiet, a few seconds later all of them will be running at the same
time.

We might want to vary *both* `mu` and `sd`, in which case it might be
convenient to keep track of our inputs in a `data.frame`:

``` r
pars <- data.frame(mu = mu, sd = sqrt(mu + 1))
```

We can use `task_create_bundle_call` with this, too:

``` r
b <- task_create_bulk_call(mysim, pars)
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'carneous_hoki' with 5 tasks
hipercow_bundle_wait(b)
#> [1] TRUE
hipercow_bundle_result(b)
#> [[1]]
#> [1] 0.01579004 1.02915967
#> 
#> [[2]]
#> [1] 0.9373602 1.9673634
#> 
#> [[3]]
#> [1] 1.967042 3.022058
#> 
#> [[4]]
#> [1] 3.011726 4.121857
#> 
#> [[5]]
#> [1] 4.060191 4.956555
```

This iterates over the data in a **row-wise** way. Note that this is
very different to `lapply` which would iterate over **columns** (in
practice we find that this is almost never what people want). The names
of the columns must match the names of your function arguments and all
columns must be used.

We could have passed additional arguments here too, for example changing
`n`:

``` r
b <- task_create_bulk_call(mysim, pars, args = list(n = 40))
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'albinic_bird' with 5 tasks
```

### Bulk expression

We also support a bulk expression interface, which can be clearer than
the above.

``` r
b <- task_create_bulk_expr(mysim(mu, sd, n = 40), pars)
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'trainsick_husky' with 5 tasks
```

This would again work row-wise over `pars` but evaluate the expression
in the first argument with the data found in the `data.frame`. This
would allow you to use different column names if convenient:

``` r
pars <- data.frame(mean = mu, stddev = sqrt(mu + 1))
b <- task_create_bulk_expr(mysim(mean, stddev, n = 40), pars)
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'dentine_gorilla' with 5 tasks
```

### More on bundles

You can do most things to bundles that you can do to tasks:

| Action                         | Single task      | Bundle                      |
|--------------------------------|------------------|-----------------------------|
| Get result                     | `task_result`    | `hipercow_bundle_result`    |
| Wait for completion            | `task_wait`      | `hipercow_bundle_wait`      |
| Retry failed tasks (see below) | `task_retry`     | `hipercow_bundle_retry`     |
| List                           | `task_list`      | `hipercow_bundle_list`      |
| Cancel                         | `task_cancel`    | `hipercow_bundle_cancel`    |
| Get log value                  | `task_log_value` | `hipercow_bundle_log_value` |

There is no equivalent of `task_log_watch` or `task_log_show` because we
can’t easily do this for multiple tasks at the same time in a
satisfactory way.

`hipercow_bundle_delete` will delete bundles, but leave tasks alone.
`hipercow_purge` will delete tasks, causing actual deletion of data.

Some of these functions naturally have slightly different semantics to
the single-task function; for example,
[`hipercow_bundle_result()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_result.md)
returns a list of results and `hipewcow_bundle_wait` has an option
`fail_early` to control if it shold return `FALSE` as soon as any task
fails.

### Picking bundles back up again later

You can use the
[`hipercow_bundle_list()`](https://mrc-ide.github.io/hipercow/reference/hipercow_bundle_list.md)
function to list known bundles:

``` r
hipercow_bundle_list()
#>                 name                time
#> 1    dentine_gorilla 2025-11-26 12:57:03
#> 2    trainsick_husky 2025-11-26 12:57:03
#> 3       albinic_bird 2025-11-26 12:57:03
#> 4      carneous_hoki 2025-11-26 12:57:02
#> 5 rearmost_chrysalis 2025-11-26 12:57:02
```

Each bundle has a name (automatically generated by default) and the time
that it was created. If you have launched a bundle and for some reason
lost your session (e.g., Windows update has rebooted your computer) you
can use this to get your ids back.

``` r
name <- hipercow_bundle_list()$name[[1]]
bundle <- hipercow_bundle_load(name)
```

If you’re not sure what you launched, you can use `task_info`:

``` r
task_info(bundle$ids[[1]])
#> 
#> ── task eafb96aba28c725742569a0969405e64 (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: mysim(mean, stddev, n = 40)
#>   • Locals: mean and stddev
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:03.87835 (moments ago)
#> ℹ Started at 2025-11-26 12:57:03.931952 (moments ago; waited 54ms)
#> ℹ Finished at 2025-11-26 12:57:03.933635 (moments ago; ran for 2ms)
```

You can make this process a bit more friendly by setting your own name
into the bundle when creating it using the `bundle_name` argument:

``` r
pars <- data.frame(mean = mu, stddev = sqrt(mu + 1))
b <- task_create_bulk_expr(mysim(mean, stddev, n = 40), pars,
                           bundle_name = "final_runs_v2")
#> ✔ Submitted 5 tasks using 'example'
#> ✔ Created bundle 'final_runs_v2' with 5 tasks
```

### Making bundles from tasks

You can also make a bundle yourself from a group of tasks; this may be
convenient if you need to launch a number of tasks individually for some
reason but want to then consider them together as a group.

``` r
id1 <- task_create_expr(mysim(1, 2))
#> ✔ Submitted task '5f801f4c0b74c041772731bbf3e7e31b' using 'example'
id2 <- task_create_expr(mysim(2, 2))
#> ✔ Submitted task '12b9189aed42adf303dd8e718300fc23' using 'example'
b <- hipercow_bundle_create(c(id1, id2), "my_new_bundle")
#> ✔ Created bundle 'my_new_bundle' with 2 tasks
b
#> → <hipercow_bundle 'my_new_bundle' with 2 tasks>
```

We can then use this bundle as above:

``` r
hipercow_bundle_status(b)
#> [1] "success" "success"
hipercow_bundle_wait(b)
#> [1] TRUE
```

## Parallel tasks

So far, the tasks we submitted have been run using a single core on the
cluster, with no special other requests made. Here is a simple example
using two cores; we’ll use
[`hipercow_resources()`](https://mrc-ide.github.io/hipercow/reference/hipercow_resources.md)
to specify we want two cores on the cluster, and
[`hipercow_parallel()`](https://mrc-ide.github.io/hipercow/reference/hipercow_parallel.md)
to say that we want to set up two processes on those cores, using the
`parallel` package. (We also support
[`future`](https://future.futureverse.org/)).

``` r
resources <- hipercow_resources(cores = 2)
id <- task_create_expr(
  parallel::clusterApply(NULL, 1:2, function(x) Sys.sleep(5)),
  parallel = hipercow_parallel("parallel"),
  resources = resources)
#> ✔ Submitted task '046775a8a81510cc4f18a5290a06d604' using 'example'
task_wait(id)
#> [1] TRUE
task_info(id)
#> 
#> ── task 046775a8a81510cc4f18a5290a06d604 (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: parallel::clusterApply(NULL, 1:2, function(x) Sys.sleep(5))
#>   • Locals: (none)
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:04.483718 (moments ago)
#> ℹ Started at 2025-11-26 12:57:04.571042 (moments ago; waited 88ms)
#> ℹ Finished at 2025-11-26 12:57:09.953037 (moments ago; ran for 5.4s)
```

Both of our parallel tasks are to sleep for 5 seconds. We use
[`task_info()`](https://mrc-ide.github.io/hipercow/reference/task_info.md)
to report how long it took for those two runs to execute; if they ran
one-by-one, we’d expect around 10 seconds, but we are seeing a much
shorter time than that, so our pair of processes are running at the same
time.

For details on specifying resources and launching different kinds of
parallel tasks, see
[`vignette("parallel")`](https://mrc-ide.github.io/hipercow/articles/parallel.md).

## Understanding where variables come from

Suppose our simulation started not from 0, but from some point that we
have computed locally (say `x`, imaginatively)

``` r
x <- 100
```

You can use this value to start the simulation by running:

``` r
id <- task_create_expr(random_walk(x, 10))
#> ✔ Submitted task 'b7692cf4511d0df3eec388a83df4c648' using 'example'
```

Here the `x` value has come from the environment where the expression
passed into
[`task_create_expr()`](https://mrc-ide.github.io/hipercow/reference/task_create_expr.md)
was found (specifically, we use the [`rlang` “tidy
evaluation”](https://rlang.r-lib.org/reference/topic-defuse.html)
framework you might be familiar with from `dplyr` and friends).

``` r
task_wait(id)
#> [1] TRUE
task_result(id)
#>  [1] 98.37020 98.21695 97.32176 94.59951 92.63425 91.72488 93.64525 94.08023
#>  [9] 94.01831 94.53032
```

If you pass in an expression that references a value that does not exist
locally, you will get a (hopefully) informative error message when the
task is created:

``` r
id <- task_create_expr(random_walk(starting_point, 10))
#> Error in `rlang::env_get_list()`:
#> ! Can't find `starting_point` in environment.
```

## Cancelling tasks

You can cancel a task if it has been submitted and not completed, using
[`task_cancel()`](https://mrc-ide.github.io/hipercow/reference/task_cancel.md):

For example, here’s a task that will sleep for 10 seconds, which we
submit to the cluster:

``` r
id <- task_create_expr(Sys.sleep(10))
#> ✔ Submitted task 'd200f92764065ea082e1c029ac7b7be1' using 'example'
```

Having decided that this is a silly idea, we can try and cancel it:

``` r
task_cancel(id)
#> ✔ Successfully cancelled 'd200f92764065ea082e1c029ac7b7be1'
#> [1] TRUE
task_status(id)
#> [1] "cancelled"
task_info(id)
#> 
#> ── task d200f92764065ea082e1c029ac7b7be1 (cancelled) ───────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: Sys.sleep(10)
#>   • Locals: (none)
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:11.907291 (moments ago)
#> ✖ Start time unknown!
#> ℹ Finished at 2025-11-26 12:57:11.970522 (moments ago; ran for ???)
```

You can cancel a task that is submitted (waiting to be picked up by a
cluster) or running (though not all drivers will support this; we need
to add this to the example driver still, which will improve this
example!).

You can cancel many tasks at once by passing a vector of identifiers at
the same time. Tasks that have finished (successfully or not) cannot be
cancelled.

## Retrying tasks

There are lots of reasons why you might want to retry a task. For
example:

- it failed but you think it might work next time
- you updated a package that it used, and want to try again with the new
  version
- you don’t like the output from some stochastic function and want to
  generate new output
- you cancelled the task but want to try again now

You can retry tasks with
[`task_retry()`](https://mrc-ide.github.io/hipercow/reference/task_retry.md),
which is easier than submitting a new task with the same content, and
also preserves a link between retried tasks.

Our random walk will give slightly different results each time we use
it, so we demonstrate the idea with that:

``` r
id1 <- task_create_expr(random_walk(0, 10))
#> ✔ Submitted task 'd8b17525a0b9616559cbaea4bf93a7c8' using 'example'
task_wait(id1)
#> [1] TRUE
task_result(id1)
#>  [1] -0.2735067  0.8666298  1.3621934  1.5848550  2.4821706  1.4067327
#>  [7]  1.3959421  2.8494242  2.0587303  0.1845566
```

Here we ran a random walk and it got to 0.1845566, which is clearly not
what we were expecting. Let’s try it again:

``` r
id2 <- task_retry(id1)
#> ✔ Submitted task '80feae24c80da87fd46ee2ad041b3c3c' using 'example'
```

Running
[`task_retry()`](https://mrc-ide.github.io/hipercow/reference/task_retry.md)
creates a *new* task, with a new id `80feae...` compared with
`d8b175...`.

Once this task has finished, we get a different result:

``` r
task_wait(id2)
#> [1] TRUE
task_result(id2)
#>  [1] -1.5111621 -0.7961957 -1.2321372 -0.9085649 -1.3936681  0.1597208
#>  [7] -0.7046267 -1.5533940 -1.3277809 -0.8835975
```

Much better!

We get a hint that this is a retried task from the
[`task_info()`](https://mrc-ide.github.io/hipercow/reference/task_info.md)

``` r
task_info(id2)
#> 
#> ── task 80feae24c80da87fd46ee2ad041b3c3c (success) ─────────────────────────────
#> ℹ Submitted with 'example'
#> ℹ Task type: expression
#>   • Expression: random_walk(0, 10)
#>   • Locals: (none)
#>   • Environment: default
#>     USER_KEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key
#>     USER_PUBKEY:
#>     /home/runner/work/_temp/hv-20251126-2c2e7897ab86/hipercow/example/key.pub
#>     R_GC_MEM_GROW: 3
#> ℹ Created at 2025-11-26 12:57:12.10567 (moments ago)
#> ℹ Started at 2025-11-26 12:57:13.207253 (moments ago; waited 1.1s)
#> ℹ Finished at 2025-11-26 12:57:13.208932 (moments ago; ran for 2ms)
#> ℹ Last of a chain of a task retried 1 time
```

You can see the full chain of retries here:

``` r
task_info(id2)$retry_chain
#> [1] "d8b17525a0b9616559cbaea4bf93a7c8" "80feae24c80da87fd46ee2ad041b3c3c"
```

Once a task has been retried it affects how you interact with the
previous ids; by default they follow through to the most recent element
in the chain:

``` r
task_result(id1)
#>  [1] -1.5111621 -0.7961957 -1.2321372 -0.9085649 -1.3936681  0.1597208
#>  [7] -0.7046267 -1.5533940 -1.3277809 -0.8835975
task_result(id2)
#>  [1] -1.5111621 -0.7961957 -1.2321372 -0.9085649 -1.3936681  0.1597208
#>  [7] -0.7046267 -1.5533940 -1.3277809 -0.8835975
```

You can get the original result back by passing the argument
`follow = FALSE`:

``` r
task_result(id1, follow = FALSE)
#>  [1] -0.2735067  0.8666298  1.3621934  1.5848550  2.4821706  1.4067327
#>  [7]  1.3959421  2.8494242  2.0587303  0.1845566
task_result(id2)
#>  [1] -1.5111621 -0.7961957 -1.2321372 -0.9085649 -1.3936681  0.1597208
#>  [7] -0.7046267 -1.5533940 -1.3277809 -0.8835975
```

Only tasks that have been completed (`success`, `failure` or
`cancelled`) can be retried, and doing so adds a new task to the *end*
of the chain; there is no branching. Retrying the `id1` here would
create the chain `id1 -> id2 -> id3`, and following would select `id3`
for any of the three tasks in the chain.

You cannot currently change any property of a retried task, we may
change this in future.
