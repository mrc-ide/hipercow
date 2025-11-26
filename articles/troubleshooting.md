# Troubleshooting

``` r
library(hipercow)
```

## My task failed

Oh no!

### Caused by an error in your code

If your task status is `failure` that *probably* indicates an error in
your code. There are lots of reasons that this could be for, and the
first challenge is working out what happened.

``` r
id <- task_create_expr(mysimulation(10))
#> ✔ Submitted task 'efb7c0024f0b471243984fff563237b5' using 'example'
```

This task will fail, and
[`task_status()`](https://mrc-ide.github.io/hipercow/reference/task_status.md)
will report `failure`

``` r
task_status(id)
#> [1] "failure"
```

The first place to look is the result of the task itself. Unlike an
error in your console, an error that happens on the cluster can be
returned and inspected:

``` r
task_result(id)
#> <simpleError in mysimulation(10): could not find function "mysimulation">
```

In this case the error is because the function `mysimulation` does not
exist! This is because we’ve forgotten to tell the cluster where to find
it.

The other place worth looking is the task log (via
[`task_log_show()`](https://mrc-ide.github.io/hipercow/reference/task_log.md)),
which provides more diagnostic information. We will often ask you to
show this to us.

``` r
task_log_show(id)
#> 
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-300b23b63e93' 
#> ℹ library paths:
#> • /home/runner/work/_temp/Library
#> • /opt/R/4.5.2/lib/R/site-library
#> • /opt/R/4.5.2/lib/R/library
#> ℹ id: efb7c0024f0b471243984fff563237b5
#> ℹ starting at: 2025-11-26 12:57:56.621419
#> ℹ Task type: expression
#> • Expression: mysimulation(10)
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-300b23b63e93/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-300b23b63e93/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ───────────────────────────────────────────────────────────────── task logs ↓ ──
#> 
#> ───────────────────────────────────────────────────────────────── task logs ↑ ──
#> ✖ status: failure
#> ✖ Error: could not find function "mysimulation"
#> ℹ finishing at: 2025-11-26 12:57:56.621419 (elapsed: 0.3294 secs)
```

In this case the task log does not have anything very interesting in it.

Here’s another example, something that might work perfectly well on your
machine, but fails on the cluster:

``` r
id <- task_create_expr(read.csv("c:/myfile.csv"))
#> ✔ Submitted task 'ae02aab6c7a27f9026e807f7037b03c8' using 'example'
```

Here is the error, which is a bit less informative this time:

``` r
task_result(id)
#> <simpleError in file(file, "rt"): cannot open the connection>
```

The log gives a better idea of what is going on - the file
`c:/myfile.csv` does not exist (because it is not found on the cluster;
using relative paths is much preferred to absolute paths)

``` r
task_log_show(id)
#> 
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-300b23b63e93' 
#> ℹ library paths:
#> • /home/runner/work/_temp/Library
#> • /opt/R/4.5.2/lib/R/site-library
#> • /opt/R/4.5.2/lib/R/library
#> ℹ id: ae02aab6c7a27f9026e807f7037b03c8
#> ℹ starting at: 2025-11-26 12:57:57.957014
#> ℹ Task type: expression
#> • Expression: read.csv("c:/myfile.csv")
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-300b23b63e93/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-300b23b63e93/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ───────────────────────────────────────────────────────────────── task logs ↓ ──
#> 
#> ───────────────────────────────────────────────────────────────── task logs ↑ ──
#> ✖ status: failure
#> ✖ Error: cannot open the connection
#> ! 1 warning found:
#> • cannot open file 'c:/myfile.csv': No such file or directory
#> ℹ finishing at: 2025-11-26 12:57:57.957014 (elapsed: 0.3579 secs)
```

The real content of the error message is present in the warning! You can
also get the warnings with

``` r
task_result(id)$warnings
#> [[1]]
#> <simpleWarning in file(file, "rt"): cannot open file 'c:/myfile.csv': No such file or directory>
```

Which will be a list of all warnings generated during the execution of
your task (even if it succeeds). The traceback also shows what happened:

``` r
task_result(id)$trace
#>      ▆
#>   1. ├─rlang::try_fetch(...)
#>   2. │ ├─base::tryCatch(...)
#>   3. │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>   4. │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>   5. │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>   6. │ └─base::withCallingHandlers(...)
#>   7. ├─hipercow:::task_eval_expression(data, envir, verbose)
#>   8. │ ├─hipercow:::eval_with_hr(...)
#>   9. │ │ └─base::force(expr)
#>  10. │ └─base::eval(data$expr, envir)
#>  11. │   └─base::eval(data$expr, envir)
#>  12. ├─utils::read.csv("c:/myfile.csv")
#>  13. │ └─utils::read.table(...)
#>  14. │   └─base::file(file, "rt")
#>  15. └─base::.handleSimpleError(...)
#>  16.   └─rlang (local) h(simpleError(msg, call))
#>  17.     └─handlers[[2L]](cnd)
```

### Caused by an error during startup

These are harder to troubleshoot but we can still pull some information
out. The example here was a real-world case and illustrates one of the
issues with using a shared filesystem in the way that we do here.

Suppose you have some code in `mycode.R`:

``` r
times2 <- function(x) {
  2 * x
}
```

We can create an environment with this code and use it just fine:

``` r
hipercow_environment_create(sources = "mycode.R")
#> ✔ Created environment 'default'
id <- task_create_expr(times2(10))
#> ✔ Submitted task '99b6c748350bcbb08d5d87f9084295e2' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> [1] 20
```

…but imagine then you’re editing the file and save the file but it is
not syntactically correct:

``` r
times2 <- function(x) {
  2 * x
}
newfun <- function(x)
```

And then you either submit a task, **or** a task that you have
previously submitted gets run (which could happen ages after you submit
it if the cluster is busy).

``` r
id <- task_create_expr(times2(10))
#> ✔ Submitted task 'befe8d147b20d2d6b3e8b977803b066c' using 'example'
task_wait(id)
#> [1] FALSE
task_status(id)
#> [1] "failure"
```

The error here has happened before getting to your code - it is
happening when the source files are loaded. The log makes this a bit
clearer:

``` r
task_log_show(id)
#> 
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-300b23b63e93' 
#> ℹ library paths:
#> • /home/runner/work/_temp/Library
#> • /opt/R/4.5.2/lib/R/site-library
#> • /opt/R/4.5.2/lib/R/library
#> ℹ id: befe8d147b20d2d6b3e8b977803b066c
#> ℹ starting at: 2025-11-26 12:58:00.606184
#> ℹ Task type: expression
#> • Expression: times2(10)
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-300b23b63e93/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-300b23b63e93/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ℹ Loading environment 'default'...
#> • packages: (none)
#> • sources: mycode.R
#> • globals: (none)
#> ✖ status: failure
#> ✖ Error: 5:0: unexpected end of input 3: } 4: newfun <- function(x)   ^
#> ℹ finishing at: 2025-11-26 12:58:00.606184 (elapsed: 0.3433 secs)
```

### My task got stuck at `submitted`

(Previous users of `didehpc` may recognise this as being stuck at
`PENDING`).

This is the most annoying one, and can happen for many reasons. You can
see via the [web
interface](https://mrc-ide.github.io/hipercow/articles/mrcdata.dide.ic.ac.uk/hpc/)
or the Microsoft cluster tools that your task has failed but `hipercow`
is reporting it as pending. This happens when something has failed
during the script that runs *before* any `hipercow` code runs on the
cluster.

Things that have triggered this situation in the past:

- An error in the Microsoft cluster tools
- A misconfigured node (sometimes they are missing particular software)
- A networking issue
- Gremlins
- Network path mapping error
- Running out of disk space after submitting your job

There are doubtless others.

If you suspect your task has become stuck at `submitted` (but is not
actually running any more) you should try one or more of:

- run
  [`task_info()`](https://mrc-ide.github.io/hipercow/reference/task_info.md)
  with your id which will fetch this true id and tell you about any
  discrepancy
- checking the web portal to see if the task is really listed as queued.
  Check the failed tasks and see if it’s there
- look at the *outer* logs (`task_log_show(id, outer = TRUE)`) which
  will show you the scheduler’s logs for this task, which may be
  informative.

### My code works on my computer but not on the cluster

In that case, something is different between how the cluster sees the
world, and how your computer sees it.

- Look in the logs to try and find the reason why the failing tasks are
  doing so.
- Are there variables in the global R environment on your local
  computer, that your code relies upon, that won’t be present on the
  cluster? Do you have local R packages or sources loaded which you
  haven’t told `hipercow` about?
- Or any system variables, or other dependencies which enable your task
  to work locally, but won’t be set up on a cluster node?
- Are you referring to any files visible to your local machine, but not
  on the cluster? Are you referring to `C:` for instance?
- (Rarely:) Are you viewing a cached version of any network storage on
  your local computer, that has not been synced to the real network
  storage view that the cluster has?
- Check that you have not run out of disk-space. The Q: quota is
  normally 15Gb.
- If you are running C code, check for other causes of indeterminate
  failures, such as uninitialised variables, or array out-of-bounds
  errors. These are unpredictable errors by nature, but surprisingly
  often you might get away with it on a local computer, while a cluster
  node behaves differently.
- If you are running stochastic code, check that you are *really* using
  the same random number seeds. If you are expecting exactly the same
  result from the cluster job, check that your code produces the same
  answer if you repeat it locally.

### Some of my tasks work on the cluster, but others fail

- Look in the logs to try and find the reason why the failing tasks do
  so.
- Try rerunning the failed tasks. Is it the same set that passes, and
  the same set that fails? In that case, consider what makes your tasks
  different - perhaps task-specific input data or parameters could cause
  the failures.
- If you find messages about `Error allocating a vector...` or
  `std::bad_alloc`, then try and work out the memory usage of a single
  task. Perhaps run it locally with task manager (Windows), or
  `top`/`htop` (macOS/Linux) running, and watch to see what the memory
  usage is. If the task is single-core, consider the total memory used
  if you run 8 or 16 instances on the same cluster machine. If the total
  memory exceeds the available, then behaviour will be undefined, and
  some tasks will likely fail.
- In the above example, note that someone else’s memory-hungry
  combination of tasks may affect your small-memory task, if they run on
  the same node. We don’t enforce any memory limits on tasks, which on
  the whole, is nice and convenient, but it carries the risk that the
  above can happen.
- Always check you’re not running out disk space. The Q: quota is
  normally 15Gb.
- Find what node your tasks were running on. If you consistently get
  errors on one node, but not others, then get in touch with Wes, as we
  do get node failures from time to time, where the fault is not obvious
  at first.

### My code is slower on the cluster than running locally!

- This is expected, especially for single-core tasks. Cluster nodes are
  often aiming to provide larger throughput, rather than better linear
  performance, so a single task may run slower on a cluster node than on
  your own computer. But the cluster node might be able to run 16 or
  more such tasks at once, without taking any longer, while you continue
  using your local computer for local things.
- If that is still insufficient, and you still want to compare timings
  in this way, then check that the cluster is doing *exactly* the same
  work as your local computer.

## I can’t connect to the cluster

There are lots of possible causes of this, and ways that this might
manifest as an error message, for example:

    Error in client_parse_submit(httr_text(r), 1L) :
      Job submission has likely failed; could be a login error

(we will add other error messages here as we catch them).

By the time you get here, we’ve thrown a pretty generic error because
for some reason we can’t tell what has happened. Possible reasons that
you might see an error like this:

- Your local internet connection has failed
- Your ZScaler session has timed out and needs re-authentication
- Your cluster session has timed out
- Your DIDE password has expired

You can check most of these by running

``` r
dide_check()
```

which will work through many common points of failure and report back
what does and does not work. If you want help with diagnosing this sort
of error, we would expect to see output from this command.

If that does work, but you are still having what looks like connection
problems, then try

``` r
hipercow_hello()
```

which will launch a simple job. If this does not work, and you want to
ask for help, we would like to see the whole output of this command.

If that works, but your actual job does not work, then something about
what you are submitting is causing the problem. In this case, if you are
asking for help, we would need to know something about your code, in
which case read on for the next section.

## Asking for help

If you need help, you can ask in the “Cluster” teams channel. This is
better than emailing Rich or Wes directly as they may not have time to
respond, or may be on leave.

When asking for help it is really important that you make it as easy as
possible for us to help you. This is surprisingly hard to do well, and
we would ask that you first take a look at these two short articles:

- [How to ask a good
  question](https://stackoverflow.com/help/how-to-ask)
- [How to create a minimal, reproducible
  example](https://stackoverflow.com/help/minimal-reproducible-example)

Things we will need to know:

- The contents of
  [`hipercow::hipercow_configuration()`](https://mrc-ide.github.io/hipercow/reference/hipercow_configuration.md)
- What you’ve tried doing
- The values of any errors (not just that they occurred!)
- Logs of the offending task if you have it

Too often, we will get requests for help with no information about what
was run, what packages or versions are being installed, etc. This means
your message sits there until we see it, we’ll ask for clarification -
that message sits there until you see it, you respond with a little more
information, and it may be days until we finally discover the root cause
of your problem, by which point we’re both quite fed up. We will never
complain if you provide “too much” information in a good effort to
outline where your problem is.

**Don’t say**

> Hi, I was running a cluster task, but it seems like it failed. I’m
> sure it worked the other day though! Do you know what the problem is?

**Do say**

> Since yesterday, my cluster task has stopped working.
>
> My DIDE username is `alicebobson` and my configuration is:
>
>     -- hipercow configuration -----
>     [etc]
>
> I have set up my cluster task with
>
>     # include short script here if you can!
>
> The task `43333cbd79ccbf9ede79556b592473c8` is one that failed with an
> error, and the log says
>
>     # contents of task_log_show(id) here

with this sort of information the problem may just jump out at us, or we
may be able to create the error ourselves - either way we may be able to
work on the problem and get back to you with a solution rather than a
request for more information.

Other tips, and reasons you may have been directed to this page:

- **Please provide the whole error message**. Do not provide only the
  line of that you think is interesting. You can store a great many
  lines of text in Teams, and you can always attach file if you need to.
  We would really like to see as much information as possible.
- **Please do not provide screenshots** or photos of text unless for
  some reason your computer has lost the ability to copy and paste.
- **Please provide as much context as possible** as to what you are
  working on, and where. Please don’t assume that we remember anything
  you told us in a previous discussion - we’ve probably forgotten and
  you have more context about your problem at the point where you ask
  than we do.
- **Please let us know how you got in**; if you have asked us about a
  problem and have solved it, please let us know it is working, anything
  extra you did to get it working, and anything we can do to make the
  documentation clearer.
- In general, please try and reduce the chance that our response to your
  message has to be another question from us to you. It may feel like
  you will get to your answer quicker if you don’t try and investigate
  at your end, but it will take much longer overall and use up more of
  everyone’s time.

We do want to help, but expect slower responses where we have to do lots
of discovery to find out what your problem is, it will take longer until
we find the time and energy to start digging. The more information you
provide, the more likely it is we can spot the error.
