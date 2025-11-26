# Environments

It is likely that the code that you want to run uses other code written
by someone else, or code that you have written yourself. This vignette
covers how you can make this code available to your tasks, covering:

- Loading packages automatically
- Using environments to avoid sending large files over the network

``` r
library(hipercow)
hipercow_init(driver = "example")
#> ✔ Initialised hipercow at '.' (/home/runner/work/_temp/hv-20251126-2b7892bd679)
#> ✔ Configured hipercow to use 'example'
```

## Basics

There are two main things that make up an “environment”:

- a set of packages that you would like attached (loaded via
  [`library()`](https://rdrr.io/r/base/library.html))
- a set of source files that you would like loaded into the session (via
  [`source()`](https://rdrr.io/r/base/source.html))

Because you can do anything you want within a source file (including
loading packages!) this is very flexible.

Here’s what is going on with the default environment:

``` r
id <- task_create_expr(list(session = sessionInfo(), objects = ls()))
#> ✔ Submitted task 'd3fba3e82aff9c7682fdf2a1f5c242e3' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> $session
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
#> 
#> $objects
#> character(0)
```

You can see that there are few **attached** packages (these are packages
that are loaded via `library` and whose functions are available) but
there are a couple of extra **loaded** packages, for example`hipercow`
and its dependencies such as `cli` (these are packages that R has loaded
but functions of which are not available without `::`).

The `objects` field shows the objects loaded into the session; this is
empty because the session has nothing in it.

### Loading packages

Your code might want to use functions from a package. For example, we
might want to use the `say` function from the `cowsay` package

``` r
id <- task_create_expr(say("Moooo", by = "cow"))
#> ✔ Submitted task '20fb72d79de496cde226c4c34f06e5b0' using 'example'
task_wait(id)
#> [1] FALSE
task_result(id)
#> <simpleError in say("Moooo", by = "cow"): could not find function "say">
```

This task fails and the error message here indicates the problem: the
`say()` function is not found. We could run this as `cowsay::say` but
this might be inconvenient or just not what we prefer to do. We need to
arrange for [`library(cowsay)`](https://github.com/sckott/cowsay) to be
run before trying to run our expression.

To do this, we update the **default environment**:

``` r
hipercow_environment_create(packages = "cowsay")
#> ✔ Created environment 'default'
```

This time our task succeeds:

``` r
id <- task_create_expr(say("Moooooo", by = "cow"))
#> ✔ Submitted task 'ef0c556570c4acb6fbe44f4a84f5a021' using 'example'
task_wait(id)
#> [1] FALSE
```

Looking at the logs, we can see the glorious output of our task:

``` r
task_log_show(id)
#> 
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-2b7892bd679' ─
#> ℹ library paths:
#> • /home/runner/work/_temp/Library
#> • /opt/R/4.5.2/lib/R/site-library
#> • /opt/R/4.5.2/lib/R/library
#> ℹ id: ef0c556570c4acb6fbe44f4a84f5a021
#> ℹ starting at: 2025-11-26 12:56:48.320871
#> ℹ Task type: expression
#> • Expression: say("Moooooo", by = "cow")
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-2b7892bd679/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-2b7892bd679/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ℹ Loading environment 'default'...
#> • packages: cowsay
#> • sources: (none)
#> • globals: (none)
#> ✖ status: failure
#> ✖ Error: there is no package called ‘cowsay’
#> ℹ finishing at: 2025-11-26 12:56:48.320871 (elapsed: 0.313 secs)
```

We can also see the logs say:

``` r
ℹ Loading environment 'default'...
• packages: cowsay
• sources: (none)
• globals: (none)
```

As the task starts up, and before it runs our code, it has loaded
`default` and that has attached `cowsay`.

You can see what is in the default environment by using
[`hipercow_environment_show()`](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)

``` r
hipercow_environment_show("default")
#> 
#> ── hipercow environment 'default' ──────────────────────────────────────────────
#> • packages: cowsay
#> • sources: (none)
#> • globals: (none)
```

which prints the same information.

You can list as many packages as you want, and they will be loaded in
order.

### Loading your own functions

Now, suppose we want to change the length of the moo that the cow
produces. We might write a function.

``` r
moo <- function(n) {
  sprintf("M%s!", strrep("o", n))
}
```

Ignoring the cluster for a minute, if you wanted to use this code you
would ordinarily use `source` to load it:

``` r
source("moo.R")
moo(5)
#> [1] "Mooooo!"
```

We need to do the same thing on the cluster, as by default it would not
be found:

``` r
id <- task_create_expr(say(moo(5), by = "cow"))
#> ✔ Submitted task '2d3ce63d047dbb930752d05385501c38' using 'example'
task_wait(id)
#> [1] FALSE
task_result(id)
#> <packageNotFoundError in library(p, character.only = TRUE): there is no package called ‘cowsay’>
```

The same error as before, but the consequence is different - this is
because **our** code is not present. We update our environment call to
add `moo.R`:

``` r
hipercow_environment_create(packages = "cowsay", sources = "moo.R")
#> ✔ Updated environment 'default'
```

(note that this replaces the previous definition of `default`). Now, we
can run our task:

``` r
id <- task_create_expr(say(moo(5), by = "cow"))
#> ✔ Submitted task '9f3a1b536ab5d0c19909a3825c20454a' using 'example'
task_wait(id)
#> [1] FALSE
task_log_show(id)
#> 
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-2b7892bd679' ─
#> ℹ library paths:
#> • /home/runner/work/_temp/Library
#> • /opt/R/4.5.2/lib/R/site-library
#> • /opt/R/4.5.2/lib/R/library
#> ℹ id: 9f3a1b536ab5d0c19909a3825c20454a
#> ℹ starting at: 2025-11-26 12:56:50.745717
#> ℹ Task type: expression
#> • Expression: say(moo(5), by = "cow")
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-2b7892bd679/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-2b7892bd679/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ℹ Loading environment 'default'...
#> • packages: cowsay
#> • sources: moo.R
#> • globals: (none)
#> ✖ status: failure
#> ✖ Error: there is no package called ‘cowsay’
#> ℹ finishing at: 2025-11-26 12:56:50.745717 (elapsed: 0.3142 secs)
```

You can list as many source files as you want. If you are brave, you
could use [`dir()`](https://rdrr.io/r/base/list.files.html) to do:

    hipercow_environment_create(
      sources = dir(glob2rx("R/", pattern = "*.R", full.names = TRUE)))

which would load all `.R` files within the directory `R/` of your
`hipercow` root. Be careful not to start recursively loading files
though; this would happen if you included
[`source()`](https://rdrr.io/r/base/source.html) commands within your R
files which, when followed, result in a cycle.

## Defining globals

A more advanced use of environments is to load large objects so that
they are ready when your task runs. Suppose we have a large object we
need to work with, for example a large loaded geojson file or a fit from
a model run, and we want to run a function on it.

The obvious thing to do is to write:

``` r
myfn(my_big_data, another_argument)
```

where

- `myfn` is our analysis function
- `my_big_data` is the large object
- `another_argument` is some other argument we’re passing to the call

When we do this, `hipercow` will save all the inputs to disk, and this
could be quite big. This could be even worse if this is part of a bulk
call, where multiple copies may be saved.

This problem can be so bad that we prevent it by default:

``` r
task_create_expr(length(my_big_data))
#> Error in `task_create_expr()`:
#> ! Object too large to save with task: 'my_big_data'
#> ✖ Objects saved with a hipercow task can only be 1 MB
#> ℹ You can increase the limit by increasing the value of the option
#>   'hipercow.max_size_local', even using 'Inf' to disable this check entirely
#>   (see the `vignette(hipercow::details)` vignette)
#> ℹ Better again, create large objects from your 'sources' argument to your
#>   environment, and then advertise this using the 'globals' argument (see the
#>   `vignette(hipercow::environments)` vignette)
```

So, how do we get this object into our session? Suppose that
`read_big_data()` is the function that we used to read the object in the
first place. We might write a source file `data.R` (the name is not
important):

``` r
my_big_data <- read_big_data("bigfile.shp")
```

and add this to our `sources`, but **also** add `my_big_data` to
`globals`. This tells `hipercow` that the object `my_big_data` will be
globally available and that if it sees it in an expression that you
create it should not try and save it:

``` r
hipercow_environment_create(sources = "data.R", globals = "my_big_data")
#> ✔ Updated environment 'default'
```

We can now create the task and run it:

``` r
id <- task_create_expr(length(my_big_data))
#> ✔ Submitted task '11eb8fb5f7b6826f89434f3f7640a809' using 'example'
task_wait(id)
#> [1] TRUE
task_result(id)
#> [1] 1000000
```

If you use this approach, you should be aware of a few things:

- By default, there is no guarantee that the object you see when
  creating the task is the same as the object loaded by `sources`. You
  can enable this by setting the option `hipercow.validate_globals` (see
  [`vignette("details")`](https://mrc-ide.github.io/hipercow/articles/details.md))
- You should generally load (via
  [`source()`](https://rdrr.io/r/base/source.html)) your data into the
  local session too so you know what you are working with on the cluster
- Don’t update the definitions in the files you `source` while some
  tasks are running or you will get confused about what was loaded

## Other points

Don’t rely on the environment being the same when you submitted your
tasks and when you load them, if you change the environment. That is,
don’t write:

``` r
hipercow_environment_create(packages = "pkg.a")
task_create_expr(use_pkg_a())
hipercow_environment_create(packages = "pkg.b")
task_create_expr(use_pkg_b())
```

If you do this then the second call to `hipercow_environment_create`
**overwrites** the `default` environment, and if the first task has not
started yet, it will load the second environment and not the first!

You can have as many named environments as you want, so we might write
that as:

``` r
hipercow_environment_create(packages = "pkg.a", name = "env_a")
task_create_expr(use_pkg_a(), environment = "env_a")
hipercow_environment_create(packages = "pkg.b", name = "env_a")
task_create_expr(use_pkg_b(), environment = "env_b")
```

## Relationship with provisioning

When you **provision**, you install packages. This defines the set of
packages that are **available**, but none will be loaded by default. You
will be able to load any package that you have installed via
[`library()`](https://rdrr.io/r/base/library.html), or access the
functions by using the namespace operator `::`, but by default only the
normal minimal set of packages will be loaded.

If you specify packages within your provision script but do not load
them, that’s is fine - the packages exist in the library but may not be
used.

If you use packages in an environment that you do not provision, then
your task will fail to start once the package is not found.

When you provision a set of packages, it provisions all the
dependencies. Some people will add `tidyverse` to their
`pkgdepends.txt`, and then use (say) `ggplot2` in their environment
because that is a dependency of `tidyverse`. This is fine.
