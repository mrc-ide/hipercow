# Get task log

Get the task log, if the task has produced one. Tasks run by the
`dide-windows` driver will generally produce a log. A log might be quite
long, and you might want to print it to screen in its entirety
(`task_log_show`), or return it as character vector (`task_log_value`).

## Usage

``` r
task_log_show(id, outer = FALSE, follow = TRUE, root = NULL)

task_log_value(id, outer = FALSE, follow = TRUE, root = NULL)

task_log_watch(
  id,
  poll = 1,
  skip = 0,
  timeout = NULL,
  progress = NULL,
  follow = TRUE,
  root = NULL
)
```

## Arguments

- id:

  The task identifier

- outer:

  Logical, indicating if we should request the "outer" logs; these are
  logs from the underlying HPC software before it hands off to hipercow.

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

- poll:

  Time, in seconds, used to throttle calls to the status function. The
  default is 1 second

- skip:

  Optional integer indicating how to handle log content that exists at
  the point where we start watching. The default (0) shows all log
  contents. A positive integer skips that many lines, while a negative
  integer shows only that many lines (so -5 shows the first five lines
  in the log). You can pass `Inf` to discard all previous logs, but
  stream all new ones.

- timeout:

  The time to wait for the task to complete. The default is to wait
  forever.

- progress:

  Logical value, indicating if a progress spinner should be used. The
  default `NULL` uses the option `hipercow.progress`, and if unset
  displays a progress bar in an interactive session.

## Value

Depending on the function:

- `task_log_show` returns the log value contents invisibly, but
  primarily displays the log contents on the console as a side effect

- `task_log_value` returns a character of log contents

- `task_log_watch` returns the status converted to logical (as for
  [task_wait](https://mrc-ide.github.io/hipercow/reference/task_wait.md))

## Details

The function `task_log_watch` has similar semantics to
[task_wait](https://mrc-ide.github.io/hipercow/reference/task_wait.md)
but does not error on timeout, and always displays a log.

## Examples

``` r
cleanup <- hipercow_example_helper(with_logging = TRUE)
#> ℹ This example uses a special helper

# Tasks that don't produce any output (print, cat, warning, etc)
# will only contain logging information from hipercow itself
id <- task_create_expr(runif(1))
#> ✔ Submitted task 'ab89467248f3f1861f6afaeddfcf7999' using 'example'
task_wait(id)
#> [1] TRUE
task_log_show(id)
#> 
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-186c3bfb7034' 
#> ℹ library paths:
#> • /home/runner/work/_temp/Library
#> • /opt/R/4.5.2/lib/R/site-library
#> • /opt/R/4.5.2/lib/R/library
#> ℹ id: ab89467248f3f1861f6afaeddfcf7999
#> ℹ starting at: 2025-11-26 12:56:27.564487
#> ℹ Task type: expression
#> • Expression: runif(1)
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-186c3bfb7034/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-186c3bfb7034/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ───────────────────────────────────────────────────────────────── task logs ↓ ──
#> 
#> ───────────────────────────────────────────────────────────────── task logs ↑ ──
#> ✔ status: success
#> ℹ finishing at: 2025-11-26 12:56:27.564487 (elapsed: 0.2204 secs)

# If your task creates output then it will appear within the
# horizontal rules:
id <- task_create_explicit(quote({
  message("Starting analysis")
  x <- mean(runif(100))
  message("all done!")
  x
}))
#> ✔ Submitted task '48f7b05563f6604122e3badd45f98f73' using 'example'
task_wait(id)
#> [1] TRUE
task_log_show(id)
#> 
#> ── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-186c3bfb7034' 
#> ℹ library paths:
#> • /home/runner/work/_temp/Library
#> • /opt/R/4.5.2/lib/R/site-library
#> • /opt/R/4.5.2/lib/R/library
#> ℹ id: 48f7b05563f6604122e3badd45f98f73
#> ℹ starting at: 2025-11-26 12:56:28.484576
#> ℹ Task type: explicit
#> • Expression: { [...]
#> • Locals: (none)
#> • Environment: default
#>   USER_KEY:
#>   /home/runner/work/_temp/hv-20251126-186c3bfb7034/hipercow/example/key
#>   USER_PUBKEY:
#>   /home/runner/work/_temp/hv-20251126-186c3bfb7034/hipercow/example/key.pub
#>   R_GC_MEM_GROW: 3
#> ───────────────────────────────────────────────────────────────── task logs ↓ ──
#> Starting analysis
#> all done!
#> 
#> ───────────────────────────────────────────────────────────────── task logs ↑ ──
#> ✔ status: success
#> ℹ finishing at: 2025-11-26 12:56:28.484576 (elapsed: 0.2324 secs)

# Use "task_log_value" to get the log value as a character vector
task_log_value(id)
#>  [1] ""                                                                                
#>  [2] "── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-186c3bfb7034' "
#>  [3] "ℹ library paths:"                                                                
#>  [4] "• /home/runner/work/_temp/Library"                                               
#>  [5] "• /opt/R/4.5.2/lib/R/site-library"                                               
#>  [6] "• /opt/R/4.5.2/lib/R/library"                                                    
#>  [7] "ℹ id: 48f7b05563f6604122e3badd45f98f73"                                          
#>  [8] "ℹ starting at: 2025-11-26 12:56:28.484576"                                       
#>  [9] "ℹ Task type: explicit"                                                           
#> [10] "• Expression: { [...]"                                                           
#> [11] "• Locals: (none)"                                                                
#> [12] "• Environment: default"                                                          
#> [13] "  USER_KEY:"                                                                     
#> [14] "  /home/runner/work/_temp/hv-20251126-186c3bfb7034/hipercow/example/key"         
#> [15] "  USER_PUBKEY:"                                                                  
#> [16] "  /home/runner/work/_temp/hv-20251126-186c3bfb7034/hipercow/example/key.pub"     
#> [17] "  R_GC_MEM_GROW: 3"                                                              
#> [18] "───────────────────────────────────────────────────────────────── task logs ↓ ──"
#> [19] "Starting analysis"                                                               
#> [20] "all done!"                                                                       
#> [21] ""                                                                                
#> [22] "───────────────────────────────────────────────────────────────── task logs ↑ ──"
#> [23] "✔ status: success"                                                               
#> [24] "ℹ finishing at: 2025-11-26 12:56:28.484576 (elapsed: 0.2324 secs)"               

# Depending on the driver you are using, there may be useful
# information in the "outer" log; the logs produced by the
# submission system before hipercow takes over:
task_log_show(id, outer = TRUE)
#> Running task 48f7b05563f6604122e3badd45f98f73
#> Finished task 48f7b05563f6604122e3badd45f98f73

cleanup()
#> ℹ Cleaning up example
```
