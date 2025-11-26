# Wait for a task to complete

Wait for a single task to complete (or to start). This function is very
similar to
[task_log_watch](https://mrc-ide.github.io/hipercow/reference/task_log.md),
except that it errors if the task does not complete (so that it can be
used easily to ensure a task has completed) and does not return any
logs.

## Usage

``` r
task_wait(
  id,
  for_start = FALSE,
  timeout = NULL,
  poll = 1,
  progress = NULL,
  follow = TRUE,
  root = NULL
)
```

## Arguments

- id:

  The task identifier

- for_start:

  Logical value, indicating if we only want to wait for the task to
  *start* rather than complete. This will block until the task moves
  away from `submitted`, and will return when it takes the status
  `running` or any terminal status (`success`, `failure`, `cancelled`).
  Note that this does not guarantee that your task will still be running
  by the time `task_wait` exits, your task may have finished by then!

- timeout:

  The time to wait for the task to complete. The default is to wait
  forever.

- poll:

  Time, in seconds, used to throttle calls to the status function. The
  default is 1 second

- progress:

  Logical value, indicating if a progress spinner should be used. The
  default `NULL` uses the option `hipercow.progress`, and if unset
  displays a progress bar in an interactive session.

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

Logical value, `TRUE` if the task completed successfully, `FALSE`
otherwise.

## Details

The progress spinners here come from the cli package and will respond to
cli's options. In particular `cli.progress_clear` and
`cli.progress_show_after`.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper

id <- task_create_expr(sqrt(2))
#> ✔ Submitted task '49fd8472028480257d7e76f6f26b03c9' using 'example'
task_wait(id)
#> [1] TRUE

cleanup()
#> ℹ Cleaning up example
```
