# Fetch bundle logs

Fetch logs from tasks in a bundle.

## Usage

``` r
hipercow_bundle_log_value(bundle, outer = FALSE, follow = TRUE, root = NULL)
```

## Arguments

- bundle:

  Either a `hipercow_bundle` object, or the name of a bundle.

- outer:

  Logical, indicating if we should request the "outer" logs; these are
  logs from the underlying HPC software before it hands off to hipercow.

- follow:

  Logical, indicating if we should follow any retried tasks.

- root:

  A hipercow root, or path to it. If `NULL` we search up your directory
  tree.

## Value

A list with each element being the logs for the corresponding element in
the bundle.

## Examples

``` r
cleanup <- hipercow_example_helper(with_logging = TRUE)
#> ℹ This example uses a special helper
bundle <- task_create_bulk_expr(sqrt(x), data.frame(x = 1:2))
#> ✔ Submitted 2 tasks using 'example'
#> ✔ Created bundle 'curt_millipede' with 2 tasks
hipercow_bundle_wait(bundle)
#> [1] TRUE
hipercow_bundle_log_value(bundle)
#> [[1]]
#>  [1] ""                                                                                
#>  [2] "── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-186c1a6c5d3e' "
#>  [3] "ℹ library paths:"                                                                
#>  [4] "• /home/runner/work/_temp/Library"                                               
#>  [5] "• /opt/R/4.5.2/lib/R/site-library"                                               
#>  [6] "• /opt/R/4.5.2/lib/R/library"                                                    
#>  [7] "ℹ id: 590a4fc492259bc4f6eb35a3175cf0e4"                                          
#>  [8] "ℹ starting at: 2025-11-26 12:53:48.271701"                                       
#>  [9] "ℹ Task type: expression"                                                         
#> [10] "• Expression: sqrt(x)"                                                           
#> [11] "• Locals: x"                                                                     
#> [12] "• Environment: default"                                                          
#> [13] "  USER_KEY:"                                                                     
#> [14] "  /home/runner/work/_temp/hv-20251126-186c1a6c5d3e/hipercow/example/key"         
#> [15] "  USER_PUBKEY:"                                                                  
#> [16] "  /home/runner/work/_temp/hv-20251126-186c1a6c5d3e/hipercow/example/key.pub"     
#> [17] "  R_GC_MEM_GROW: 3"                                                              
#> [18] "───────────────────────────────────────────────────────────────── task logs ↓ ──"
#> [19] ""                                                                                
#> [20] "───────────────────────────────────────────────────────────────── task logs ↑ ──"
#> [21] "✔ status: success"                                                               
#> [22] "ℹ finishing at: 2025-11-26 12:53:48.271701 (elapsed: 0.2216 secs)"               
#> 
#> [[2]]
#>  [1] ""                                                                                
#>  [2] "── hipercow 1.1.8 running at '/home/runner/work/_temp/hv-20251126-186c1a6c5d3e' "
#>  [3] "ℹ library paths:"                                                                
#>  [4] "• /home/runner/work/_temp/Library"                                               
#>  [5] "• /opt/R/4.5.2/lib/R/site-library"                                               
#>  [6] "• /opt/R/4.5.2/lib/R/library"                                                    
#>  [7] "ℹ id: c91a565c41b82741ed7b6621399005b1"                                          
#>  [8] "ℹ starting at: 2025-11-26 12:53:48.689455"                                       
#>  [9] "ℹ Task type: expression"                                                         
#> [10] "• Expression: sqrt(x)"                                                           
#> [11] "• Locals: x"                                                                     
#> [12] "• Environment: default"                                                          
#> [13] "  USER_KEY:"                                                                     
#> [14] "  /home/runner/work/_temp/hv-20251126-186c1a6c5d3e/hipercow/example/key"         
#> [15] "  USER_PUBKEY:"                                                                  
#> [16] "  /home/runner/work/_temp/hv-20251126-186c1a6c5d3e/hipercow/example/key.pub"     
#> [17] "  R_GC_MEM_GROW: 3"                                                              
#> [18] "───────────────────────────────────────────────────────────────── task logs ↓ ──"
#> [19] ""                                                                                
#> [20] "───────────────────────────────────────────────────────────────── task logs ↑ ──"
#> [21] "✔ status: success"                                                               
#> [22] "ℹ finishing at: 2025-11-26 12:53:48.689455 (elapsed: 0.2261 secs)"               
#> 

cleanup()
#> ℹ Cleaning up example
```
