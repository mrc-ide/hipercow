# Describe cluster

Describe information about the cluster. This is (naturally) very
dependent on the cluster but some details of the value are reliable; see
Value for details.

## Usage

``` r
hipercow_cluster_info(driver = NULL, root = NULL)
```

## Arguments

- driver:

  The driver to use, which determines the cluster to fetch information
  from (depending on your configuration). If no driver is configured, an
  error will be thrown.

- root:

  Hipercow root, usually best `NULL`

## Value

A list describing the cluster. The details depend on the driver, and are
subject to change. We expect to see elements:

- resources: Describes the computational resources on the cluster, which
  is used by
  [hipercow_resources_validate](https://mrc-ide.github.io/hipercow/reference/hipercow_resources_validate.md).
  Currently this is a simple list with elements `max_ram` (max RAM
  available, in GB), `max_cores` (max number of cores you can request),
  `queues` (character vector of available queues), `nodes` (character
  vector of available nodes), `default_queue` (the default queue). These
  details are subject to change but the contents should always be
  informative and fairly self explanatory.

- redis_url: The URL of the redis server to communicate with from
  outside of the cluster (i.e., from your computer), in a form suitable
  for use with redux::hiredis

- r_versions: A vector of R versions, as `numeric_vector` objects

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
hipercow_cluster_info()
#> $resources
#> $resources$name
#> [1] "example"
#> 
#> $resources$node_os
#> [1] "example_os"
#> 
#> $resources$max_ram
#> [1] 4
#> 
#> $resources$max_cores
#> [1] 4
#> 
#> $resources$queues
#> [1] "alltasks" "bigmem"   "fast"    
#> 
#> $resources$build_queue
#> [1] "fast"
#> 
#> $resources$nodes
#> [1] "node-1" "node-2" "gpu-3"  "gpu-4" 
#> 
#> $resources$default_queue
#> [1] "alltasks"
#> 
#> $resources$redis_url
#> [1] "127.0.0.1:6379"
#> 
#> 
#> $r_versions
#> [1] ‘4.5.2’
#> 
#> $redis_url
#> [1] "127.0.0.1:6379"
#> 
cleanup()
#> ℹ Cleaning up example
```
