# Set up parallel cluster

Create a parallel cluster for use with degen functions.

## Usage

``` r
setup_cluster(n_cores = NULL, type = c("PSOCK", "FORK"))
```

## Arguments

- n_cores:

  Number of cores to use. Default is `parallel::detectCores() - 1`

- type:

  Cluster type: "PSOCK" (default, works on all platforms) or "FORK"
  (Unix only, more efficient)

## Value

A cluster object, or NULL if parallel is not available

## Examples

``` r
# \donttest{
cl <- setup_cluster(2)  # CRAN policy: max 2 cores
# Use cl with parallel-enabled functions
stop_cluster(cl)
# }
```
