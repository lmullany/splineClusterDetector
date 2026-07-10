# Vignette 3: Accessing Interim Objects from Cluster Estimation

## Overview

The
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
function is the main entry point for this package, and under typical
conditions, the function either returns a message indicating that no
clusters were found, or returns an object of class “clusters”, which is
a two element list containing 1) the clusters found and 2) information
about the locations in each of those clusters. However, callers can pass
`return_interim=True` to obtain the interim or intermediate objects that
are estimated under the hood when
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
is called.

### Functions called by `find_clusters()`

[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
is a wrapper function around a number of other package functions, and it
calls the below functions, in order. When the user passes
`interim_results=TRUE` the outputs of each of these functions are stored
separately within an expanded output list.

[TABLE]

Table: Functions called by
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
{.table style="width:100%;"}

See help via `?<function>` for further details on each of these above
functions, including optional and required parameters and examples of
how to use them.

### Example Usage:

Note: we set `return_interim` to `TRUE`

``` r

result <- find_clusters(
  cases = example_count_data,
  distance_matrix = county_distance_matrix("OH")[["distance_matrix"]],
  detect_date = example_count_data[, max(date)],
  distance_limit = 50,
  return_interim = TRUE
)

# objects included in the output
for (n in names(result)) {
  cat(
    "Name: ",
    n,
    "\nClass: ",
    paste(class(result[[n]]), collapse = " "),
    "\n\n",
    sep = ""
  )
}
#> Name: case_grid_info
#> Class: list CaseGrids
#> 
#> Name: nearby_case_info
#> Class: list NearbyClusterGrids
#> 
#> Name: obs_expected_frame
#> Class: data.table data.frame ObservedExpectedGrid
#> 
#> Name: obs_expected_frame_with_spline
#> Class: data.table data.frame ClusterAlertTable
#> 
#> Name: compressed_clusters
#> Class: list clusters
#> 
#> Name: result
#> Class: list clusters
```
