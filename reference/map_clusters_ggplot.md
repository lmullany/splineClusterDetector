# Render Cluster Map with ggplot2

Internal helper used by
[`map_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/map_clusters.md)
when `engine = "ggplot"`. The function takes prepared map data from
[`prepare_map_data()`](https://lmullany.github.io/gsClusterDetect/reference/prepare_map_data.md)
and returns a static ggplot2 map. Locations are drawn from the merged sf
object, with clustered locations filled according to their cluster
center.

## Usage

``` r
map_clusters_ggplot(
  md,
  label = c("none", "cluster_centers", "cluster_locations", "all")
)
```

## Arguments

- md:

  named list as returned by
  [`prepare_map_data()`](https://lmullany.github.io/gsClusterDetect/reference/prepare_map_data.md).

- label:

  choice for labeling (defaults to "none")

## Value

A ggplot2 object.
