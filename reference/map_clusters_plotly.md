# Render Cluster Map with plotly

Internal helper used by
[`map_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/map_clusters.md)
when `engine = "plotly"`. The function takes prepared map data from
[`prepare_map_data()`](https://lmullany.github.io/gsClusterDetect/reference/prepare_map_data.md)
and returns an interactive plotly map. Polygon outlines and cluster
fills are drawn as scatter traces, while hover labels are provided by a
separate invisible point layer generated from the representative point
data.

## Usage

``` r
map_clusters_plotly(md)
```

## Arguments

- md:

  named list as returned by
  [`prepare_map_data()`](https://lmullany.github.io/gsClusterDetect/reference/prepare_map_data.md).

## Value

A plotly htmlwidget object.
