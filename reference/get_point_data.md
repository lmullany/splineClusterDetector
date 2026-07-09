# Compute representative data points for sf polygons

Computes one point per input feature for use in plotly hover layers. If
the input geometry is longitude/latitude, the point-on-surface operation
is performed in a projected CRS and then transformed back to the
original CRS.

## Usage

``` r
get_point_data(x, point_crs = NULL)
```

## Arguments

- x:

  sf object

- point_crs:

  CRS to use when \`x\` is longitude/latitude. If NULL, EPSG:3857 is
  used as a general-purpose fallback.
