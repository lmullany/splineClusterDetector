# Convert sf to plotly polygons

Helper function that takes the shape file merged with cluster
information as returned by the "map_data" element of
[`prepare_map_data()`](https://lmullany.github.io/gsClusterDetect/reference/prepare_map_data.md)
and converts each location to long polygon coordinate rows suitable for
plotly.

## Usage

``` r
sf_to_plotly_polygons(md)
```

## Arguments

- md:

  list as returned by
  [`prepare_map_data()`](https://lmullany.github.io/gsClusterDetect/reference/prepare_map_data.md)

## Details

This function only prepares drawing geometry. Hover information is
prepared separately in
[`prepare_map_data()`](https://lmullany.github.io/gsClusterDetect/reference/prepare_map_data.md)
as the "hover_data" element.
