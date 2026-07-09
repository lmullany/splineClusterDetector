# Prepare cluster result and shape file for mapping

Function ingest an clusters object as returned by
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md),
a shape file, and a unique identifier for the locations in that shape
file, and prepares these objects for mapping.

## Usage

``` r
prepare_map_data(cl, s, s_id, label_id = NULL, point_crs = NULL)
```

## Arguments

- cl:

  an object of class "clusters" as returned by
  [`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)

- s:

  shape file; must be of class sf

- s_id:

  string unique identifier of \`s\`

- label_id:

  string column of \`s\` that indicates display label for the row in
  \`s\` (default is NULL)

- point_crs:

  optional coordinate reference system used to compute representative
  points when \`s\` is in longitude/latitude coordinates. If \`NULL\`,
  EPSG:3857 is used as a general-purpose fallback. The resulting points
  are transformed back to the CRS of \`s\` before plotting

## Examples

``` r
# example code
# get some data
dd <- example_count_data[, max(date)]
# get a distance matrix
dm <- create_dist_list("county", 50, st = "OH")
# find the clusters
cl <- find_clusters(
  cases = example_count_data,
  detect_date = dd,
  distance_matrix = dm
)
# get shape file
ohio_shape <- tigris::counties("OH", cb = TRUE, class = "sf")
#> Retrieving data for the year 2024

# prepare map data
md <- prepare_map_data(cl, ohio_shape, "GEOID")
md <- prepare_map_data(cl, ohio_shape, "GEOID", label_id = "NAME")
```
