# Map Clusters

Utility for mapping clusters. The function ingest an object \`cl\` as
returned by
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
and a shape file provisioned by the caller, along with a string name of
a column in the shape file that uniquely defines the locations. Note
that this column will be used to merge with the clusters and therefore
must aligned with the labels in \`cl\` cluster locations. The function
returns a basic plotly map object that can be further modified by the
user

## Usage

``` r
map_clusters(
  cl,
  s,
  s_id,
  label_id = NULL,
  label = c("none", "cluster_centers", "cluster_locations", "all"),
  engine = c("plotly", "ggplot"),
  point_crs = NULL
)
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

- label:

  for `engine = "ggplot"`, indicates which locations should receive
  visible text labels. Valid choices are `"none"`, `"cluster_centers"`,
  `"cluster_locations"`, and `"all"`. The default is `"none"`. This
  argument is ignored when `engine = "plotly"` because plotly maps
  always include hover labels for all locations.

- engine:

  string label to indicate plotting engine; either "plotly" (default) or
  "ggplot"

- point_crs:

  optional coordinate reference system used to compute representative
  points when \`s\` is in longitude/latitude coordinates. If \`NULL\`,
  EPSG:3857 is used as a general-purpose fallback. The resulting points
  are transformed back to the CRS of \`s\` before plotting

## Examples

``` r
if (
  requireNamespace("tigris", quietly = TRUE) &&
    requireNamespace("ggplot2", quietly = TRUE)
) {
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

  # prepare map data
  md <- map_clusters(cl, ohio_shape, "GEOID")
}
#> Retrieving data for the year 2024
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  13%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |========================                                              |  34%  |                                                                              |==========================                                            |  37%  |                                                                              |============================                                          |  40%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  46%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=========================================                             |  59%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  79%  |                                                                              |=========================================================             |  82%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
```
