# Changelog

## gsClusterDetect 1.0.1

CRAN release: 2026-07-27

- Parameter `post_cluster_min_count` removed from
  [`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
  as it was never implemented or used
  ([\#15](https://github.com/lmullany/gsClusterDetect/issues/15))
- Package documentation using `pkgdown` added
  ([\#22](https://github.com/lmullany/gsClusterDetect/issues/22))
- Vignette describing the overall usage of the package added
  ([\#14](https://github.com/lmullany/gsClusterDetect/issues/14))
- Vignette describing the creation of distance objects added
  ([\#28](https://github.com/lmullany/gsClusterDetect/issues/28))
- Vignette describing the use of the `return_interim` parameter in
  [`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
  and the intermediate objects returned
  ([\#32](https://github.com/lmullany/gsClusterDetect/issues/32))
- Function
  [`state_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/state_distance_matrix.md)
  created and `"state"` added as allowed value in
  [`create_dist_list()`](https://lmullany.github.io/gsClusterDetect/reference/create_dist_list.md)
  ([\#30](https://github.com/lmullany/gsClusterDetect/issues/30))
- Output table from
  [`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
  updated with more sensible column names
  ([\#20](https://github.com/lmullany/gsClusterDetect/issues/20))
- Adds basic mapping functionality using new function
  [`map_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/map_clusters.md)
  ([\#17](https://github.com/lmullany/gsClusterDetect/issues/17))

## gsClusterDetect 1.0.0

CRAN release: 2026-03-23

### Initial release

- Initial CRAN submission.
- Provides a workflow for detecting and classifying geo-spatial clusters
  from count time series by location and date.
- Includes end-to-end cluster detection helpers covering case-grid
  generation, nearby-case aggregation, observed-versus-expected
  calculations, and identification of priority clusters using a
  spline-based threshold classification model learned from previous scan
  statistic runs.
- Provides distance utilities for county, ZIP code, census tract,
  U.S.-wide, and custom point-based locations, including sparse
  neighbor-list constructors.
- Includes summary and visualization helpers for baseline and test
  intervals, including summary tables, heat map data and plots, and
  time-series data and plots.
- Includes built-in example data and spline look-up tables for package
  examples and testing.
