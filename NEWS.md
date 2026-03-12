# gsClusterDetect 1.0.0

## Initial release

- Initial CRAN submission.
- Provides a workflow for detecting and classifying geospatial clusters from count time series by location and date.
- Includes end-to-end cluster detection helpers covering case-grid generation, nearby-case aggregation, observed-versus-expected calculations, and
identification of priority clusters using a spline-based threshold classification model learned from previous scan statistic runs.
- Provides distance utilities for county, ZIP code, census tract, U.S.-wide, and custom point-based locations, including sparse neighbor-list constructors.
- Includes summary and visualization helpers for baseline and test intervals, including summary tables, heat map data and plots, and time-series data and plots.
- Includes built-in example data and spline lookup tables for package examples and testing.
