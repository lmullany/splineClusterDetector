# splineClusterDetector

### Description

An R package for implementing spline-based cluster classification/detection of geospatial
clusters from time series of counts, by location. Locations can be expressed as counties,
zip codes, census tracts, or custom definitions. Users provide:

1. a data.frame of counts by location and date
2. a distance object, that contains the distance between location and its neighbors

The package provides functions to create these distance objects in either matrix or list 
format. These can be generated for census tract, zip codes, or counties (fips), or can
be constructed for custom locations by provided a dataframe with columns for latitude and 
longitude (i.e the centroid of each location)

### Installation

Install the `splineClusterDetector` package from CRAN as follows:
```r
install.packages("splineClusterDetector")
```

Install the development version from git as follows:
```r
devtools::install_github("lmullany/splineClusterDetector")
```

### Getting Started:
```r
# load package and the example data
library(splineClusterDetector)
```

### Contacts:
 - Luke Mullany <Luke.Mullany@jhuapl.edu>
 - Howard Burkom <Howard.Burkom@jhuapl.edu>

Copyright 2026 The Johns Hopkins University Applied Physics Laboratory LLC.



