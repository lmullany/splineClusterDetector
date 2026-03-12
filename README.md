# gsClusterDetect

### Description

An R package for implementing geospatial cluster identification from time series of
counts, by location. Locations can be expressed as counties, zip codes, census tracts,
or other user-defined geographies. Users provide:

1. a data.frame of counts by location and date
2. a distance object, that contains the distance between location and its neighbors

The package provides functions to create these distance objects in either matrix or list 
format. These can be generated for census tract, zip codes, or counties (fips), or can
be constructed for custom locations by providing a dataframe with columns for latitude and 
longitude (i.e the centroid of each location).

### Installation

Install the `gsClusterDetect` package from CRAN as follows:
```r
install.packages("gsClusterDetect")
```

Install the development version from git as follows:
```r
devtools::install_github("lmullany/gsClusterDetect")
```

### Getting Started:

1. Load the package and provide data frame with `location`, `date`, and `count` columns.
```r
library(gsClusterDetect)
df <- example_count_data
tail(df)

   location       date count
     <char>     <IDat> <int>
1:    39171 2025-02-04     1
2:    39171 2025-02-05     0
3:    39173 2025-02-04     6
4:    39173 2025-02-05     7
5:    39175 2025-02-04     2
6:    39175 2025-02-05     0
```

2. Generate the distance matrix for this location. In this case, the synthetic data has
counts from counties/fips in the state of OHIO, so we use `county_distance_matrix()` and
pass the state abbreviation:

```r
ohio_dm <- county_distance_matrix("OH")

# This is named list of two elements
cat("Class:", class(ohio_dm), "\nNames:", names(ohio_dm))

Class: list 
Names: loc_vec distance_matrix
```

3. Set the end of your target period. This is called the `detect_date`, and is a parameter that must be
passed to the `find_clusters` function. Typically, this might be the current (or last available) date.
```r
detect_date <- max(df[, date])
```

4. Call the `find_clusters()` function; See `?find_clusters()` for full set of options. Note that below, 
we pass the minimum required elements: `cases`, `distance_matrix`, `detect_date`, and set the 
`distance_limit` (the maximum size of the clusters) to 50 (miles).
```r
clusters <- find_clusters(
    cases = df,
    distance_matrix = ohio_dm[["distance_matrix"]],
    detect_date = detect_date,
    distance_limit = 50
)
```

### Contacts:
 - Luke Mullany <Luke.Mullany@jhuapl.edu>
 - Howard Burkom <Howard.Burkom@jhuapl.edu>

Copyright 2026 The Johns Hopkins University Applied Physics Laboratory LLC.



