# Get nearby locations

Given a location, a square distance matrix, and numeric value
(radius_miles), this helper function returns a 2-column data frame
listing the locations within that radius

## Usage

``` r
get_nearby_locations(center_location, distance_matrix, radius_miles)
```

## Arguments

- center_location:

  location

- distance_matrix:

  a distance matrix

- radius_miles:

  a numeric value \>0

## Value

a data.table

## Examples

``` r
dm <- zip_distance_matrix("MD")$distance_matrix
nearby_locations <- get_nearby_locations("21228", dm, 10)
```
