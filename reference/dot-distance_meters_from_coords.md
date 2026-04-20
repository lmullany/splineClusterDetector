# Helper function gets the distance in meter between pairs of coordinates. Note that `coords` must be a matrix or frame, where the first col is longitude and the second column is latitude

Helper function gets the distance in meter between pairs of coordinates.
Note that `coords` must be a matrix or frame, where the first col is
longitude and the second column is latitude

## Usage

``` r
.distance_meters_from_coords(coords)
```

## Arguments

- coords:

  Matrix-like object with longitude in column 1 and latitude in column
  2.

## Value

Numeric square matrix of pairwise distances in meters.
