# Helper function takes a vector of locations, and a set of coords (which must be a matrix or frame with first two columns being longitude and latitude), and returns a square distance matrix for all pairs of coordinates in a given unit

Helper function takes a vector of locations, and a set of coords (which
must be a matrix or frame with first two columns being longitude and
latitude), and returns a square distance matrix for all pairs of
coordinates in a given unit

## Usage

``` r
.distance_result_from_coords(
  loc_vec,
  coords,
  unit = c("miles", "kilometers", "meters")
)
```

## Arguments

- loc_vec:

  Character vector of location identifiers used as matrix row/column
  names.

- coords:

  Matrix-like object with longitude in column 1 and latitude in column
  2.

- unit:

  Character scalar unit for returned distances; one of `"miles"`,
  `"kilometers"`, or `"meters"`.

## Value

A list with elements `loc_vec` and `distance_matrix`.
