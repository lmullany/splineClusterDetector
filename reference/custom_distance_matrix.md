# Build a Distance Matrix from a Custom Data Frame

Generates an all-pairs distance matrix from latitude/longitude
coordinates in a user-supplied data frame. Row and column names of the
matrix are set from a unique label variable.

## Usage

``` r
custom_distance_matrix(
  df,
  unit = c("miles", "kilometers", "meters"),
  label_var,
  lat_var,
  long_var
)
```

## Arguments

- df:

  A `data.frame` containing label and coordinate columns.

- unit:

  Character string; one of `"miles"` (default), `"kilometers"`, or
  `"meters"`.

- label_var:

  Character scalar; column name to use for matrix row/column names.
  Values in this column must be unique and non-missing.

- lat_var:

  Character scalar; column name containing latitude values.

- long_var:

  Character scalar; column name containing longitude values.

## Value

A list with:

- loc_vec:

  Character vector of location labels (same order as matrix dimensions)

- distance_matrix:

  Square numeric matrix of pairwise distances in requested units

## Examples

``` r
# \donttest{
md <- tract_generator("24")
dm <- custom_distance_matrix(
  md,
  label_var = "geoid", lat_var = "latitude", long_var = "longitude"
)
dim(dm[["distance_matrix"]])
#> [1] 1475 1475

names(md) <- c("tract_id", "lat", "lon")
dm_km <- custom_distance_matrix(
  md,
  unit = "kilometers",
  label_var = "tract_id",
  lat_var = "lat",
  long_var = "lon"
)
# }
```
