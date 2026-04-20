# Helper function reduces a data frame to only those rows where latitude and longitude are not missing

Helper function reduces a data frame to only those rows where latitude
and longitude are not missing

## Usage

``` r
.numeric_location_coords(locs)
```

## Arguments

- locs:

  A `data.table` with `latitude` and `longitude` columns.

## Value

`data.table` filtered to non-missing latitude rows with numeric
latitude/longitude columns.
