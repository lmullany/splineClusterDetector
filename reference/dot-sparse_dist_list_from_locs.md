# This is a helper function to create a named list of all the locations in `locs` within `threshold_meters` of each loc in `locs`.

This is a helper function to create a named list of all the locations in
`locs` within `threshold_meters` of each loc in `locs`.

## Usage

``` r
.sparse_dist_list_from_locs(locs, threshold_meters, meters_per_unit)
```

## Arguments

- locs:

  A `data.table` with columns `location`, `latitude`, and `longitude`.

- threshold_meters:

  Numeric scalar distance threshold in meters.

- meters_per_unit:

  Numeric scalar conversion factor from output unit to meters.

## Value

Named list of numeric vectors of neighbor distances, keyed by location.
