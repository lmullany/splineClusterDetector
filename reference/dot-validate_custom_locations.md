# Helper function: given a data frame, and strings for label_var, lat_var, and long_var, the df is checked for

Helper function: given a data frame, and strings for label_var, lat_var,
and long_var, the df is checked for

## Usage

``` r
.validate_custom_locations(df, label_var, lat_var, long_var)
```

## Arguments

- df:

  A `data.frame` containing label and coordinate columns.

- label_var:

  Character scalar naming the label column.

- lat_var:

  Character scalar naming the latitude column.

- long_var:

  Character scalar naming the longitude column.

## Value

A `data.table` with standardized columns `location`, `latitude`, and
`longitude`.
