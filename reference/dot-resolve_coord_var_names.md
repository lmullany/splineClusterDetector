# Helper function resolves coordinate variable names

Helper function resolves coordinate variable names

## Usage

``` r
.resolve_coord_var_names(lat_var = NULL, long_var = NULL)
```

## Arguments

- lat_var:

  Character scalar latitude column name or `NULL` to use `"latitude"`.

- long_var:

  Character scalar longitude column name or `NULL` to use `"longitude"`.

## Value

A named list with elements `lat_var` and `long_var`.
