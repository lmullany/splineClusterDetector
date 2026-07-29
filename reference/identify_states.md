# Identify states from location frame

Helper function to identify states represented in a location frame; this
is helpful because the location functions are faster and more efficient
when states are specified.

## Usage

``` r
identify_states(df, level = c("zip", "county"), location = "location")
```

## Arguments

- df:

  frame/data.table that contains a column of locations

- level:

  string either "zip" (default) or "county"

- location:

  string column identifying the location (default is "location")

## Value

vector of state abbreviations

## Examples

``` r
identify_states(example_count_data, level = "zip")
identify_states(example_count_data, level = "county")
#> [1] "OH"
```
