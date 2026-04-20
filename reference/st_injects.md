# Add data counts for parameterized injected clusters

Function st_injects returns a list of two objects 1. a full dataset as a
data.table with inject counts added according to design parameters. 2. a
table of only the inject counts, locations, and dates.

## Usage

``` r
st_injects(
  cases,
  distance_matrix,
  target_loc,
  center_decile,
  radius_miles,
  nr_cases,
  nr_days,
  end_date
)
```

## Arguments

- cases:

  data frame of cases

- distance_matrix:

  a distance matrix

- target_loc:

  a location into which the injection should occur

- center_decile:

  an integer value between 1 and 10, inclusive

- radius_miles:

  a numeric value \>0

- nr_cases:

  number of cases to inject

- nr_days:

  number of days over which we want to inject cases

- end_date:

  last date of injection

## Value

a two-element list; each element is a dataframe. The first is the full
dataset with injected cases and the second is the injected cases only

## Examples

``` r
cases <- example_count_data
dm <- county_distance_matrix("OH")
target_loc <- "39175"
scen1 <- st_injects(
  cases = cases,
  distance_matrix = dm[["distance_matrix"]],
  target_loc = target_loc,
  center_decile = 7,
  radius_miles = 70,
  nr_cases = 100,
  nr_days = 4,
  end_date = "2025-02-05"
)
```
