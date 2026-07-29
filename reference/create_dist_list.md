# Generalized distance list as sparse list

This function is an alternative to the package functions that create a
square distance matrix of dimension N, with all pairwise distances. In
this approach a list of named vectors is returned, where there is one
element in the list for each location, and each named vector holds the
distance within \`threshold\` of the location.

## Usage

``` r
create_dist_list(
  level,
  threshold,
  st = NULL,
  county = NULL,
  unit = c("miles", "kilometers", "meters")
)
```

## Arguments

- level:

  string either "state", "county", "zip", or "tract"

- threshold:

  numeric value; include in each location-specific named vector only
  those locations that a within \`threshold\` distance units of the
  target. Reasonable thresholds might be 250 (miles), 50 (miles), 15
  (miles) and 3 (miles) for county, zip, and tract, respectively, but
  these can be adjusted. Note if a different unit other than miles is
  used, then the user should also adjust this parameter appropriately

- st:

  string; optional to specify a state or vector of states (required for
  tract, and can only be one); if NULL distances are returned for all
  zip codes, counties, or states in the US

- county:

  string vector of 3-fips to restrict within `st`; ignored unless
  `level` is "tract"

- unit:

  string one of miles (default), kilometers, or meters; this is the unit
  relevant to the threshold

## Value

a named list, where each element, named by a target location, is a named
vector of distances that are within \`threshold\` \`units\` of the
target.

## Examples

``` r
md_tracts <- create_dist_list(
  level = "tract",
  threshold = 3,
  st = c("MD")
)
md_va_zips <- create_dist_list(
  level = "zip",
  threshold = 15,
  st = c("MD", "VA")
)
ca_counties <- create_dist_list(
  level = "county",
  threshold = 50,
  st = "CA",
  unit = "kilometers"
)
```
