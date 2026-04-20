# Build a Tract Distance Matrix for a State

Creates an all-pairs distance matrix between census tract centroids for
a state, using state abbreviation input similar to
[`zip_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/zip_distance_matrix.md).

## Usage

``` r
tract_distance_matrix(
  st,
  county = NULL,
  unit = c("miles", "kilometers", "meters"),
  use_cache = TRUE,
  ...
)
```

## Arguments

- st:

  Character scalar; 2-character USPS state abbreviation (for example,
  `"MD"`).

- county:

  A three-digit FIPS code (string) of the county or counties to subset
  on. This can also be a county name or vector of names.

- unit:

  Character string; one of `"miles"` (default), `"kilometers"`, or
  `"meters"`.

- use_cache:

  Logical; if `TRUE`, enables `options(tigris_use_cache = TRUE)`.

- ...:

  arguments passed on to tigris::tracts

## Value

A list with:

- loc_vec:

  Character vector of tract GEOIDs (same order as matrix dimensions)

- distance_matrix:

  Square numeric matrix of pairwise distances in requested units

## Examples

``` r
# \donttest{
md_dm <- tract_distance_matrix("MD")
dim(md_dm$distance_matrix)
#> [1] 1475 1475
md_dm_km <- tract_distance_matrix("MD", unit = "kilometers")
# }
```
