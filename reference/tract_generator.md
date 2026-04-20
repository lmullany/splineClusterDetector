# Generate Census Tract Centroids for a State

Pulls census tracts using tigris, computes tract centroids, and returns
a three-column data.table with GEOID, latitude, and longitude.

## Usage

``` r
tract_generator(st, county = NULL, use_cache = TRUE, ...)
```

## Arguments

- st:

  Character scalar; either a 2-digit state FIPS code (for example,
  `"24"`) or a 2-letter USPS abbreviation (for example, `"MD"`).

- county:

  A three-digit FIPS code (string) of the county or counties to subset
  on. This can also be a county name or vector of names.

- use_cache:

  a boolean, defaults to TRUE, to set tigris option to use cache

- ...:

  arguments to be passed on to tigris::tracts()

## Value

A `data.table` with columns:

- geoid:

  11-digit tract GEOID (`state(2) + county(3) + tract(6)`)

- latitude:

  Centroid latitude in WGS84

- longitude:

  Centroid longitude in WGS84

## Examples

``` r
md_tracts <- tract_generator("24")
md_tracts2 <- tract_generator("MD")
howard_county_tracts <- tract_generator("MD", county = "027")
head(md_tracts)
#>          geoid latitude longitude
#>         <char>    <num>     <num>
#> 1: 24005403100 39.35309 -76.73331
#> 2: 24005403201 39.35490 -76.72152
#> 3: 24033807304 39.02498 -76.95943
#> 4: 24033807305 39.01239 -76.96315
#> 5: 24033807405 39.03451 -76.92868
#> 6: 24033807407 39.06685 -76.92374
```
