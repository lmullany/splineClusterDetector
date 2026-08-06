# Vignette 2: Creating Distance Objects

## Overview

When using this package to identify clusters, given a set of
date-location-count data, one of the key elements that must be provided
is the geographical information about the units of clustering.
Specifically, we need to provide to the
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
function an object that contains information about pairwise distances
(See `vignette("basic_demo")` for overview information on finding
clusters).

Here we describe how the package provides utilities to generate these
objects.

## Types of Distance Objects:

There are two types of distance objects that can be passed to the
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
function. These include square pairwise distance matrices and a
constrained version that contains only a limited subset of the
locations.

### Square pairwise distance matrices

If we have a set of locations among which we would like to identify
clusters (say all the counties in Minnesota, or all the zip codes in
Maryland), we can create a matrix containing all the pairwise distances
between the centroids of these locations. The package provides functions
to do this for census tracts, zipcodes, counties, and states.

- [`county_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/county_distance_matrix.md)
- [`zip_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/zip_distance_matrix.md)
- [`tract_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/tract_distance_matrix.md)

Each of these functions must be called with a state level parameter
(`st`), to constrain to a particular state (To combine states, see the
section on “Custom Distance Objects”). There is also a `unit` parameter,
which takes a string: `"miles"` (default), `"kilometers"`, or “`meters"`
to obtains the distance estimates in alternative units.

All of these functions return a two-element named list containing:

- `loc_vec`: a vector containing the names of the `N` locations in the
  geographic target (e.g. “Minnesota”)
- `distance_matrix`: an `N x N` matrix containing the pairwise distances
  between all locations

For all of these functions, locations in the rows and columns of the
`distance_matrix` will have the same ordering as in the `loc_vec`
vector.

#### Example: county distance matrix:

``` r

minnesota_counties <- county_distance_matrix(st = "MN")

# show the length and first 10 values of the `loc_vec`
length(minnesota_counties$loc_vec)
#> [1] 87
minnesota_counties$loc_vec |> head(10)
#>  [1] "27095" "27045" "27073" "27085" "27153" "27105" "27001" "27057" "27063"
#> [10] "27039"

# show the dimension and upper left section of the `distance_matrix`
dim(minnesota_counties$distance_matrix)
#> [1] 87 87
minnesota_counties$distance_matrix[1:5, 1:5]
#>           27095    27045     27073     27085     27153
#> 27095   0.00000 172.7927 138.99740  82.58382  61.58768
#> 27045 172.79267   0.0000 221.40230 133.61217 214.66847
#> 27073 138.99740 221.4023   0.00000  94.00058  96.16435
#> 27085  82.58382 133.6122  94.00058   0.00000  91.24692
#> 27153  61.58768 214.6685  96.16435  91.24692   0.00000
```

#### Example: zip code distance matrix:

``` r

maryland_zips <- zip_distance_matrix(st = "MD")

# show the length and first 10 values of the `loc_vec`
length(maryland_zips$loc_vec)
#> [1] 621
maryland_zips$loc_vec |> head(10)
#>  [1] "00152" "20058" "20207" "20233" "20389" "20395" "20409" "20599" "20601"
#> [10] "20602"

# show the dimension and upper left section of the `distance_matrix`
dim(maryland_zips$distance_matrix)
#> [1] 621 621
maryland_zips$distance_matrix[1:5, 1:5]
#>          00152      20058      20207       20233       20389
#> 00152   0.0000 121.718302 117.987122 105.9661465 106.0287922
#> 20058 121.7183   0.000000   3.896055  16.8089272  16.7956074
#> 20207 117.9871   3.896055   0.000000  12.9178321  12.9064482
#> 20233 105.9661  16.808927  12.917832   0.0000000   0.1337462
#> 20389 106.0288  16.795607  12.906448   0.1337462   0.0000000
```

#### Example: tract distance matrix:

The package also provides a function to generate distance matrices
between census tracts. However, unlike at the zip code and the county
level, there is no built-in data set that contains the centroids of
these tracts across the United States. Therefore, to use the
[`tract_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/tract_distance_matrix.md)
function, the user will need to have the package `tigris` installed.
Also, in addition to the `st` and `unit` parameters, the user can also
restrict the tracts within a state by providing a vector of 3-character
fips codes in the `county` parameter. Furthermore, to prevent the
`tigris` package from using a local cache (using the cache does have the
benefit of speed for repeated calls), the user can turn this cache off
using `use_cache = FALSE`

``` r

cook_county_tracts <- tract_distance_matrix(
  st = "IL",
  county = "031" # the full 5-digit code for Cook County, IL is 17031
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |===========                                                           |  15%  |                                                                              |=============                                                         |  18%  |                                                                              |==================                                                    |  26%  |                                                                              |=============================                                         |  42%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  70%  |                                                                              |===================================================                   |  73%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  79%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=================================================================     |  93%  |                                                                              |===================================================================   |  96%  |                                                                              |======================================================================| 100%

# show the length and first 10 values of the `loc_vec`
length(cook_county_tracts$loc_vec)
#> [1] 1332
cook_county_tracts$loc_vec |> head(10)
#>  [1] "17031030101" "17031030701" "17031070103" "17031807100" "17031807200"
#>  [6] "17031807300" "17031807400" "17031807500" "17031807700" "17031807900"

# show the dimension and upper left section of the `distance_matrix`
dim(cook_county_tracts$distance_matrix)
#> [1] 1332 1332
cook_county_tracts$distance_matrix[1:5, 1:5]
#>             17031030101 17031030701 17031070103 17031807100 17031807200
#> 17031030101   0.0000000   0.6128501    4.496586   5.0124266   4.9426298
#> 17031030701   0.6128501   0.0000000    3.944467   5.3549625   5.2210581
#> 17031070103   4.4965858   3.9444667    0.000000   9.0726214   8.8099097
#> 17031807100   5.0124266   5.3549625    9.072621   0.0000000   0.6311266
#> 17031807200   4.9426298   5.2210581    8.809910   0.6311266   0.0000000
```

As described above, the default unit for these objects is “miles”.
However, for all of these functions, one can also obtain distances in
other units by passing “kilometers” or “meters” to the `unit` parameter.

``` r

maryland_zip <- zip_distance_matrix(st = "MD", unit = "kilometers")
```

### Custom Distance Matrices

Notice that all of the above functions require the user to indicate a
state parameter, `st`, thus restricting the output to a single state. We
also provide a function
[`custom_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/custom_distance_matrix.md)
to allow users to provide any type of geo-spatial unit. The function has
a different set of parameters than the other functions described above:

- `df`: This is a `data.frame` with one row per unit, a unique label,
  and columns containing the latitude and longitude of the centroid of
  each unit
- `unit`: as before, this allows the user to obtain distances in miles,
  kilometers, or meters
- `label_var`: this is the string name of the column containing the
  name/label of the unit
- `lat_var`: the user must provide the name of the column containing the
  latitude of the centroid
- `long_var`: the user must provide the name of the column containing
  the longitude of the centroid

Below, we demonstrate how this function could be used to get a pairwise
distance matrix for a collection of contiguous states, in this case
Maryland, Delaware, and Virginia

``` r

# Use the built-in-counties dataset to get a dataframe of
# counties in the states of interests
states <- c("Delaware", "Maryland", "Virginia")
delmarva_counties <- counties[state_name %in% states]

head(delmarva_counties, 3)
#>    state_name  state state_fips   fips longitude latitude
#>        <char> <char>     <char> <char>     <num>    <num>
#> 1:   Delaware     DE         10  10003 -75.64413 39.57592
#> 2:   Delaware     DE         10  10005 -75.33702 38.67323
#> 3:   Delaware     DE         10  10001 -75.50298 39.09709

# Use the custom function to get the distance matrix
delmarva_dm <- custom_distance_matrix(
  df = delmarva_counties,
  label_var = "fips",
  lat_var = "latitude",
  long_var = "longitude"
)
```

The output structure of the
[`custom_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/custom_distance_matrix.md)
function is the same as the other distance matrix functions.

``` r

# show the length and first 10 values of the `loc_vec`
length(delmarva_dm$loc_vec)
#> [1] 160
delmarva_dm$loc_vec |> head(10)
#>  [1] "10003" "10005" "10001" "24047" "24001" "24510" "24015" "24005" "24013"
#> [10] "24009"

# show the dimension and upper left section of the `distance_matrix`
dim(delmarva_dm$distance_matrix)
#> [1] 160 160
delmarva_dm$distance_matrix[1:5, 1:5]
#>           10003     10005     10001     24047    24001
#> 10003   0.00000  64.50542  33.93277  95.24787 162.8771
#> 10005  64.50542   0.00000  30.61596  31.20211 191.6783
#> 10001  33.93277  30.61596   0.00000  61.34431 174.6244
#> 24047  95.24787  31.20211  61.34431   0.00000 206.1344
#> 24001 162.87708 191.67834 174.62439 206.13442   0.0000
```

### Distance Lists

Notice that the square matrices in all the above examples are of size
`N x N` where `N` is the number of unique locations in your target
geography. These matrices can become very large, and as they become
large, they also become slower to calculate. Furthermore, many of the
pairs are never really used in subsequent analyses, because the distance
between them is larger than any typical radius constraint that a user
might want to place on their cluster-finding technique (Again, see
`vignette("basic_demo")` for more information on using the
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
function and setting the maximum distance to consider when constructing
clusters).

For example, we typically recommend starting at 50 miles as the radius
for county-level cluster finding, 15 miles for zip-level cluster
finding, and 3 miles for tract-level cluster finding. Whatever the
radius used, say `r`, in the square distance matrices above, none of the
pairs where the distance between centroids exceeds `r` are needed for
the calculation.

Therefore, we also a provide a way to construct distance objects for
[`find_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/find_clusters.md)
that only includes pairs where the centroid-to-centroid distance is
within some threshold. These objects can be returned using a function
`create_distance_list()` which has the following parameters:

- `level`: this is the geographic level, one of “tract”, “zip”,
  “county”, or “state”
- `threshold`: this is the threshold distance, expressed in units given
  by the `unit` argument, to constrain paired locations; for each
  location `x`, return a vector with only those locations `y1`, `y2`,
  `y3`, `...` where distance between `x_i` and `y_i` \< `threshold`
- `st`: this is a two character state abbreviation, which is required
  for `level="tract"`, but is otherwise optional. In the latter case
  (i.e. when `st` is not specified), the function will return distances
  of pairs within `threshold` for all zips or counties in the entire
  United States
- `county`: like the
  [`tract_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/tract_distance_matrix.md)
  function, the user can restrict the estimation of tract-to-tract
  distances to a vector of 3-character fips codes
- `unit`: like other functions discussed above, the distance unit can be
  returned in `miles` (default), kilometers, or meters. The function
  assumes that `threshold` is always given in terms of `unit` (i.e. if
  `unit` is set to “kilometers”, for example, be sure to express
  `threshold` in kilometers).

The output structure of this function is a list of vectors. The list is
of length `N` where `N` is the number of unique locations in the target
geography. Each element of the list is a named vector of distances to
the location(s) (i.e. the `y_i`) that are within `threshold` units of
the current location. An example will help illustrate:

``` r

maryland_zip_list <- create_dist_list(
  level = "zip",
  # using a small distance for zip code clustering for demo purposes
  threshold = 7,
  st = "MD"
)

# this returns a list
class(maryland_zip_list)
#> [1] "list"

# the list is of length equal to the number of unique zip codes.
# Recall from above that we produced a 621 x 621 matrix; in this
# case, our list is of length 621
length(maryland_zip_list)
#> [1] 621

# the names of the list are the locations
names(maryland_zip_list) |> head(10)
#>  [1] "00152" "20058" "20207" "20233" "20389" "20395" "20409" "20599" "20601"
#> [10] "20602"

# each element of the list is a named vector with distances to
# those locations within threshold units
maryland_zip_list |> tail(3)
#> $`21921`
#>    21921    21916    21922    21920    21901 
#> 0.000000 1.415658 2.044843 2.762370 6.917458 
#> 
#> $`21922`
#>    21922    21921    21916    21920 
#> 0.000000 2.044843 3.443627 4.097526 
#> 
#> $`21930`
#>    21930    21913    21635    21919    21912    21645 
#> 0.000000 2.691130 3.465579 4.221354 5.411272 5.925401
```

This approach is MUCH faster than the `<geolevel>_distance_matrix()`
functions described above, when the number of locations is large, but
only a few are used. A good example of this is obtaining the zip code
level distance information for Texas.

``` r
system.time(texas_zip_dm <- zip_distance_matrix(st="TX"))

user  system elapsed 
4.88    0.08    4.97 
   
system.time(texas_zip_dl <- create_dist_list(level="zip", threshold=15, st="TX"))

user  system elapsed 
0.76    0.03    0.80 
```

### Custom Distance Lists

As above, we provide a way to generate distance lists for custom
location units. In this case, we use the function
`create_custom_distance_list()`. Again, we can use the example of
creating a distance list for the joint Maryland, Delaware, and Virginia
region, but any user-defined data set can be used for this. The
requirements are basically the same as the
[`custom_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/custom_distance_matrix.md)
function, except that we additionally require a `threshold`.

Another advantage of custom lists requiring thresholds is that if a user
specifies non-contiguous states covering possibly large distances,
distance lists limited by a reasonable threshold will avoid calculation
of many long distances between faraway locations that would be omitted
from clusters by the maximum cluster radius anyway.

``` r

# As before, we use the built-in-counties dataset to get a dataframe of counties
states <- c("Delaware", "Maryland", "Virginia")
delmarva_counties <- counties[state_name %in% states]

head(delmarva_counties, 3)
#>    state_name  state state_fips   fips longitude latitude
#>        <char> <char>     <char> <char>     <num>    <num>
#> 1:   Delaware     DE         10  10003 -75.64413 39.57592
#> 2:   Delaware     DE         10  10005 -75.33702 38.67323
#> 3:   Delaware     DE         10  10001 -75.50298 39.09709

# Use the custom function to get the distance list
delmarva_dl <- create_custom_dist_list(
  df = delmarva_counties,
  label_var = "fips",
  lat_var = "latitude",
  long_var = "longitude",
  threshold = 50
)

# this is a list
class(delmarva_dl)
#> [1] "list"

# with length equal to all the counties in the Delmarva region
length(delmarva_dl)
#> [1] 160

# first three elements (i.e. locations) in this list
delmarva_dl[1:3]
#> $`10003`
#>    10003    24015    10001    24029    24025    24035    24011 
#>  0.00000 15.87035 33.93277 34.58720 35.02830 43.77808 49.69264 
#> 
#> $`10005`
#>    10005    24045    24011    10001    24047    24019    24041    24035 
#>  0.00000 26.47662 29.96149 30.61596 31.20211 41.92635 45.66223 47.46551 
#>    24039 
#> 49.93719 
#> 
#> $`10001`
#>    10001    24011    10005    24035    10003    24029    24015    24041 
#>  0.00000 23.54765 30.61596 31.32545 33.93277 34.82778 39.78547 43.57806
```

### Other Functions

1.  We also provide
    [`us_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/us_distance_matrix.md)
    which is a similar to the the other `<resolution>_distance_matrix()`
    functions except that it only takes the `unit` parameter. It returns
    a county level distance matrix (and vector) for all the counties in
    the US. In general, unless all pairwise distances are truly desired,
    we strongly recommend that users desiring national-level distance
    information pre-determine a threshold and use the
    [`create_dist_list()`](https://lmullany.github.io/gsClusterDetect/reference/create_dist_list.md)
    function leaving the `st` parameter as `NULL`, as that approach will
    be substantially faster.

2.  We provide a function
    [`state_distance_matrix()`](https://lmullany.github.io/gsClusterDetect/reference/state_distance_matrix.md)
    which acts identically to the other `<location>_distance_matrix()`
    functions, except that it doesn’t take `st` parameter. This returns
    a distance matrix for all states in the US, and is included for
    completeness and convenience, even if its practical usage is
    limited.
