# Create a sparse distance list from custom location data

This function is a custom-data version of
[`create_dist_list()`](https://lmullany.github.io/gsClusterDetect/reference/create_dist_list.md).
It returns a list of named numeric vectors where each list element
contains only locations within `threshold` distance units of a target
location.

## Usage

``` r
create_custom_dist_list(
  df,
  label_var,
  lat_var,
  long_var,
  threshold,
  unit = c("miles", "kilometers", "meters")
)
```

## Arguments

- df:

  data.frame containing label and coordinate columns

- label_var:

  character scalar; column name used as location label (must be unique
  and non-missing)

- lat_var:

  character scalar; latitude column name.

- long_var:

  character scalar; longitude column name.

- threshold:

  numeric scalar distance cutoff in units of `unit`

- unit:

  string, one of "miles" (default), "kilometers", or "meters"

## Value

a named list, where each element, named by a target location, is a named
vector of distances that are within \`threshold\` \`units\` of the
target.

## Examples

``` r
# \donttest{
md <- tract_generator("MD")
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  39%  |                                                                              |=============================                                         |  42%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |=====================================                                 |  53%  |                                                                              |=============================================                         |  65%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%
dlist <- create_custom_dist_list(
  df = md,
  label_var = "geoid",
  lat_var = "latitude",
  long_var = "longitude",
  threshold = 15,
  unit = "miles"
)
# }
```
