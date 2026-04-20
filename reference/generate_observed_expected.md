# Generate the observed and expected information

Function takes an object of class \`NearbyClusterGrids\`, as returned
from
[`gen_nearby_case_info()`](https://lmullany.github.io/gsClusterDetect/reference/gen_nearby_case_info.md),
and adds observed and expected information.

## Usage

``` r
generate_observed_expected(
  nearby_counts,
  case_grid,
  adjust = FALSE,
  adj_constant = 1
)
```

## Arguments

- nearby_counts:

  an object of class \`NearbyClusterGrids\`

- case_grid:

  an object of class \`CaseGrids\`

- adjust:

  boolean default TRUE, set to `FALSE` to avoid adding one to the
  expected when it is zero. Could result in errors.

- adj_constant:

  numeric (default=1.0); this is the constant to be added if
  `baseline_adjustment == 'add_one'` or
  `baseline_adjustment == 'add_one'`

## Value

a dataframe of class \`ObservedExpectedGrid\`, which is simply a data
frame with

## Examples

``` r
case_grid <- generate_case_grids(
  example_count_data,
  example_count_data[, max(date)]
)
nci <- gen_nearby_case_info(
  cg = case_grid,
  distance_matrix = county_distance_matrix("OH")[["distance_matrix"]],
  distance_limit = 25
)
generate_observed_expected(
  nearby_counts = nci,
  case_grid = case_grid
)
#>             date test_totals target location base_clust_sums distance_value
#>           <Date>       <int> <char>   <char>           <num>          <num>
#>    1: 2025-01-30        9673  39001    39001             251        0.00000
#>    2: 2025-01-30        9673  39003    39003            1263        0.00000
#>    3: 2025-01-30        9673  39005    39005             489        0.00000
#>    4: 2025-01-30        9673  39007    39007             650        0.00000
#>    5: 2025-01-30        9673  39009    39009             560        0.00000
#>   ---                                                                      
#> 2374: 2025-02-03        4256  39063    39173            1545       24.97287
#> 2375: 2025-02-04        2858  39173    39063            4271       24.97287
#> 2376: 2025-02-04        2858  39063    39173            1545       24.97287
#> 2377: 2025-02-05        1447  39173    39063            4271       24.97287
#> 2378: 2025-02-05        1447  39063    39173            1545       24.97287
#>       count count_sum observed detect_date baseline_total  expected log_obs_exp
#>       <int>     <int>    <int>      <Date>          <num>     <num>       <num>
#>    1:     9        39       39  2025-02-05          63803  38.05343  0.02457042
#>    2:    67       335      335  2025-02-05          63803 191.48001  0.55934712
#>    3:    44       166      166  2025-02-05          63803  74.13597  0.80608701
#>    4:     7        69       69  2025-02-05          63803  98.54474 -0.35640415
#>    5:    59       215      215  2025-02-05          63803  84.90008  0.92916296
#>   ---                                                                          
#> 2374:    10        23       67  2025-02-05          63803 103.05973 -0.43061611
#> 2375:     6        12      148  2025-02-05          63803 191.31574 -0.25671287
#> 2376:     6        13       44  2025-02-05          63803  69.20693 -0.45291142
#> 2377:     6         6       70  2025-02-05          63803  96.86280 -0.32480027
#> 2378:     7         7       24  2025-02-05          63803  35.03934 -0.37841760
```
