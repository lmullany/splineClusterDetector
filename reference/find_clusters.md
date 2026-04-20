# Find clusters

Function will return clusters, given a frame of case counts by location
and date, a distance matrix, a spline lookup table, and other parameters

## Usage

``` r
find_clusters(
  cases,
  distance_matrix,
  detect_date,
  spline_lookup = NULL,
  baseline_length = 90,
  max_test_window_days = 7,
  guard_band = 0,
  distance_limit = 15,
  baseline_adjustment = c("add_one", "add_one_global", "add_test", "none"),
  adj_constant = 1,
  min_clust_cases = 0,
  max_clust_cases = Inf,
  use_fast = TRUE,
  return_interim = FALSE
)
```

## Arguments

- cases:

  a frame of case counts by location and date

- distance_matrix:

  a square distance matrix, named on both dimensions or a list of
  distance vectors, one for each location

- detect_date:

  a date that indicates the end of the test window in which we are
  looking for clusters

- spline_lookup:

  default NULL; either a spline lookup table, which is a data frame that
  has at least two columns: including "observed" and "spl_thresh", OR a
  string indicating to use one of the built in lookup tables: i.e. one
  of `"001", "005", "01", "05"`. If NULL, the default table will be 01
  (i.e. `spline_01` dataset)

- baseline_length:

  integer (default = 90) number of days in the baseline interval

- max_test_window_days:

  integer (default = 7) number of days for the test window

- guard_band:

  integer (default = 0) buffer days between baseline and test interval

- distance_limit:

  numeric (default=15) maximum distance to consider cluster size. Note
  that the units of the value default (miles) should be the same unit as
  the values in the distance matrix

- baseline_adjustment:

  one of four string options: "add_one" (default), "add_one_global",
  "add_test", or "none". All methods except for "none" will ensure that
  the log(obs/expected) is always defined (i.e. avoids expected =0). For
  the default, this will add 1 to the expected for any individual
  calculation if expected would otherwise be zero. "add_one_global",
  will add one to all baseline location case counts. For
  "add_test_interval", each location in the baseline is increased by the
  number of cases in that location during the test interval. If "none",
  no adjustment is made.

- adj_constant:

  numeric (default=1.0); this is the constant to be added if
  `baseline_adjustment == 'add_one'` or
  `baseline_adjustment == 'add_one'`

- min_clust_cases:

  (default = 0); minimum number of case within a returned cluster.

- max_clust_cases:

  (default = Inf); maximum number of cases within a returned cluster.

- use_fast:

  boolean (default = TRUE) - set to TRUE to use the fast version of the
  compress clusters function

- return_interim:

  boolean (default = FALSE) - set to TRUE to return all interim objects
  of the `find_clusters()` function

## Value

returns a list of two of two dataframes.

## Examples

``` r
find_clusters(
  cases = example_count_data,
  distance_matrix = county_distance_matrix("OH")[["distance_matrix"]],
  detect_date = example_count_data[, max(date)],
  distance_limit = 50
)
#> $cluster_alert_table
#> Key: <target>
#>     target observed spl_thresh       date test_totals location base_clust_sums
#>     <char>    <int>      <num>     <Date>       <int>   <char>           <num>
#>  1:  39003      335  0.2113664 2025-01-30        9673    39003            1263
#>  2:  39005      166  0.3304438 2025-01-30        9673    39005             489
#>  3:  39009      215  0.2769749 2025-01-30        9673    39009             560
#>  4:  39015       80  0.4995393 2025-02-04        2858    39025             862
#>  5:  39017      280  0.2330866 2025-01-30        9673    39017            1383
#>  6:  39039       67  0.5758212 2025-02-01        6898    39039             335
#>  7:  39061      287  0.2297259 2025-02-02        5553    39061            2270
#>  8:  39081       37  0.8068677 2025-02-04        2858    39081             326
#>  9:  39109      399  0.1928263 2025-02-04        2858    39021            5866
#> 10:  39141      160  0.3385027 2025-01-31        8267    39129             794
#>     distance_value count count_sum detect_date baseline_total  expected
#>              <num> <int>     <int>      <Date>          <num>     <num>
#>  1:        0.00000    67       335  2025-02-05          63803 191.48001
#>  2:        0.00000    44       166  2025-02-05          63803  74.13597
#>  3:        0.00000    59       215  2025-02-05          63803  84.90008
#>  4:       17.32111    26        59  2025-02-05          63803  38.61254
#>  5:        0.00000    46       280  2025-02-05          63803 209.67288
#>  6:        0.00000    14        67  2025-02-05          63803  36.21820
#>  7:        0.00000    79       287  2025-02-05          63803 197.56610
#>  8:        0.00000    20        37  2025-02-05          63803  14.60289
#>  9:       24.96767    11        25  2025-02-05          63803 262.76238
#> 10:       22.46182    10        59  2025-02-05          63803 102.87914
#>     log_obs_exp min_dist alert_ratio  alert_gap    id nr_locs   max_date
#>           <num>    <num>       <num>      <num> <int>   <int>     <IDat>
#>  1:   0.5593471  0.00000    2.646338 0.34798069     2       1 2025-02-05
#>  2:   0.8060870  0.00000    2.439407 0.47564318    10       1 2025-02-05
#>  3:   0.9291630  0.00000    3.354683 0.65218806    15       1 2025-02-05
#>  4:   0.7284495 17.32111    1.458243 0.22891024    46       2 2025-02-05
#>  5:   0.2892410  0.00000    1.240916 0.05615438    59       1 2025-02-05
#>  6:   0.6151308  0.00000    1.068267 0.03930963   123       1 2025-02-05
#>  7:   0.3734090  0.00000    1.625454 0.14368309   141       1 2025-02-05
#>  8:   0.9296987  0.00000    1.152232 0.12283098   163       1 2025-02-05
#>  9:   0.4177113 24.96767    2.166257 0.22488502   190       5 2025-02-05
#> 10:   0.4416189 22.46182    1.304625 0.10311621   259       3 2025-02-05
#> 
#> $cluster_location_counts
#>     location count target
#>       <char> <int> <char>
#>  1:    39009   215  39009
#>  2:    39005   166  39005
#>  3:    39003   335  39003
#>  4:    39015    21  39015
#>  5:    39025    59  39015
#>  6:    39021    25  39109
#>  7:    39037    26  39109
#>  8:    39109    36  39109
#>  9:    39113   299  39109
#> 10:    39149    13  39109
#> 11:    39061   287  39061
#> 12:    39081    37  39081
#> 13:    39129    59  39141
#> 14:    39131    22  39141
#> 15:    39141    79  39141
#> 16:    39017   280  39017
#> 17:    39039    67  39039
#> 
#> attr(,"class")
#> [1] "list"     "clusters"
```
