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
#> Key: <cluster_center>
#>     cluster_center cluster_start_date cluster_end_date cluster_max_distance
#>             <char>             <Date>           <Date>                <num>
#>  1:          39003         2025-01-30       2025-02-05              0.00000
#>  2:          39005         2025-01-30       2025-02-05              0.00000
#>  3:          39009         2025-01-30       2025-02-05              0.00000
#>  4:          39015         2025-02-04       2025-02-05             17.32111
#>  5:          39017         2025-01-30       2025-02-05              0.00000
#>  6:          39039         2025-02-01       2025-02-05              0.00000
#>  7:          39061         2025-02-02       2025-02-05              0.00000
#>  8:          39081         2025-02-04       2025-02-05              0.00000
#>  9:          39109         2025-02-04       2025-02-05             24.96767
#> 10:          39141         2025-01-31       2025-02-05             22.46182
#>     cluster_center_observed observed  expected log_obs_exp threshold  alert_gap
#>                       <int>    <int>     <num>       <num>     <num>      <num>
#>  1:                     335      335 191.48001   0.5593471 0.2113664 0.34798069
#>  2:                     166      166  74.13597   0.8060870 0.3304438 0.47564318
#>  3:                     215      215  84.90008   0.9291630 0.2769749 0.65218806
#>  4:                      21       80  38.61254   0.7284495 0.4995393 0.22891024
#>  5:                     280      280 209.67288   0.2892410 0.2330866 0.05615438
#>  6:                      67       67  36.21820   0.6151308 0.5758212 0.03930963
#>  7:                     287      287 197.56610   0.3734090 0.2297259 0.14368309
#>  8:                      37       37  14.60289   0.9296987 0.8068677 0.12283098
#>  9:                      36      399 262.76238   0.4177113 0.1928263 0.22488502
#> 10:                      79      160 102.87914   0.4416189 0.3385027 0.10311621
#>     alert_ratio n_cluster_locations
#>           <num>               <int>
#>  1:    2.646338                   1
#>  2:    2.439407                   1
#>  3:    3.354683                   1
#>  4:    1.458243                   2
#>  5:    1.240916                   1
#>  6:    1.068267                   1
#>  7:    1.625454                   1
#>  8:    1.152232                   1
#>  9:    2.166257                   5
#> 10:    1.304625                   3
#> 
#> $cluster_location_counts
#>     location count cluster_center
#>       <char> <int>         <char>
#>  1:    39009   215          39009
#>  2:    39005   166          39005
#>  3:    39003   335          39003
#>  4:    39015    21          39015
#>  5:    39025    59          39015
#>  6:    39021    25          39109
#>  7:    39037    26          39109
#>  8:    39109    36          39109
#>  9:    39113   299          39109
#> 10:    39149    13          39109
#> 11:    39061   287          39061
#> 12:    39081    37          39081
#> 13:    39129    59          39141
#> 14:    39131    22          39141
#> 15:    39141    79          39141
#> 16:    39017   280          39017
#> 17:    39039    67          39039
#> 
#> attr(,"class")
#> [1] "list"     "clusters"
```
