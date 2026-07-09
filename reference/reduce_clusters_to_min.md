# Filter clusters on minimum overall count

Function takes a set of clusters identified via
[`compress_clusters()`](https://lmullany.github.io/gsClusterDetect/reference/compress_clusters.md)
and a minimum threshold for counts, and reduces the identified clusters
to only those clusters where the total number of observed across the
cluster meets that minimum threshold.

## Usage

``` r
reduce_clusters_to_min(cl, minimum = 0)
```

## Arguments

- cl:

  a object of class `clusters`, as returned from `compress_clusters`

- minimum:

  numeric (default = 0); minimum number across all locations in a
  cluster in order to retain

## Value

an object of class `clusters`

## Examples

``` r
cl <- find_clusters(
  cases = example_count_data,
  distance_matrix = county_distance_matrix("OH")[["distance_matrix"]],
  detect_date = example_count_data[, max(date)],
  distance_limit = 50
)
reduce_clusters_to_min(cl, 50)
#> $cluster_alert_table
#> Key: <cluster_center>
#>    cluster_center cluster_start_date cluster_end_date cluster_max_distance
#>            <char>             <Date>           <Date>                <num>
#> 1:          39003         2025-01-30       2025-02-05              0.00000
#> 2:          39005         2025-01-30       2025-02-05              0.00000
#> 3:          39009         2025-01-30       2025-02-05              0.00000
#> 4:          39015         2025-02-04       2025-02-05             17.32111
#> 5:          39017         2025-01-30       2025-02-05              0.00000
#> 6:          39039         2025-02-01       2025-02-05              0.00000
#> 7:          39061         2025-02-02       2025-02-05              0.00000
#> 8:          39109         2025-02-04       2025-02-05             24.96767
#> 9:          39141         2025-01-31       2025-02-05             22.46182
#>    cluster_center_observed observed  expected log_obs_exp threshold  alert_gap
#>                      <int>    <int>     <num>       <num>     <num>      <num>
#> 1:                     335      335 191.48001   0.5593471 0.2113664 0.34798069
#> 2:                     166      166  74.13597   0.8060870 0.3304438 0.47564318
#> 3:                     215      215  84.90008   0.9291630 0.2769749 0.65218806
#> 4:                      21       80  38.61254   0.7284495 0.4995393 0.22891024
#> 5:                     280      280 209.67288   0.2892410 0.2330866 0.05615438
#> 6:                      67       67  36.21820   0.6151308 0.5758212 0.03930963
#> 7:                     287      287 197.56610   0.3734090 0.2297259 0.14368309
#> 8:                      36      399 262.76238   0.4177113 0.1928263 0.22488502
#> 9:                      79      160 102.87914   0.4416189 0.3385027 0.10311621
#>    alert_ratio n_cluster_locations
#>          <num>               <int>
#> 1:    2.646338                   1
#> 2:    2.439407                   1
#> 3:    3.354683                   1
#> 4:    1.458243                   2
#> 5:    1.240916                   1
#> 6:    1.068267                   1
#> 7:    1.625454                   1
#> 8:    2.166257                   5
#> 9:    1.304625                   3
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
#> 12:    39129    59          39141
#> 13:    39131    22          39141
#> 14:    39141    79          39141
#> 15:    39017   280          39017
#> 16:    39039    67          39039
#> 
#> attr(,"class")
#> [1] "list"     "clusters"
```
