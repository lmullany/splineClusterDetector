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
#> Key: <target>
#>    target observed spl_thresh       date test_totals location base_clust_sums
#>    <char>    <int>      <num>     <Date>       <int>   <char>           <num>
#> 1:  39003      335  0.2113664 2025-01-30        9673    39003            1263
#> 2:  39005      166  0.3304438 2025-01-30        9673    39005             489
#> 3:  39009      215  0.2769749 2025-01-30        9673    39009             560
#> 4:  39015       80  0.4995393 2025-02-04        2858    39025             862
#> 5:  39017      280  0.2330866 2025-01-30        9673    39017            1383
#> 6:  39039       67  0.5758212 2025-02-01        6898    39039             335
#> 7:  39061      287  0.2297259 2025-02-02        5553    39061            2270
#> 8:  39109      399  0.1928263 2025-02-04        2858    39021            5866
#> 9:  39141      160  0.3385027 2025-01-31        8267    39129             794
#>    distance_value count count_sum detect_date baseline_total  expected
#>             <num> <int>     <int>      <Date>          <num>     <num>
#> 1:        0.00000    67       335  2025-02-05          63803 191.48001
#> 2:        0.00000    44       166  2025-02-05          63803  74.13597
#> 3:        0.00000    59       215  2025-02-05          63803  84.90008
#> 4:       17.32111    26        59  2025-02-05          63803  38.61254
#> 5:        0.00000    46       280  2025-02-05          63803 209.67288
#> 6:        0.00000    14        67  2025-02-05          63803  36.21820
#> 7:        0.00000    79       287  2025-02-05          63803 197.56610
#> 8:       24.96767    11        25  2025-02-05          63803 262.76238
#> 9:       22.46182    10        59  2025-02-05          63803 102.87914
#>    log_obs_exp min_dist alert_ratio  alert_gap    id nr_locs   max_date
#>          <num>    <num>       <num>      <num> <int>   <int>     <IDat>
#> 1:   0.5593471  0.00000    2.646338 0.34798069     2       1 2025-02-05
#> 2:   0.8060870  0.00000    2.439407 0.47564318    10       1 2025-02-05
#> 3:   0.9291630  0.00000    3.354683 0.65218806    15       1 2025-02-05
#> 4:   0.7284495 17.32111    1.458243 0.22891024    46       2 2025-02-05
#> 5:   0.2892410  0.00000    1.240916 0.05615438    59       1 2025-02-05
#> 6:   0.6151308  0.00000    1.068267 0.03930963   123       1 2025-02-05
#> 7:   0.3734090  0.00000    1.625454 0.14368309   141       1 2025-02-05
#> 8:   0.4177113 24.96767    2.166257 0.22488502   190       5 2025-02-05
#> 9:   0.4416189 22.46182    1.304625 0.10311621   259       3 2025-02-05
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
#> 12:    39129    59  39141
#> 13:    39131    22  39141
#> 14:    39141    79  39141
#> 15:    39017   280  39017
#> 16:    39039    67  39039
#> 
#> attr(,"class")
#> [1] "list"     "clusters"
```
