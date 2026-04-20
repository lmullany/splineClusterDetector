# Fast version of compress clusters

Function reduces an object of class ClusterAlertTable to the final set
of clusters and locations. The idea of this function is to retain only
the most significant, non-overlapping clusters from the cluster alert
table. The surrogate for significance is 'alertGap', or
log(observed/expected) minus the threshold that the spline assigns to
the observed value\`.

## Usage

``` r
compress_clusters_fast(cluster_alert_table, distance_matrix)
```

## Arguments

- cluster_alert_table:

  an object of class \`ClusterAlertTable\`

- distance_matrix:

  a square distance matrix, named on both dimensions or a list of
  distance vectors, one for each location

## Value

an object of class \`clusters\`, which is simply a a list including a a
data.frame of clusters and another frame of individual location counts

## Examples

``` r
case_grid <- generate_case_grids(
  example_count_data, example_count_data[, max(date)]
)
nci <- gen_nearby_case_info(
  cg = case_grid,
  distance_matrix = county_distance_matrix("OH")[["distance_matrix"]],
  distance_limit = 25
)
obs_exp_grid <- generate_observed_expected(
  nearby_counts = nci,
  case_grid = case_grid
)
cla <- add_spline_threshold(oe_grid = obs_exp_grid)
compress_clusters_fast(
  cluster_alert_table = cla,
  distance_matrix = county_distance_matrix("OH")[["distance_matrix"]]
)
#> $cluster_alert_table
#>     target observed spl_thresh       date test_totals location base_clust_sums
#>     <char>    <int>      <num>     <Date>       <int>   <char>           <num>
#>  1:  39009      215  0.2769749 2025-01-30        9673    39009             560
#>  2:  39005      166  0.3304438 2025-01-30        9673    39005             489
#>  3:  39003      335  0.2113664 2025-01-30        9673    39003            1263
#>  4:  39015       80  0.4995393 2025-02-04        2858    39025             862
#>  5:  39109      399  0.1928263 2025-02-04        2858    39021            5866
#>  6:  39061      287  0.2297259 2025-02-02        5553    39061            2270
#>  7:  39081       37  0.8068677 2025-02-04        2858    39081             326
#>  8:  39141      160  0.3385027 2025-01-31        8267    39129             794
#>  9:  39017      280  0.2330866 2025-01-30        9673    39017            1383
#> 10:  39039       67  0.5758212 2025-02-01        6898    39039             335
#>     distance_value count count_sum detect_date baseline_total  expected
#>              <num> <int>     <int>      <Date>          <num>     <num>
#>  1:        0.00000    59       215  2025-02-05          63803  84.90008
#>  2:        0.00000    44       166  2025-02-05          63803  74.13597
#>  3:        0.00000    67       335  2025-02-05          63803 191.48001
#>  4:       17.32111    26        59  2025-02-05          63803  38.61254
#>  5:       24.96767    11        25  2025-02-05          63803 262.76238
#>  6:        0.00000    79       287  2025-02-05          63803 197.56610
#>  7:        0.00000    20        37  2025-02-05          63803  14.60289
#>  8:       22.46182    10        59  2025-02-05          63803 102.87914
#>  9:        0.00000    46       280  2025-02-05          63803 209.67288
#> 10:        0.00000    14        67  2025-02-05          63803  36.21820
#>     log_obs_exp min_dist alert_ratio  alert_gap    id nr_locs
#>           <num>    <num>       <num>      <num> <int>   <int>
#>  1:   0.9291630  0.00000    3.354683 0.65218806    13       1
#>  2:   0.8060870  0.00000    2.439407 0.47564318     8       1
#>  3:   0.5593471  0.00000    2.646338 0.34798069     1       1
#>  4:   0.7284495 17.32111    1.458243 0.22891024    28       2
#>  5:   0.4177113 24.96767    2.166257 0.22488502    72       5
#>  6:   0.3734090  0.00000    1.625454 0.14368309    54       1
#>  7:   0.9296987  0.00000    1.152232 0.12283098    62       1
#>  8:   0.4416189 22.46182    1.304625 0.10311621   111       3
#>  9:   0.2892410  0.00000    1.240916 0.05615438    33       1
#> 10:   0.6151308  0.00000    1.068267 0.03930963    49       1
#> 
#> $clust_loc_list
#> $clust_loc_list$`13`
#> [1] "39009"
#> 
#> $clust_loc_list$`8`
#> [1] "39005"
#> 
#> $clust_loc_list$`1`
#> [1] "39003"
#> 
#> $clust_loc_list$`28`
#> [1] "39015" "39025"
#> 
#> $clust_loc_list$`72`
#> [1] "39021" "39109" "39037" "39113" "39149"
#> 
#> $clust_loc_list$`54`
#> [1] "39061"
#> 
#> $clust_loc_list$`62`
#> [1] "39081"
#> 
#> $clust_loc_list$`111`
#> [1] "39131" "39129" "39141"
#> 
#> $clust_loc_list$`33`
#> [1] "39017"
#> 
#> $clust_loc_list$`49`
#> [1] "39039"
#> 
#> 
#> attr(,"class")
#> [1] "list"     "clusters"
```
