# Vignette 4: Artificial Cluster Injection

## Overview: Creation of artificial target signals

This package provides a function `inject_counts` to create artificial
cluster signals for addition to background data in order to test and
evaluate the detection capability of the package, particularly the
`find_clusters` method. The function adds a total number of case counts
to a cylindrical region. The counts are added into the 3-column input
frame at locations within a fixed radius of central location.

## Available options for signal injections

For purely spatial clusters, the counts are added on a single date. For
spatio-temporal clusters of events, the counts may be spread over
multiple days. The total number of added case counts, the final
injection date, the fixed radius, and the number of inject days are all
prescribed by the user as calling arguments to `inject_counts`.

There are two ways to specify the center location.

1.  Most directly, the user may use one of the distance matrix locations
    for the argument `target_loc`. This cluster center need not be
    included with case counts in the input frame `cases`.

2.  The user may also have `inject_counts` randomly choose the center
    location from an area within the distance matrix where counts are
    dense or sparse. For this option, distance matrix locations are
    partitioned into deciles according to their total number of counts
    in the input frame `cases`. To specify this method of injection, the
    caller sets `taget_loc` to `NULL` and specifies an integer from 1 to
    10 for `center_decile`, where a value 1 is used to indicate a
    sparse-data rural location and a value of 10 indicates a dense-count
    urban location.

## Generating Cluster Signals for Detection

Calling arguments of `inject_counts` are:

- `cases` : a data frame of (date, location, count) triples in the same
  format as `example_count_data`
- `distance matrix` : as returned by
  `<resolution>_distance_matrix()[["distance_matrix]]`
- `target_loc` : one of the locations in the distance matrix, or if
  NULL,
- `center_decile` : if target_loc is NULL, an integer between 1 and 10
- `radius_miles` : conceptually, the maximum cluster radius about the
  center location; technically, the largest allowable distance from the
  center location centroid to the centroid of another location in the
  cluster
- `nr_cases` : total number of cases to be injected
- `detect_date`: final date for the injected cluster
- `nr_days`: number of days with injected cases; if `nr_days = 1`, all
  injects are on detect_date

## Example Cluster Signal Generation

The following example begins with the package’s `example_count_data`
table, randomizes counts to ensure no initial clusters, and injects 100
case counts over a 20-mile radius at a randomly chosen rural location.
The resulting data frame is then passed to the `find_clusters` to
illustrate detection of the injected signal.

``` r

# Use example count data to find clusters randomize counts to ensure no initial clusters
d <- data.table::copy(example_count_data)

# Here we simply replace the counts with randomly distribution Poisson count data, to ensure that there are no true clusters in the data prior to injection
d[, count := rpois(n = 1, lambda = 15), .(location, date)]

# set the cluster detection date (i.e. end date of prospective space-time clusters) as the final date in the dataset
detect_date <- d[, max(date)]

# get the square distance matrix for counties in Ohio, to align with the package's example data set.
distance_locations <- county_distance_matrix("OH", unit = "miles")

# Confirm there are no clusters after replace counts with random couns
cl <- find_clusters(
  case = d,
  distance_matrix = distance_locations$distance_matrix,
  detect_date = detect_date,
  distance_limit = 50
)
```

Now, we inject a 1-day cluster with a 20-mile radius at a randomly
chosen rural location

``` r

# set seed for reproducibility
set.seed(1)

# We will inject 100 cases
nr_cases1 <- 100

# the center location will not be set explicitly, but rather, chosen randomly
# using the center_decile parameter
target_loc <- NULL
center_decile <- 1

# set the radius within which the injected cases should distributed, relative to
# the center location
radius_miles <- 20

# set the number of days over which the injected cases should be distributed.
nr_days1 <- 1

# Call the function to inject the clusters
scenario_1 <- inject_counts(
  cases = d,
  distance_matrix = distance_locations$distance_matrix,
  target_loc = target_loc,
  center_decile = center_decile,
  radius_miles = radius_miles,
  nr_cases = nr_cases1,
  nr_days = nr_days1,
  end_date = detect_date
)
```

The `inject_counts` function returns a named list of two objects 1.
`case_counts_inj`: a data frame of the same dimensions as the original
location-count-date frame passed to `inject_counts`, but now updated
with new artificial/injected case counts 2. `inject_tbl`: a data frame
holding the injected cases (i.e. only the location-count-date rows that
were added to the original data. This is shown below, for the above
example:

``` r

scenario_1[["inject_tbl"]]
#>    location count       date
#>      <char> <num>     <Date>
#> 1:    39017    51 2025-02-05
#> 2:    39061    49 2025-02-05
```

``` r

# Passed the updated
cluster_data <- find_clusters(
  cases = scenario_1[["case_counts_inj"]],
  distance_matrix = distance_locations$distance_matrix,
  detect_date = detect_date,
  distance_limit = 50
)

# Get tigris based shape file
oh <- tigris::counties("OH", cb = TRUE, class = "sf")
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |===========                                                           |  15%  |                                                                              |===============                                                       |  22%  |                                                                              |=========================                                             |  36%  |                                                                              |===================================                                   |  50%  |                                                                              |=============================================                         |  64%  |                                                                              |=======================================================               |  79%  |                                                                              |=================================================================     |  93%  |                                                                              |======================================================================| 100%
# Pass these to the map_clusters() function
map_clusters(cl = cluster_data, s = oh, s_id = "GEOID")
```

## Example with multiple artificial signals

To create multiple clusters, calls to `inject_counts` may be chained as
follows:

``` r

nr_cases1 <- 100
if (nr_cases1 > 0) {
  set.seed(1)
  target_loc <- "39037" # Darke
  center_decile <- 1
  radius_miles <- 30
  nr_days <- 1
  scen1 <- inject_counts(
    d, distance_locations$distance_matrix, target_loc, center_decile, radius_miles, nr_cases1, nr_days, detect_date
  )

  d1 <- scen1$case_counts_inj

  nr_cases2 <- 30 # Set = 0 to skip second set of injects  #30
  if (nr_cases2 > 0) {
    target_loc <- "39045" # Fairfield
    center_decile <- 1
    radius_miles <- 0.1
    nr_days <- 2
    scen2 <- inject_counts(
      d1, distance_locations$distance_matrix, target_loc, center_decile, radius_miles, nr_cases2, nr_days, detect_date
    )
    data_table <- scen2$case_counts_inj
  } else {
    data_table <- d1
  }
} else {
  data_table <- d
}

# Get Cluster Data
cluster_data <- find_clusters(data_table, distance_locations$distance_matrix, detect_date, distance_limit = 50)

cluster_data$cluster_alert_table
#> Key: <cluster_center>
#>    cluster_center cluster_start_date cluster_end_date cluster_max_distance
#>            <char>             <Date>           <Date>                <num>
#> 1:          39037         2025-02-05       2025-02-05             27.91033
#> 2:          39045         2025-02-04       2025-02-05              0.00000
#>    cluster_center_observed observed expected log_obs_exp threshold alert_gap
#>                      <num>    <int>    <num>       <num>     <num>     <num>
#> 1:                      34      177 80.32448   0.7900753 0.3165810 0.4734943
#> 2:                      66       66 30.73040   0.7644024 0.5828298 0.1815726
#>    alert_ratio n_cluster_locations
#>          <num>               <int>
#> 1:    2.495650                   5
#> 2:    1.311536                   1

# Again, pass to the map_clusters() function
map_clusters(cl = cluster_data, s = oh, s_id = "GEOID")
```
