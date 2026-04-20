# Summary count-by-location-and-date data, given baseline and test interval lengths, and an end-date for the test interval

Function will return a summary data frame of information related to a
given count-by-location-and-date dataset, provided the user gives the
count data, a set of locations, and the length of the baseline and test
intervals, and and end date for the test interval. Note that a guard, a
buffer between the end of the baseline interval and the test interval
can be provided.

## Usage

``` r
generate_summary_table(
  data,
  end_date = NULL,
  locations = NULL,
  baseline_length = 90,
  test_length = 7,
  guard = 0,
  cut_vec = c(0, 1.5, 2.5, 5.5, 10.5, Inf),
  cut_labels = c("Nr. Locs, daily mean 1 or less", "Nr. Locs, daily mean 2",
    "Nr. Locs, daily mean 3-5", "Nr. Locs, daily mean 6-10", "Nr. Locs, daily mean >10")
)
```

## Arguments

- data:

  data frame with (at least) three columns: location, date, count

- end_date:

  date indicating end of test interval; if not provided the last date in
  \`dt\` will be used

- locations:

  a vector of locations to subset the table; if none provided then all
  locations will be used

- baseline_length:

  numeric (default=90) number of days in baseline interval

- test_length:

  numeric (default=7) number of days in test interval

- guard:

  numeric (default=0) number of days between baseline and test interval

- cut_vec:

  numeric vector of n cut points to examine categories of daily mean
  counts

- cut_labels:

  character vector of labels for the n-1 categories created by
  \`cut_vec\`

## Value

data frame of summary statistics

## Examples

``` r
generate_summary_table(
  data = example_count_data
)
#>          Statistic (rounded means) Baseline Interval Test Interval
#>                             <char>             <num>         <num>
#>  1:                      Nr. Dates              90.0           7.0
#>  2:                Nr. Total Cases           63803.0        9673.0
#>  3:                  Cases per Day             708.9        1381.9
#>  4:        Nr. Locations with Data              88.0          88.0
#>  5:      Nr. Locations, no records               0.0           0.0
#>  6: Nr. Locs, daily mean 1 or less              15.0           8.0
#>  7:         Nr. Locs, daily mean 2              13.0           6.0
#>  8:       Nr. Locs, daily mean 3-5              29.0          16.0
#>  9:      Nr. Locs, daily mean 6-10              13.0          25.0
#> 10:       Nr. Locs, daily mean >10              18.0          32.0
```
