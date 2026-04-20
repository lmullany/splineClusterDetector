# Get heat map data from a set of location, date, count data

Generate heat map data frame count information by date and location
given an input frame of count-by-location-and-date data.

## Usage

``` r
generate_heatmap_data(
  data,
  end_date = NULL,
  locations = NULL,
  baseline_length = 90,
  test_length = 7,
  guard = 0,
  break_points = c(-1, 2, 4, 9, 19, Inf),
  break_labels = c("0-1", "2-4", "5-9", "10-19", "20+")
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

- break_points:

  break points for the discrete groups (default = `c(-1,2,4,9,19,Inf)`)

- break_labels:

  string vector of labels for the groups (default =
  `c("0-1", "2-4", "5-9", "10-19", "20+")`)

## Value

a data frame of heat map data

## Examples

``` r
generate_heatmap_data(
  data = example_count_data
)
#>       location       date count count_cat
#>         <char>     <IDat> <int>    <fctr>
#>    1:    39001 2024-11-01     2       0-1
#>    2:    39001 2024-11-02     1       0-1
#>    3:    39003 2024-11-01     7       5-9
#>    4:    39003 2024-11-02     9       5-9
#>    5:    39005 2024-11-01     2       0-1
#>   ---                                    
#> 8532:    39171 2025-02-05     0       0-1
#> 8533:    39173 2025-02-04     6       5-9
#> 8534:    39173 2025-02-05     7       5-9
#> 8535:    39175 2025-02-04     2       0-1
#> 8536:    39175 2025-02-05     0       0-1
```
