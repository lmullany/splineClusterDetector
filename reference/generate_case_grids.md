# Get candidate clusters and locations in baseline intervals

Given raw case counts by location, and some dates and other params
return candidate clusters and counts

## Usage

``` r
generate_case_grids(
  cases,
  detect_date,
  baseline_length = 90,
  max_test_window_days = 7,
  guard_band = 0,
  baseline_adjustment = c("add_one", "add_one_global", "add_test", "none"),
  adj_constant = 1
)
```

## Arguments

- cases:

  frame of cases with counts, location(s) and dates

- detect_date:

  date to end examination of detection of clusters

- baseline_length:

  number of days (integer) used for baseline detection (default = 90)

- max_test_window_days:

  integer, max number of days in a detected cluster, defaults to 7

- guard_band:

  integer (default=0) number of days buffer between test interval and
  baseline

- baseline_adjustment:

  one of three string options: "add_one" (default), "add_test", or
  "none". All methods except for "none" will ensure that the
  log(obs/expected) is always defined (i.e. avoids expected =0). For the
  default, this will add 1 to the expected for any individual
  calculation if expected would otherwise be zero. For
  "add_test_interval", each location in the baseline is increased by the
  number of cases in that location during the test interval. If "none",
  no adjustment is made.

- adj_constant:

  numeric (default=1.0); this is the constant to be added if
  `baseline_adjustment == 'add_one'` or
  `baseline_adjustment == 'add_one'`

## Value

an object of class \`CaseGrids\` contain a list of items

- \`baseline_counts_by_location\`: a frame of counts over the baseline
  interval by location

- \`case_grid\`: a frame of cases during the test dates, with reverse
  cumulative counts within location, by date

- \`case_grid_totals_by_date\`: reverse cumulative sum of counts over
  all locations, by date

- \`test_cases\`: case location counts only during the test dates

- \`detect_date\`: the detect date passed to this function

- \`baseline_total\`: an integer holding the total counts over all
  locations and dates

## Examples

``` r
dd <- example_count_data[, max(date)]
generate_case_grids(
  cases = example_count_data,
  detect_date = dd
)
#> $baseline_counts_by_location
#>     location base_loc_sums
#>       <char>         <num>
#>  1:    39001           251
#>  2:    39003          1263
#>  3:    39005           489
#>  4:    39007           650
#>  5:    39009           560
#>  6:    39011           410
#>  7:    39013           110
#>  8:    39015           160
#>  9:    39017          1383
#> 10:    39019           179
#> 11:    39021           246
#> 12:    39023           982
#> 13:    39025           702
#> 14:    39027           295
#> 15:    39029           574
#> 16:    39031           144
#> 17:    39033           515
#> 18:    39035          6679
#> 19:    39037           327
#> 20:    39039           335
#> 21:    39041           839
#> 22:    39043           533
#> 23:    39045          1776
#> 24:    39047           101
#> 25:    39049          6490
#> 26:    39051           289
#> 27:    39053            97
#> 28:    39055           170
#> 29:    39057          1203
#> 30:    39059           389
#> 31:    39061          2270
#> 32:    39063           384
#> 33:    39065           147
#> 34:    39067           100
#> 35:    39069           133
#> 36:    39071           310
#> 37:    39073           440
#> 38:    39075           126
#> 39:    39077          1014
#> 40:    39079            83
#> 41:    39081           326
#> 42:    39083           129
#> 43:    39085           782
#> 44:    39087            48
#> 45:    39089          1463
#> 46:    39091           153
#> 47:    39093          1525
#> 48:    39095          2792
#> 49:    39097           314
#> 50:    39099          1886
#> 51:    39101           490
#> 52:    39103           815
#> 53:    39105           134
#> 54:    39107           257
#> 55:    39109           650
#> 56:    39111            66
#> 57:    39113          4404
#> 58:    39115           112
#> 59:    39117           246
#> 60:    39119           488
#> 61:    39121           114
#> 62:    39123           298
#> 63:    39125           154
#> 64:    39127           159
#> 65:    39129           316
#> 66:    39131           104
#> 67:    39133           695
#> 68:    39135           142
#> 69:    39137           240
#> 70:    39139          1129
#> 71:    39141           374
#> 72:    39143           370
#> 73:    39145           248
#> 74:    39147           427
#> 75:    39149           239
#> 76:    39151          1284
#> 77:    39153          2745
#> 78:    39155           952
#> 79:    39157           490
#> 80:    39159           205
#> 81:    39161           153
#> 82:    39163           123
#> 83:    39165           833
#> 84:    39167           402
#> 85:    39169           388
#> 86:    39171           217
#> 87:    39173           592
#> 88:    39175           182
#>     location base_loc_sums
#>       <char>         <num>
#> 
#> $case_grid
#>      location       date count count_sum
#>        <char>     <Date> <int>     <int>
#>   1:    39001 2025-01-30     9        39
#>   2:    39003 2025-01-30    67       335
#>   3:    39005 2025-01-30    44       166
#>   4:    39007 2025-01-30     7        69
#>   5:    39009 2025-01-30    59       215
#>  ---                                    
#> 598:    39163 2025-02-05     4         4
#> 599:    39165 2025-02-05    20        20
#> 600:    39167 2025-02-05    11        11
#> 601:    39169 2025-02-05     8         8
#> 602:    39173 2025-02-05     7         7
#> 
#> $case_grid_totals_by_date
#>          date test_totals
#>        <Date>       <int>
#> 1: 2025-01-30        9673
#> 2: 2025-01-31        8267
#> 3: 2025-02-01        6898
#> 4: 2025-02-02        5553
#> 5: 2025-02-03        4256
#> 6: 2025-02-04        2858
#> 7: 2025-02-05        1447
#> 
#> $test_cases
#>      location       date count
#>        <char>     <IDat> <int>
#>   1:    39001 2025-01-30     9
#>   2:    39001 2025-01-31     4
#>   3:    39001 2025-02-01     7
#>   4:    39001 2025-02-02     7
#>   5:    39001 2025-02-03     2
#>  ---                          
#> 612:    39171 2025-02-05     0
#> 613:    39173 2025-02-04     6
#> 614:    39173 2025-02-05     7
#> 615:    39175 2025-02-04     2
#> 616:    39175 2025-02-05     0
#> 
#> $detect_date
#> [1] "2025-02-05"
#> 
#> $baseline_total
#> [1] 63803
#> 
#> attr(,"class")
#> [1] "list"      "CaseGrids"
```
