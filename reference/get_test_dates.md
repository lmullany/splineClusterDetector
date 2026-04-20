# Generate test dates vector

Function to generate test dates given an end date and test length

## Usage

``` r
get_test_dates(end_date, test_length)
```

## Arguments

- end_date:

  End date of the test interval

- test_length:

  (integer) length of the test interval in days

## Value

vector of dates

## Examples

``` r
get_test_dates(
  end_date = "2025-01-01",
  test_length = 10
)
#>  [1] "2024-12-23" "2024-12-24" "2024-12-25" "2024-12-26" "2024-12-27"
#>  [6] "2024-12-28" "2024-12-29" "2024-12-30" "2024-12-31" "2025-01-01"
```
