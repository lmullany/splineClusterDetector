# Example Count Dataset

Synthetic county-level example count data for package examples and
tests. Generation included a synthetic injection of cases near the end
of the time series to ensure that clusters are detected in this example
dataset.

## Usage

``` r
example_count_data
```

## Format

A data frame with 11,264 rows and 4 columns:

- location:

  county FIPS code as character

- date:

  date of observation

- count:

  non-negative integer daily count

## Source

package authors
