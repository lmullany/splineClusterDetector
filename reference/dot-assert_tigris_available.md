# Helper function simply asserts if tigris is installed. It is not required, to run the package in general, but is required for some additional functionality

Helper function simply asserts if tigris is installed. It is not
required, to run the package in general, but is required for some
additional functionality

## Usage

``` r
.assert_tigris_available(fn_name)
```

## Arguments

- fn_name:

  Character scalar naming the calling function, used in the error
  message.

## Value

Invisibly returns `NULL`; otherwise throws an error when tigris is
unavailable.
