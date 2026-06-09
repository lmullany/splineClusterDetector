# Clean and finalize cluster alert table:

Function takes the raw cluster alert table, and cleans it up, namely
removing some columns that are not needed, renaming some columns, and
setting the final order of columns

## Usage

``` r
.clean_up_alert_table(clt)
```

## Arguments

- clt:

  cluster alert table holding the cluster specific information

## Value

data.table of cleaned up column names
