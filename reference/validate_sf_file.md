# Validates sf file

Function ingests an object and check if this is of class sf, and has an
unique identifer for its rows

## Usage

``` r
validate_sf_file(s, s_id, label_id = NULL)
```

## Arguments

- s:

  shape file; must be of class sf

- s_id:

  string unique identifier of \`s\`

- label_id:

  string column of \`s\` that indicates display label for the row in
  \`s\` (default is NULL)
