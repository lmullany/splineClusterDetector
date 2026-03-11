## R CMD check results

0 errors | 0 warnings | 1 note

## Submission

This is a new release.

## Notes

There is one expected note:

* `Suggests` includes the non-CRAN package `Rnssp`.

`Rnssp` is optional and is only used for one alternate data source in
`county_distance_matrix()`. The default behavior uses this package's built-in
datasets and does not require `Rnssp`.

Examples, tests, and the primary user workflow do not require `Rnssp`, so the
package can be checked and used without it being installed.

`Rnssp` is available from:
https://cdcgov.github.io/Rnssp/
