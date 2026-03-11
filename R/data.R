#' County Location Dataset
#'
#' A data set that provides latitude and longitude for each
#' county in the United Sates
#'
#' @format
#' A data frame with 3,144 rows and 6 columns:
#' \describe{
#'   \item{state_name, state}{full and abbreviated names for states}
#'   \item{state_fips, fips}{state and county fips codes}
#'   \item{longitude, latitude}{numeric coordinates for fips}
#' }
#' @source `tigris` package
"counties"

#' Zipcode Location Dataset
#'
#' A data set that provides latitude and longitude for each
#' zipcode in the United Sates
#'
#' @format
#' A data frame with 42,482 rows and 11 columns:
#' \describe{
#'   \item{id}{serial integer id (1, 2, 3, .. etc)}
#'   \item{zip_code}{5 digit string for zipcode}
#'   \item{state}{state abbreviation}
#'   \item{county}{county name}
#'   \item{region}{region name}
#'   \item{region_id}{id for region}
#'   \item{region_name}{region name}
#'   \item{pop, modified}{undocumented}
#'   \item{latitude, longitude}{numeric coordinates for zipcode}
#' }
#' @source unknown
"zipcodes"

#' Spline Lookup Table - 0.001
#'
#' Spline threshold lookup table, p-value = 0.001
#'
#' @format A data frame with 399 rows and 2 columns:
#' \describe{
#'   \item{observed}{number of observed in cluster}
#'   \item{spl_thresh}{log observed-over-expected above which cluster
#'    is significant at the 0.001 level}
#' }
#' @source package authors
"spline_001"

#' Spline Lookup Table - 0.005
#'
#' Spline threshold lookup table, p-value = 0.005
#'
#' @format A data frame with 399 rows and 2 columns:
#' \describe{
#'   \item{observed}{number of observed in cluster}
#'   \item{spl_thresh}{log observed-over-expected above which
#'    cluster is significant at the 0.005 level}
#' }
#' @source package authors
"spline_005"

#' Spline Lookup Table - 0.01
#'
#' Spline threshold lookup table, p-value = 0.01
#'
#' @format A data frame with 399 rows and 2 columns:
#' \describe{
#'   \item{observed}{number of observed in cluster}
#'   \item{spl_thresh}{log observed-over-expected above which
#'   cluster is significant at the 0.01 level}
#' }
#' @source package authors

"spline_01"

#' Spline Lookup Table - 0.05
#'
#' Spline threshold lookup table, p-value = 0.05
#'
#' @format A data frame with 399 rows and 2 columns:
#' \describe{
#'   \item{observed}{number of observed in cluster}
#'   \item{spl_thresh}{log observed-over-expected above which
#'   cluster is significant at the 0.05 level}
#' }
#' @source package authors
"spline_05"

#' Example Count Dataset
#'
#' Synthetic county-level example count data for package examples and tests. 
#' Generation included a synthetic injection of cases near the end of the time
#' series to ensure that clusters are detected in this example dataset.
#'
#' @format A data frame with 11,264 rows and 4 columns:
#' \describe{
#'   \item{location}{county FIPS code as character}
#'   \item{date}{date of observation}
#'   \item{count}{non-negative integer daily count}
#' }
#' @source package authors
