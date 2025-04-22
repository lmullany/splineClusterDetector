# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

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
