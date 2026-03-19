#' Function returns the number of meters in unit (one of miles,
#' kilometers, or meters)
#' @param unit Character scalar specifying distance unit; one of
#'   \code{"miles"}, \code{"kilometers"}, or \code{"meters"}.
#' @returns Numeric scalar conversion factor from the selected unit to meters.
#' @keywords internal
.meters_per_unit <- function(unit) {
  unit <- match.arg(unit, c("miles", "kilometers", "meters"))
  c("miles" = 1609.344, "kilometers" = 1000, "meters" = 1)[[unit]]
}

#' Helper function simply asserts if tigris is installed. It is not
#' required, to run the package in general, but is required
#' for some additional functionality
#' @param fn_name Character scalar naming the calling function, used in the
#'   error message.
#' @returns Invisibly returns \code{NULL}; otherwise throws an error when
#'   \pkg{tigris} is unavailable.
#' @keywords internal
.assert_tigris_available <- function(fn_name) {
  if (!requireNamespace("tigris", quietly = TRUE)) {
    cli::cli_abort(
      paste(
        "{.pkg tigris} is required for",
        paste0("{.fn ", fn_name, "}."),
        "Install it with install.packages('tigris')."
      ),
      class = "tigris_not_available"
    )
  }
}

#' Helper function resolves coordinate variable names
#' @param lat_var Character scalar latitude column name or \code{NULL} to use
#'   \code{"latitude"}.
#' @param long_var Character scalar longitude column name or \code{NULL} to use
#'   \code{"longitude"}.
#' @returns A named list with elements \code{lat_var} and \code{long_var}.
#' @keywords internal
.resolve_coord_var_names <- function(lat_var = NULL, long_var = NULL) {
  if (is.null(lat_var)) {
    lat_var <- "latitude"
  }
  if (is.null(long_var)) {
    long_var <- "longitude"
  }

  if (!is.character(lat_var) || length(lat_var) != 1) {
    cli::cli_abort(
      "{.arg lat_var} must be NULL or a single column name."
    )
  }
  if (!is.character(long_var) || length(long_var) != 1) {
    cli::cli_abort(
      "{.arg long_var} must be NULL or a single column name."
    )
  }

  list(lat_var = lat_var, long_var = long_var)
}

#' Helper function: given a data frame, and strings for label_var,
#' lat_var, and long_var, the df is checked for
#' @param df A \code{data.frame} containing label and coordinate columns.
#' @param label_var Character scalar naming the label column.
#' @param lat_var Character scalar naming the latitude column.
#' @param long_var Character scalar naming the longitude column.
#' @returns A \code{data.table} with standardized columns
#'   \code{location}, \code{latitude}, and \code{longitude}.
#' @keywords internal
.validate_custom_locations <- function(df, label_var, lat_var, long_var) {
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be an object of class data.frame.")
  }

  if (!is.character(label_var) || length(label_var) != 1) {
    cli::cli_abort("{.arg label_var} must be a single column name.")
  }

  check_vars(df, c(label_var, lat_var, long_var))

  locs <- data.table::as.data.table(df)[
    , .SD,
    .SDcols = c(label_var, lat_var, long_var)
  ]
  data.table::setnames(locs, new = c("location", "latitude", "longitude"))

  # Validate that the label is not NA and unique
  if (any(is.na(locs$location))) {
    cli::cli_abort("Label column contains NA values.")
  }
  if (anyDuplicated(locs$location)) {
    cli::cli_abort("Values in {.arg label_var} must be unique.")
  }

  # Validated that lat and long are numeric and not missing
  if (!is.numeric(locs$latitude) || !is.numeric(locs$longitude)) {
    cli::cli_abort("Latitude and longitude columns must be numeric.")
  }
  if (any(is.na(locs$latitude)) || any(is.na(locs$longitude))) {
    cli::cli_abort(
      "Latitude and longitude columns cannot contain NA values."
    )
  }

  locs
}

#' Helper function reduces a data frame to only those rows where latitude
#' and longitude are not missing
#' @param locs A \code{data.table} with \code{latitude} and \code{longitude}
#'   columns.
#' @returns \code{data.table} filtered to non-missing latitude rows with
#'   numeric latitude/longitude columns.
#' @keywords internal
.numeric_location_coords <- function(locs) {
  latitude <- NULL

  # reduce to valid locations
  locs <- locs[!is.na(latitude) & !latitude == "NULL"]

  # replace any char with numeric
  for (c in c("latitude", "longitude")) {
    data.table::set(locs, j = c, value = as.numeric(locs[[c]]))
  }

  locs
}

#' Helper function gets the distance in meter between
#' pairs of coordinates. Note that \code{coords} must be a matrix
#' or frame, where the first col is longitude and the second
#' column is latitude
#' @param coords Matrix-like object with longitude in column 1 and latitude in
#'   column 2.
#' @returns Numeric square matrix of pairwise distances in meters.
#' @keywords internal
.distance_meters_from_coords <- function(coords) {
  pts <- sf::st_as_sf(
    data.frame(longitude = coords[, 1], latitude = coords[, 2]),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  dist_units <- sf::st_distance(pts)
  matrix(
    as.numeric(dist_units),
    nrow = nrow(dist_units),
    ncol = ncol(dist_units)
  )
}

#' Helper function takes a vector of locations, and a set of coords
#' (which must be a matrix or frame with first two columns being longitude
#' and latitude), and returns a square distance matrix for all pairs
#' of coordinates in a given unit
#' @param loc_vec Character vector of location identifiers used as matrix
#'   row/column names.
#' @param coords Matrix-like object with longitude in column 1 and latitude in
#'   column 2.
#' @param unit Character scalar unit for returned distances; one of
#'   \code{"miles"}, \code{"kilometers"}, or \code{"meters"}.
#' @returns A list with elements \code{loc_vec} and \code{distance_matrix}.
#' @keywords internal
.distance_result_from_coords <- function(
  loc_vec,
  coords,
  unit = c("miles", "kilometers", "meters")
) {
  if (length(loc_vec) == 0 || nrow(coords) == 0) {
    cli::cli_abort("No valid locations found for distance computation.")
  }

  # get the unit
  unit <- match.arg(unit)

  # get the meters for this unit
  meters_per <- .meters_per_unit(unit)

  # get the distance matrix
  dist_meters <- .distance_meters_from_coords(coords)

  # convert it to the right distance (i.e to the requested unit)
  distance_matrix <- dist_meters / meters_per

  # add dimnames
  dimnames(distance_matrix) <- list(loc_vec, loc_vec)

  # return the list
  list(
    loc_vec = loc_vec,
    distance_matrix = distance_matrix
  )
}

#' This is a helper function to create a named list of all the locations in
#' \code{locs} within \code{threshold_meters} of each loc in \code{locs}.
#' @param locs A \code{data.table} with columns \code{location},
#'   \code{latitude}, and \code{longitude}.
#' @param threshold_meters Numeric scalar distance threshold in meters.
#' @param meters_per_unit Numeric scalar conversion factor from output unit to
#'   meters.
#' @returns Named list of numeric vectors of neighbor distances, keyed by
#'   location.
#' @keywords internal
.sparse_dist_list_from_locs <- function(
  locs,
  threshold_meters,
  meters_per_unit
) {
  # convert to sf
  locs <- sf::st_as_sf(locs, coords = c("longitude", "latitude"), crs = 4326)

  # get sparse predicate that holds just those locations within certain distance
  # i.e. the neighbors
  nb <- sf::st_is_within_distance(locs, dist = threshold_meters)

  # get the number of locations
  n <- nrow(locs)

  # Flatten indices (aligned with sgbp order)
  ii <- rep.int(seq_len(n), lengths(nb))
  jj <- unlist(nb, use.names = FALSE)

  # Use sf only for pairwise neighbor distances (returned in meters).
  geom <- sf::st_geometry(locs)
  d <- sf::st_distance(
    geom[ii],
    geom[jj],
    by_element = TRUE
  )
  # convert the distances vector to the unit desired
  d <- as.numeric(d) / meters_per_unit

  # convert to list
  d <- split(d, rep.int(seq_len(n), lengths(nb)))

  # Now, lets get the locations in each of d
  loc_names <- locs$location[unlist(nb, use.names = FALSE)]
  loc_names <- split(loc_names, rep.int(seq_len(n), lengths(nb)))

  # apply these locations as names along each element of each element of d
  d <- lapply(seq_along(d), \(i) sort(stats::setNames(d[[i]], loc_names[[i]])))

  # and now place the names of d itself
  names(d) <- locs$location

  d
}

#' Get distance matrix for zip codes within a state
#'
#' Function returns a list of zipcodes and a matrix with the distance between
#' those zip codes. leverages a built in dataset (`zipcodes`) that maps
#' zipcodes to counties.
#' @param st two-character string denoting a state
#' @param unit string, one of "miles" (default), "kilometers", or "meters".
#'   Indicating the desired unit for the distances
#' @export
#' @returns a named list of length two; first element (`loc_vec`) is a vector of
#' locations and the second element (`distance_matrix`) is a square matrix
#' containing the pairwise distance (in the given `unit`) between all locations.
#' @examples
#' zip_distance_matrix("MD")
zip_distance_matrix <- function(
  st,
  unit = c("miles", "kilometers", "meters")
) {
  unit <- match.arg(unit)

  # global declarations to avoid check CMD errors
  state <- zip_code <- longitude <- latitude <- NULL

  # get the subset of the built in dataset for this state, limit rows, and only
  # where lat/long available

  mapping <- zipcodes[
    state %chin% st & !is.na(latitude) & !latitude == "NULL",
    list("location" = zip_code, latitude, longitude)
  ]

  mapping <- .numeric_location_coords(mapping)

  .distance_result_from_coords(
    loc_vec = mapping[["location"]],
    coords = as.matrix(mapping[, list(longitude, latitude)]),
    unit = unit
  )
}


#' Get distance matrix for counties within a state
#'
#' Function returns a list of counties and a matrix with the distance between
#' those counties. leverages a built in dataset (`counties`).
#' @param st two-character string denoting a state, or "US". If "US", then this
#'   is equivalent to calling \code{us_distance_matrix()}.
#' @param unit string, one of "miles" (default), "kilometers", or "meters".
#'   Indicating the desired unit for the distances
#' @param source string indicating either "tigris" (default) or "rnssp". Both
#'   are built-in datasets (i.e. are part of this package). The default
#'   ("tigris") uses county names and locations as found in tigris 2024. The
#'   "rnssp" option uses a package-stored version of the publicly available
#'   shape file for counties from Rnssp package at
#'   https://cdcgov.github.io/Rnssp/
#'
#' @export
#' @returns a named list of length two; first element (`loc_vec`) is a vector of
#'   locations and the second element (`distance_matrix`) is a square matrix
#'   containing the pairwise distance (in the given `unit`) between all
#'   locations.
#' @examples
#' county_distance_matrix("MD", source = "tigris")
#' county_distance_matrix("WI", source = "rnssp", unit = "kilometers")
county_distance_matrix <- function(
  st,
  unit = c("miles", "kilometers", "meters"),
  source = c("tigris", "rnssp")
) {
  # if State = "US" pass this request on to us_distance_matrix()
  # which always uses built-in tigris style dataset
  if (st == "US") {
    us_distance_matrix(unit = unit)
  } else {
    # match source
    source <- match.arg(source)
    unit <- match.arg(unit)

    # global declarations to avoid check CMD errors
    state <- fips <- longitude <- latitude <- NULL

    # get the subset of the built in dataset for this state,
    # limit rows, and only
    # where lat/long available

    if (source == "rnssp") {
      # look up the state fips code for this two letter code
      st <- state_fips_codes[
        state_fips_codes$STUSPS == toupper(st),
      ]$STATEFP |>
        as.character()

      county_sf <- county_sf[county_sf$STATEFP == st, ]
      loc_vec <- county_sf$GEOID
      dist_units <- suppressWarnings(
        county_sf |> sf::st_centroid() |> sf::st_distance()
      )
      distance_matrix <- matrix(
        as.numeric(dist_units),
        nrow = nrow(dist_units),
        ncol = ncol(dist_units)
      )
      distance_matrix <- distance_matrix / .meters_per_unit(unit)
      dimnames(distance_matrix) <- list(loc_vec, loc_vec)

      list(
        loc_vec = as.character(loc_vec),
        distance_matrix = distance_matrix
      )
    } else {
      mapping <- counties[
        state %chin% toupper(st) & !is.na(latitude) & !latitude == "NULL",
        list("location" = fips, latitude, longitude)
      ]

      mapping <- .numeric_location_coords(mapping)
      .distance_result_from_coords(
        loc_vec = mapping[["location"]],
        coords = as.matrix(mapping[, list(longitude, latitude)]),
        unit = unit
      )
    }
  }
}


#' Get distance matrix for all counties in the US
#'
#' Function returns a list of counties and a matrix with the distance between
#' those counties. leverages a built in dataset (`counties`). Note that the
#' generation of this matrix can take a few seconds. Note: it is better and
#' faster to use \code{create_dist_list()}.
#' @param unit string, one of "miles" (default), "kilometers", or "meters".
#'   Indicating the desired unit for the distances
#' @export
#' @returns a named list of length two; first element (`loc_vec`) is a vector of
#' locations and the second element (`distance_matrix`) is a square matrix
#' containing the pairwise distance (in the given `unit`) between all locations.
#' @examples
#' \donttest{
#' # Takes ~ 10 seconds, depending on machine
#' us_distance_matrix(unit = "kilometers")
#' }
us_distance_matrix <- function(
  unit = c("miles", "kilometers", "meters")
) {
  # provide warning re the time it takes to construct this matrix
  cli::cli_alert_danger("Warning... this will take a few seconds...")

  unit <- match.arg(unit)

  # global declarations to avoid check CMD errors
  fips <- longitude <- latitude <- NULL

  mapping <- counties[
    !is.na(latitude) & !latitude == "NULL",
    list("location" = fips, latitude, longitude)
  ]

  mapping <- .numeric_location_coords(mapping)
  result <- .distance_result_from_coords(
    loc_vec = mapping[["location"]],
    coords = as.matrix(mapping[, list(longitude, latitude)]),
    unit = unit
  )

  cli::cli_alert_success("... Ok, complete.")

  result
}

#' Generalized distance list as sparse list
#'
#' This function is an alternative to the package functions that create a square
#' distance matrix of dimension N, with all pairwise distances. In this approach
#' a list of named vectors is returned, where there is one element in the list
#' for each location, and each named vector holds the distance within
#' `threshold` of the location.
#' @param level string either "county", "zip", or "tract"
#' @param threshold numeric value; include in each location-specific named
#'   vector only those locations that a within `threshold` distance units of the
#'   target. Reasonable thresholds might be 50 (miles), 15 (miles) and 3 (miles)
#'   for county, zip, and tract, respectively, but these can be adjusted. Note
#'   if a different unit other than miles is used, then the user should also
#'   adjust this parameter appropriately
#' @param st string; optional to specify a state; if NULL distances are returned
#'   for all zip codes or counties in the US
#' @param county string vector of 3-fips to restrict within \code{st}; ignored
#'   unless \code{level} is "tract"
#' @param unit string one of miles (default), kilometers, or meters; this is the
#'   unit relevant to the threshold
#' @export
#' @returns a named list, where each element, named by a target location, is a
#'   named vector of distances that are within `threshold` `units` of the
#'   target.
#' @examples
#' create_dist_list(
#'   level = "tract",
#'   threshold = 3,
#'   st = "MD"
#' )
#' create_dist_list(
#'   level = "county",
#'   threshold = 50,
#'   st = "CA",
#'   unit = "kilometers"
#' )
create_dist_list <- function(
  level,
  threshold,
  st = NULL,
  county = NULL,
  unit = c("miles", "kilometers", "meters")
) {
  state <- zip_code <- latitude <- longitude <- fips <- NULL

  level <- match.arg(level, c("county", "zip", "tract"))
  unit <- match.arg(unit)
  factor <- .meters_per_unit(unit)

  # In case threshold is null, revert to default
  if (is.null(threshold)) {
    cli::cli_abort("Threshold cannot be null")
  }


  if (level == "tract") {
    if (is.null(st)) {
      cli::cli_abort(
        "Tract distance list can only be created for a
         single state, `st` must not be null"
      )
    }
    tracts <- tract_generator(st = st, county = county)
    return(
      create_custom_dist_list(
        df = tracts,
        label_var = "geoid",
        lat_var = "latitude",
        long_var = "longitude",
        threshold = threshold,
        unit = unit
      )
    )
  } else if (level == "zip") {
    if (!is.null(st)) {
      locs <- zipcodes[
        state == st,
        list(location = zip_code, latitude, longitude)
      ]
    } else {
      locs <- zipcodes[, list(location = zip_code, latitude, longitude)]
    }
  } else {
    if (!is.null(st)) {
      locs <- counties[state == st, list(location = fips, latitude, longitude)]
    } else {
      locs <- counties[, list(location = fips, latitude, longitude)]
    }
  }

  # convert within to meters
  threshold <- threshold * factor

  locs <- .numeric_location_coords(locs)
  .sparse_dist_list_from_locs(
    locs = locs,
    threshold_meters = threshold,
    meters_per_unit = factor
  )
}

#' Create a sparse distance list from custom location data
#'
#' This function is a custom-data version of \code{create_dist_list()}. It
#' returns a list of named numeric vectors where each list element contains only
#' locations within \code{threshold} distance units of a target location.
#'
#' @param df data.frame containing label and coordinate columns
#' @param label_var character scalar; column name used as location label (must
#'   be unique and non-missing)
#' @param lat_var character scalar; latitude column name.
#' @param long_var character scalar; longitude column name.
#' @param threshold numeric scalar distance cutoff in units of \code{unit}
#' @param unit string, one of "miles" (default), "kilometers", or "meters"
#' @export
#' @returns a named list, where each element, named by a target location, is a
#'   named vector of distances that are within `threshold` `units` of the
#'   target.
#' @examples
#' md <- tract_generator("MD")
#' dlist <- create_custom_dist_list(
#'   df = md,
#'   label_var = "geoid",
#'   lat_var = "latitude",
#'   long_var = "longitude",
#'   threshold = 15,
#'   unit = "miles"
#' )
create_custom_dist_list <- function(
  df,
  label_var,
  lat_var,
  long_var,
  threshold,
  unit = c("miles", "kilometers", "meters")
) {
  coord_vars <- .resolve_coord_var_names(lat_var = lat_var, long_var = long_var)
  lat_var <- coord_vars$lat_var
  long_var <- coord_vars$long_var

  if (
    !is.numeric(threshold) ||
      length(threshold) != 1 ||
      is.na(threshold) ||
      threshold < 0
  ) {
    cli::cli_abort(
      "{.arg threshold} must be a single non-negative numeric value."
    )
  }

  unit <- match.arg(unit)
  factor <- .meters_per_unit(unit)
  threshold_meters <- threshold * factor

  locs <- .validate_custom_locations(
    df = df,
    label_var = label_var,
    lat_var = lat_var,
    long_var = long_var
  )

  if (nrow(locs) == 0) {
    return(list())
  }

  .sparse_dist_list_from_locs(
    locs = locs,
    threshold_meters = threshold_meters,
    meters_per_unit = factor
  )
}


#' Generate Census Tract Centroids for a State
#'
#' Pulls census tracts using \pkg{tigris}, computes tract centroids, and returns
#' a three-column \pkg{data.table} with GEOID, latitude, and longitude.
#'
#' @param st Character scalar; either a 2-digit state FIPS code (for example,
#'   \code{"24"}) or a 2-letter USPS abbreviation (for example, \code{"MD"}).
#' @param county A three-digit FIPS code (string) of the county or counties to
#'   subset on. This can also be a county name or vector of names.
#' @param use_cache a boolean, defaults to TRUE, to set tigris option to use
#'   cache
#' @param ... arguments to be passed on to tigris::tracts()
#'
#' @return A \code{data.table} with columns:
#' \describe{
#'   \item{geoid}{11-digit tract GEOID (\code{state(2) + county(3) + tract(6)})}
#'   \item{latitude}{Centroid latitude in WGS84}
#'   \item{longitude}{Centroid longitude in WGS84}
#' }
#' @export
#'
#' @examples
#' md_tracts <- tract_generator("24")
#' md_tracts2 <- tract_generator("MD")
#' howard_county_tracts <- tract_generator("MD", county = "027")
#' head(md_tracts)
tract_generator <- function(
  st,
  county = NULL,
  use_cache = TRUE,
  ...
) {
  .assert_tigris_available("tract_generator")

  if (
    !is.character(st) ||
      length(st) != 1 ||
      nchar(st) != 2
  ) {
    cli::cli_abort(
      "{.arg st} must be a single 2-character value (e.g., '24' or 'MD')."
    )
  }
  if (!grepl("^([0-9]{2}|[A-Za-z]{2})$", st)) {
    cli::cli_abort(
      "{.arg st} must contain exactly two digits or two letters."
    )
  }
  if (grepl("^[A-Za-z]{2}$", st)) {
    st <- toupper(st)
  }

  options(tigris_use_cache = use_cache)

  tracts_sf <- suppressMessages(
    tigris::tracts(state = st, county = county, ...)
  )
  centroids_sf <- suppressWarnings(
    sf::st_transform(sf::st_centroid(tracts_sf), 4326)
  )
  coords <- sf::st_coordinates(centroids_sf)

  result <- data.table::data.table(
    geoid = centroids_sf$GEOID,
    latitude = coords[, "Y"],
    longitude = coords[, "X"]
  )


  result
}

#' Build a Tract Distance Matrix for a State
#'
#' Creates an all-pairs distance matrix between census tract centroids for a
#' state, using state abbreviation input similar to
#' \code{zip_distance_matrix()}.
#'
#' @param st Character scalar; 2-character USPS state abbreviation
#'   (for example, \code{"MD"}).
#' @param county A three-digit FIPS code (string) of the county or
#'   counties to subset on. This can also be a county name or vector of names.
#' @param unit Character string; one of \code{"miles"} (default),
#'   \code{"kilometers"}, or \code{"meters"}.
#' @param use_cache Logical; if \code{TRUE}, enables
#'   \code{options(tigris_use_cache = TRUE)}.
#' @param ... arguments passed on to tigris::tracts
#'
#' @export
#' @return A list with:
#' \describe{
#'   \item{loc_vec}{Character vector of tract GEOIDs (same order as matrix
#'   dimensions)}
#'   \item{distance_matrix}{Square numeric matrix of pairwise distances in
#'   requested units}
#' }
#'
#' @examples
#' md_dm <- tract_distance_matrix("MD")
#' dim(md_dm$distance_matrix)
#' md_dm_km <- tract_distance_matrix("MD", unit = "kilometers")
tract_distance_matrix <- function(
  st,
  county = NULL,
  unit = c("miles", "kilometers", "meters"),
  use_cache = TRUE,
  ...
) {
  .assert_tigris_available("tract_distance_matrix")

  if (!is.character(st) || length(st) != 1 || nchar(st) != 2) {
    cli::cli_abort(
      "{.arg st} must be a single 2-character state abbreviation (e.g., 'MD')."
    )
  }
  if (!grepl("^[A-Za-z]{2}$", st)) {
    cli::cli_abort("{.arg st} must contain exactly two letters.")
  }

  unit <- match.arg(unit)
  st <- toupper(st)
  tract_df <- tract_generator(
    st = st,
    county = county,
    use_cache = use_cache,
    ...
  )

  custom_distance_matrix(
    df = tract_df,
    unit = unit,
    label_var = "geoid",
    lat_var = "latitude",
    long_var = "longitude"
  )
}

#' Build a Distance Matrix from a Custom Data Frame
#'
#' Generates an all-pairs distance matrix from latitude/longitude coordinates
#' in a user-supplied data frame. Row and column names of the matrix are set
#' from a unique label variable.
#'
#' @param df A \code{data.frame} containing label and coordinate columns.
#' @param unit Character string; one of \code{"miles"} (default),
#'   \code{"kilometers"}, or \code{"meters"}.
#' @param label_var Character scalar; column name to use for matrix
#'   row/column names. Values in this column must be unique and non-missing.
#' @param lat_var Character scalar; column name containing latitude values.
#' @param long_var Character scalar; column name containing longitude values.
#'
#' @return A list with:
#' \describe{
#'   \item{loc_vec}{Character vector of location labels (same order as matrix
#'   dimensions)}
#'   \item{distance_matrix}{Square numeric matrix of pairwise distances in
#'   requested units}
#' }
#'
#' @export
#' @examples
#' md <- tract_generator("24")
#' dm <- custom_distance_matrix(
#'   md,
#'   label_var = "geoid", lat_var = "latitude", long_var = "longitude"
#' )
#' dim(dm[["distance_matrix"]])
#'
#' names(md) <- c("tract_id", "lat", "lon")
#' dm_km <- custom_distance_matrix(
#'   md,
#'   unit = "kilometers",
#'   label_var = "tract_id",
#'   lat_var = "lat",
#'   long_var = "lon"
#' )
custom_distance_matrix <- function(
  df,
  unit = c("miles", "kilometers", "meters"),
  label_var,
  lat_var,
  long_var
) {
  longitude <- latitude <- NULL

  coord_vars <- .resolve_coord_var_names(lat_var = lat_var, long_var = long_var)
  lat_var <- coord_vars$lat_var
  long_var <- coord_vars$long_var

  unit <- match.arg(unit)
  locs <- .validate_custom_locations(
    df = df,
    label_var = label_var,
    lat_var = lat_var,
    long_var = long_var
  )
  coords <- as.matrix(locs[, list(longitude, latitude)])

  .distance_result_from_coords(
    loc_vec = locs$location,
    coords = coords,
    unit = unit
  )
}
