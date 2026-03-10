# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

#' Function returns the number of meters in unit (one of miles,
#' kilometers, or meters)
#' @param unit Character scalar specifying distance unit; one of
#'   \code{"miles"}, \code{"kilometers"}, or \code{"meters"}.
#' @return Numeric scalar conversion factor from the selected unit to meters.
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
#' @return Invisibly returns \code{NULL}; otherwise throws an error when
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
      class="tigris_not_available"
    )
  }
}

#' Helper function resolves coordinate variable names
#' @param lat_var Character scalar latitude column name or \code{NULL} to use
#'   \code{"latitude"}.
#' @param long_var Character scalar longitude column name or \code{NULL} to use
#'   \code{"longitude"}.
#' @return A named list with elements \code{lat_var} and \code{long_var}.
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

#' Helper funtion: given a data frame, and strings for label_var,
#' lat_var, and long_var, the df is checked for
#' @param df A \code{data.frame} containing label and coordinate columns.
#' @param label_var Character scalar naming the label column.
#' @param lat_var Character scalar naming the latitude column.
#' @param long_var Character scalar naming the longitude column.
#' @return A \code{data.table} with standardized columns
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

#' Helper function reduces a data fram to only those where latitude
#' and longitude are not missing
#' @param locs A \code{data.table} with \code{latitude} and \code{longitude}
#'   columns.
#' @return A \code{data.table} filtered to non-missing latitude rows with
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
#' pairs of coordinates. Note that ccords must be a matrix
#' or frame, where the first col is longitude and the second
#' column is latitude
#' @param coords Matrix-like object with longitude in column 1 and latitude in
#'   column 2.
#' @return Numeric square matrix of pairwise distances in meters.
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
#' @return A list with elements \code{loc_vec} and \code{distance_matrix}.
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
#' @return Named list of numeric vectors of neighbor distances, keyed by
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
#' @examples
#' # example code
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
#' @param source string indicating either "rnssp" (default) or "built_in".
#'   If "rnssp" is requested and \pkg{Rnssp} is available, the function uses
#'   \code{Rnssp::county_sf} with \code{sf::st_centroid()} and
#'   \code{sf::st_distance()}. If \pkg{Rnssp} is not available, the function
#'   warns and falls back to this package's default \code{counties} dataset.
#' @export
#' @examples
#' # example code
#' county_distance_matrix("MD", source = "built_in")
#' county_distance_matrix("CT", "kilometers", source = "rnssp")
county_distance_matrix <- function(
  st,
  unit = c("miles", "kilometers", "meters"),
  source = c("rnssp", "built_in")
) {
  # if State = "US" pass this request on to us_distance_matrix()
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

    if (source == "rnssp" && !requireNamespace("Rnssp", quietly = TRUE)) {
      cli::cli_warn(
        c(
          "{.pkg Rnssp} is not installed.",
          "i" = "Falling back to the built-in {.arg counties} dataset."
        )
      )
      source <- "built_in"
    }

    if (source == "rnssp") {
      # look up the state fips code for this two letter code
      st <- Rnssp::state_sf[Rnssp::state_sf$STUSPS == toupper(st), ]$STATEFP |>
        as.character()

      county_sf <- Rnssp::county_sf[Rnssp::county_sf$STATEFP == st, ]
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
#' @param level string either "county" (default), "zip", or "tract"
#' @param threshold numeric value; include in each location-specific named
#'   vector only those locations that a within `threshold` distance units of the
#'   target. The defaults is 50 (miles), 15 (miles) and 3 (miles) for county,
#'   zip, and tract, respectively, but these can be adjusted. Note if a
#'   different unit other than miles is used, then the user should also adjust
#'   this parameter appropriately
#' @param st string; optional to specify a state; if NULL distances are returned
#'   for all zip codes or counties in the US
#' @param unit string one of miles (default), kilometers, or meters; this is the
#'   unit relevant to the threshold
#' @export
create_dist_list <- function(
  level = c("county", "zip", "tract"),
  threshold = {
    data.table::fcase(
      level == "county", 50,
      level == "zip", 15,
      level == "tract", 3
    )
  },
  st = NULL,
  unit = c("miles", "kilometers", "meters")
) {
  state <- zip_code <- latitude <- longitude <- fips <- NULL

  level <- match.arg(level)
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
    tracts <- tract_generator(st = st)
    return(
      create_custom_dist_list(
        df = tracts,
        label_var = "geoid",
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
#' This function is a custom-data version of \code{create_dist_list()}.
#' It returns a list of named numeric vectors where each list element contains
#' only locations within \code{threshold} distance units of a target location.
#'
#' @param df data.frame containing label and coordinate columns
#' @param label_var character scalar; column name used as location label
#'   (must be unique and non-missing)
#' @param lat_var character scalar; latitude column name. If \code{NULL},
#'   defaults to \code{"latitude"}
#' @param long_var character scalar; longitude column name. If \code{NULL},
#'   defaults to \code{"longitude"}
#' @param threshold numeric scalar distance cutoff in units of \code{unit}
#' @param unit string, one of "miles" (default), "kilometers", or "meters"
#' @export
#' @examples
#' \dontrun{
#' md <- tract_generator("MD")
#' dlist <- create_custom_dist_list(
#'   df = md,
#'   label_var = "geoid",
#'   threshold = 15,
#'   unit = "miles"
#' )
#' }
create_custom_dist_list <- function(
  df,
  label_var,
  lat_var = NULL,
  long_var = NULL,
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
#' @param st Character scalar; either a 2-digit state FIPS code
#'   (for example, \code{"24"}) or a 2-letter USPS abbreviation
#'   (for example, \code{"MD"}).
#' @param year Integer TIGER/Line year to request from \pkg{tigris}.
#'   Default is \code{2024}.
#' @param cb Logical; passed to \code{tigris::tracts()}. Default is \code{TRUE}.
#' @param use_cache Logical; if \code{TRUE}, enables
#'   \code{options(tigris_use_cache = TRUE)}.
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
#' \dontrun{
#' md_tracts <- tract_generator("24")
#' md_tracts2 <- tract_generator("MD")
#' head(md_tracts)
#' }
tract_generator <- function(
  st,
  year = 2024,
  cb = TRUE,
  use_cache = TRUE
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

  tracts_sf <- tigris::tracts(state = st, cb = cb, year = year)
  centroids_sf <- sf::st_transform(sf::st_centroid(tracts_sf), 4326)
  coords <- sf::st_coordinates(centroids_sf)

  data.table::data.table(
    geoid = centroids_sf$GEOID,
    latitude = coords[, "Y"],
    longitude = coords[, "X"]
  )
}

#' Build a Tract Distance Matrix for a State
#'
#' Creates an all-pairs distance matrix between census tract centroids for a
#' state, using state abbreviation input similar to
#' \code{zip_distance_matrix()}.
#'
#' @param st Character scalar; 2-character USPS state abbreviation
#'   (for example, \code{"MD"}).
#' @param unit Character string; one of \code{"miles"} (default),
#'   \code{"kilometers"}, or \code{"meters"}.
#' @param year Integer TIGER/Line year to request from \pkg{tigris}.
#'   Default is \code{2024}.
#' @param cb Logical; passed to \code{tigris::tracts()}. Default is \code{TRUE}.
#' @param use_cache Logical; if \code{TRUE}, enables
#'   \code{options(tigris_use_cache = TRUE)}.
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
#' \dontrun{
#' md_dm <- tract_distance_matrix("MD")
#' dim(md_dm$distance_matrix)
#' md_dm_km <- tract_distance_matrix("MD", unit = "kilometers")
#' }
tract_distance_matrix <- function(
  st,
  unit = c("miles", "kilometers", "meters"),
  year = 2024,
  cb = TRUE,
  use_cache = TRUE
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
    year = year,
    cb = cb,
    use_cache = use_cache
  )

  custom_distance_matrix(
    df = tract_df,
    unit = unit,
    label_var = "geoid"
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
#'   If \code{NULL} (default), \code{"latitude"} is used.
#' @param long_var Character scalar; column name containing longitude values.
#'   If \code{NULL} (default), \code{"longitude"} is used.
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
#' \dontrun{
#' md <- tract_generator("24")
#' dm <- custom_distance_matrix(md, label_var = "geoid")
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
#' }
custom_distance_matrix <- function(
  df,
  unit = c("miles", "kilometers", "meters"),
  label_var,
  lat_var = NULL,
  long_var = NULL
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
