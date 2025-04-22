# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

#' Add data counts for parameterized injected clusters
#'
#' Function st_injects returns a list of two objects 1. a full dataset as a
#' data.table with inject counts added according to design parameters. 2. a
#' table of only the inject counts, locations, and dates.
#' @param cases data frame of cases
#' @param distance_matrix a distance matrix
#' @param target_loc a location into which the injection should occur
#' @param center_decile an integer value between 1 and 10, inclusive
#' @param radius_miles a numeric value >0
#' @param nr_cases number of cases to inject
#' @param nr_days number of days over which we want to inject cases
#' @param end_date last date of injection
#' @export
#' @returns a two-element list; each element is a dataframe. The first is the
#'   full dataset with injected cases and the second is the injected cases only
#' @examples
#' \dontrun{
#' scen1 <- st_injects(
#'   cases, distance_matrix, target_loc, center_decile,
#'   radius_miles, nr_cases, nr_days, end_date
#' )
#' }
st_injects <- function(
    cases,
    distance_matrix,
    target_loc,
    center_decile,
    radius_miles,
    nr_cases,
    nr_days,
    end_date) {
  # check that cases has location, count, and date
  check_vars(cases, required = c("location", "count", "date"))

  # check that date column is of class Date
  if (!"Date" %in% class(cases[["date"]])) {
    cli::cli_abort("date column must be of class Date")
  }

  # make sure that cases is a data.table
  data.table::setDT(cases)


  count <- location <- loc_sum <- count_x <- count_y <- NULL

  # The center location 'center_loc' is either the 'target_loc' argument or, if
  # target_loc is NULL, selected randomly from chosen decile of case counts

  case_counts_by_loc <- cases[, list(loc_sum = sum(count)), by = location] |>
    _[order(location)]

  case_counts_by_loc[order(loc_sum)]

  if (is.null(target_loc)) {
    nr_locs <- nrow(case_counts_by_loc)
    decile_bounds <- as.integer(
      c(
        1,
        floor(
          stats::quantile(
            1:nr_locs,
            seq(from = 0.1, to = 0.9, by = 0.1)
          )
        ),
        nr_locs
      )
    )


    center_loc <- case_counts_by_loc$location[sample(seq(
      from = decile_bounds[center_decile],
      to = decile_bounds[center_decile + 1]
    ), 1)]
  } else {
    center_loc <- target_loc
  }
  # Collect the locations within radius of the center location
  nearby_locs <- get_nearby_locations(center_loc, distance_matrix, radius_miles)

  # Get case counts for nearby locations
  nearby_counts_by_loc <- case_counts_by_loc[
    location %in% nearby_locs$nearby_locs
  ]

  nr_inject_locs <- nrow(nearby_counts_by_loc)

  # Choose number of cases to inject for each day
  nr_injects_by_day <- stats::rmultinom(
    1,
    size = nr_cases,
    prob = rep(1, nr_days)
  )

  # Choose random set of locations and days (1 to nr_days), with replacement,
  # based on probabilities
  inject_tbl <- data.table::as.data.table(
    matrix(0, nrow = nr_days * nr_inject_locs, ncol = 2)
  ) |>
    data.table::setnames(c("location", "count"))

  inject_tbl[, location := as.character(location)]

  date_vec <- rep("", nr_days * nr_inject_locs)

  for (kDay in 1:nr_days) {
    if (nr_injects_by_day[kDay] > 0) {
      inject_vec <- stats::rmultinom(
        1,
        size = nr_injects_by_day[kDay],
        prob = nearby_counts_by_loc$loc_sum
      )
      inject_tbl[(kDay - 1) * nr_inject_locs + 1:nr_inject_locs, 1] <-
        nearby_counts_by_loc$location
      inject_tbl[(kDay - 1) * nr_inject_locs + 1:nr_inject_locs, 2] <-
        inject_vec
    }

    date_vec[(kDay - 1) * nr_inject_locs + 1:nr_inject_locs] <- as.character(
      as.Date(end_date) - nr_days + kDay
    )
  }

  inject_tbl$date <- as.Date(date_vec)
  inject_tbl[, location := as.character(location)]
  inject_tbl <- inject_tbl[count > 0, ]

  # Reformat dates for output files

  # Merge old case table with inject table, make columnn of original + injected
  # counts
  case_counts_inj <- data.table::merge.data.table(
    cases,
    inject_tbl,
    all = TRUE,
    suffixes = c("_x", "_y"),
    by = c("location", "date")
  )



  case_counts_inj[is.na(case_counts_inj)] <- 0
  case_counts_inj[, count := count_x + count_y]
  case_counts_inj[, c("count_x", "count_y") := NULL]

  # Format and write output files
  data.table::setcolorder(case_counts_inj, c("location", "count", "date"))
  case_counts_inj[, date := as.Date(date)]
  case_counts_inj[, location := as.character(location)]

  list(case_counts_inj = case_counts_inj[], inject_tbl = inject_tbl)
}

#' Add data counts for injected clusters
#'
#' Returns a 2-column table of locations near center location and associated
#' case counts
#' @param center_location location
#' @param distance_matrix a distance matrix
#' @param radius_miles a numeric value >0
#' @export
#' @returns a data.table
#' @examples
#' dm <- zip_distance_matrix("MD")$distance_matrix
#' nearby_locations <- get_nearby_locations("21228", dm, 10)
get_nearby_locations <- function(
    center_location,
    distance_matrix,
    radius_miles) {
  r <- distance_matrix[center_location, ] |> sort()
  ndx <- which(r <= radius_miles)
  data.table::data.table(nearby_locs = names(ndx), distance_mi = r[ndx])
}
