.load_example_data <- function() {
  location <- example_count_data <- NULL
  data(
    "example_count_data",
    package = "gsClusterDetect",
    envir = environment()
  )
  cases <- data.table::as.data.table(example_count_data)
  cases[, location := as.character(location)]
  cases
}

.make_case_grids <- function() {
  cases <- .load_example_data()
  gsClusterDetect::generate_case_grids(
    cases = cases,
    detect_date = as.Date("2025-01-31"),
    baseline_length = 60,
    max_test_window_days = 7,
    guard_band = 0,
    baseline_adjustment = "add_one"
  )
}

testthat::test_that(
  "generate_case_grids returns CaseGrids with consistent totals",
  {
    cg <- .make_case_grids()

    testthat::expect_true("CaseGrids" %in% class(cg))
    testthat::expect_true(all(c(
      "baseline_counts_by_location",
      "case_grid",
      "case_grid_totals_by_date",
      "test_cases",
      "detect_date",
      "baseline_total"
    ) %in% names(cg)))
    testthat::expect_gt(nrow(cg[["baseline_counts_by_location"]]), 0)
    testthat::expect_gt(nrow(cg[["case_grid"]]), 0)
    testthat::expect_gt(nrow(cg[["case_grid_totals_by_date"]]), 0)
    testthat::expect_true(all(cg[["case_grid"]][["count_sum"]] > 0))
    testthat::expect_equal(
      cg[["baseline_total"]],
      sum(cg[["baseline_counts_by_location"]][["base_loc_sums"]], na.rm = TRUE)
    )
  }
)

testthat::test_that("generate_case_grids edge and error handling", {
  cases <- .load_example_data()

  cg_small <- gsClusterDetect::generate_case_grids(
    cases = cases,
    detect_date = as.Date("2025-01-31"),
    baseline_length = 1,
    max_test_window_days = 1,
    guard_band = 0,
    baseline_adjustment = "none"
  )
  testthat::expect_true("CaseGrids" %in% class(cg_small))
  testthat::expect_equal(length(unique(cg_small[["case_grid"]][["date"]])), 1)

  bad_cases <- data.table::copy(cases)
  bad_cases[, location := as.integer(location)]
  testthat::expect_error(
    gsClusterDetect::generate_case_grids(
      bad_cases,
      detect_date = as.Date("2025-01-31")
    ),
    "location column"
  )

  bad_date <- data.table::copy(cases)
  bad_date[, date := as.character(date)]
  testthat::expect_error(
    gsClusterDetect::generate_case_grids(
      bad_date,
      detect_date = as.Date("2025-01-31")
    ),
    "Date"
  )

  cg_add_test <- gsClusterDetect::generate_case_grids(
    as.data.frame(cases),
    detect_date = as.Date("2025-01-31"),
    baseline_length = 60,
    max_test_window_days = 7,
    baseline_adjustment = "add_test"
  )
  testthat::expect_true("CaseGrids" %in% class(cg_add_test))
})

testthat::test_that(
  paste(
    "gen_nearby_case_info returns NearbyClusterGrids",
    "with distance limit respected"
  ),
  {
    cg <- .make_case_grids()
    dm <- gsClusterDetect::county_distance_matrix(
      st = "OH",
      unit = "miles",
      source = "built_in"
    )[["distance_matrix"]]

    nci <- gsClusterDetect::gen_nearby_case_info(
      cg = cg,
      distance_matrix = dm,
      distance_limit = 50
    )

    testthat::expect_true("NearbyClusterGrids" %in% class(nci))
    testthat::expect_true(all(c("baseline", "test") %in% names(nci)))
    testthat::expect_gt(nrow(nci[["baseline"]]), 0)
    testthat::expect_gt(nrow(nci[["test"]]), 0)
    testthat::expect_true(all(nci[["baseline"]][["distance_value"]] <= 50))
    testthat::expect_true(all(nci[["test"]][["distance_value"]] <= 50))
  }
)

testthat::test_that("gen_nearby_case_info edge and error handling", {
  cg <- .make_case_grids()
  dm <- gsClusterDetect::county_distance_matrix(
    st = "OH",
    unit = "miles",
    source = "built_in"
  )[["distance_matrix"]]

  nci0 <- gsClusterDetect::gen_nearby_case_info(
    cg = cg,
    distance_matrix = dm,
    distance_limit = 0
  )
  testthat::expect_true(all(nci0[["baseline"]][["distance_value"]] == 0))

  testthat::expect_error(
    gsClusterDetect::gen_nearby_case_info(
      cg = list(),
      distance_matrix = dm,
      distance_limit = 50
    ),
    "CaseGrids"
  )

  dist_list <- gsClusterDetect::create_dist_list(
    level = "county",
    threshold = 50,
    st = "OH",
    unit = "miles"
  )
  nci_list <- gsClusterDetect::gen_nearby_case_info(
    cg = cg,
    distance_matrix = dist_list,
    distance_limit = 50
  )
  testthat::expect_true("NearbyClusterGrids" %in% class(nci_list))
})

testthat::test_that(
  "generate_observed_expected returns ObservedExpectedGrid and stable fields",
  {
    cg <- .make_case_grids()
    dm <- gsClusterDetect::county_distance_matrix(
      st = "OH",
      unit = "miles",
      source = "built_in"
    )[["distance_matrix"]]
    nci <- gsClusterDetect::gen_nearby_case_info(
      cg, dm,
      distance_limit = 50
    )

    oe <- gsClusterDetect::generate_observed_expected(
      nearby_counts = nci,
      case_grid = cg,
      adjust = TRUE
    )

    testthat::expect_true("ObservedExpectedGrid" %in% class(oe))
    testthat::expect_gt(nrow(oe), 0)
    testthat::expect_equal(nrow(oe), nrow(nci[["test"]]))
    testthat::expect_true(all(is.finite(oe[["log_obs_exp"]])))
    testthat::expect_true(all(oe[["expected"]] >= 0))
    testthat::expect_true(all(oe[["detect_date"]] == cg[["detect_date"]]))
    testthat::expect_true(all(oe[["baseline_total"]] == cg[["baseline_total"]]))
  }
)

testthat::test_that("generate_observed_expected validates class requirements", {
  cg <- .make_case_grids()
  dm <- gsClusterDetect::county_distance_matrix(
    st = "OH",
    unit = "miles",
    source = "built_in"
  )[["distance_matrix"]]
  nci <- gsClusterDetect::gen_nearby_case_info(
    cg, dm,
    distance_limit = 50
  )

  testthat::expect_error(
    gsClusterDetect::generate_observed_expected(
      nearby_counts = list(),
      case_grid = cg
    ),
    "NearbyClusterGrids"
  )
  testthat::expect_error(
    gsClusterDetect::generate_observed_expected(
      nearby_counts = nci,
      case_grid = list()
    ),
    "CaseGrids"
  )

  oe_no_adjust <- gsClusterDetect::generate_observed_expected(
    nearby_counts = nci,
    case_grid = cg,
    adjust = FALSE
  )
  testthat::expect_true("ObservedExpectedGrid" %in% class(oe_no_adjust))
})

testthat::test_that("add_spline_threshold returns only positive alert gaps", {
  cg <- .make_case_grids()
  dm <- gsClusterDetect::county_distance_matrix(
    st = "OH",
    unit = "miles",
    source = "built_in"
  )[["distance_matrix"]]
  nci <- gsClusterDetect::gen_nearby_case_info(
    cg, dm,
    distance_limit = 50
  )
  oe <- gsClusterDetect::generate_observed_expected(
    nci, cg,
    adjust = TRUE
  )

  alerts <- gsClusterDetect::add_spline_threshold(
    oe,
    spline_lookup = "01"
  )

  testthat::expect_true("ClusterAlertTable" %in% class(alerts))
  testthat::expect_true(
    all(c("alert_ratio", "alert_gap", "spl_thresh") %in% names(alerts))
  )
  testthat::expect_true(all(alerts[["alert_gap"]] > 0))
  testthat::expect_true(all(alerts[["target"]] %in% unique(oe[["target"]])))
})

testthat::test_that(
  "add_spline_threshold validates lookup and oe_grid inputs",
  {
    cg <- .make_case_grids()
    dm <- gsClusterDetect::county_distance_matrix(
      st = "OH",
      unit = "miles",
      source = "built_in"
    )[["distance_matrix"]]
    nci <- gsClusterDetect::gen_nearby_case_info(
      cg, dm,
      distance_limit = 50
    )
    oe <- gsClusterDetect::generate_observed_expected(
      nci, cg,
      adjust = TRUE
    )

    testthat::expect_error(
      gsClusterDetect::add_spline_threshold(oe, spline_lookup = "bad"),
      "built-in spline"
    )
    testthat::expect_error(
      gsClusterDetect::add_spline_threshold(
        oe,
        spline_lookup = data.frame(observed = 1:3)
      ),
      "spl_thresh"
    )
    testthat::expect_error(
      gsClusterDetect::add_spline_threshold(
        data.table::data.table(x = 1),
        spline_lookup = "01"
      ),
      "ObservedExpectedGrid"
    )
    default_alerts <- gsClusterDetect::add_spline_threshold(
      oe,
      spline_lookup = NULL
    )
    testthat::expect_true("ClusterAlertTable" %in% class(default_alerts))
    testthat::expect_error(
      gsClusterDetect::add_spline_threshold(oe, spline_lookup = 10),
      "must be NULL"
    )
  }
)
