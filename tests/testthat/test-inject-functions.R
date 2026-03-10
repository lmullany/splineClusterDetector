# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
testthat::test_that(
  "get_nearby_locations returns sorted neighbors within radius",
  {
    dm <- splineClusterDetector::county_distance_matrix(
      st = "OH",
      unit = "miles",
      source = "built_in"
    )[["distance_matrix"]]
    center <- rownames(dm)[1]

    nearby <- splineClusterDetector::get_nearby_locations(
      center_location = center,
      distance_matrix = dm,
      radius_miles = 50
    )

    testthat::expect_s3_class(nearby, "data.table")
    testthat::expect_true(
      all(c("nearby_locs", "distance_mi") %in% names(nearby))
    )
    testthat::expect_gt(nrow(nearby), 0)
    testthat::expect_true(all(diff(nearby[["distance_mi"]]) >= 0))
    testthat::expect_true(all(nearby[["distance_mi"]] <= 50))
    testthat::expect_true(center %in% nearby[["nearby_locs"]])
    testthat::expect_equal(nearby[nearby_locs == center, distance_mi][1], 0)
  }
)

testthat::test_that("get_nearby_locations handles edge and invalid center", {
  dm <- splineClusterDetector::county_distance_matrix(
    st = "OH",
    unit = "miles",
    source = "built_in"
  )[["distance_matrix"]]
  center <- rownames(dm)[1]

  nearby0 <- splineClusterDetector::get_nearby_locations(
    center_location = center,
    distance_matrix = dm,
    radius_miles = 0
  )
  testthat::expect_true(center %in% nearby0[["nearby_locs"]])
  testthat::expect_true(all(nearby0[["distance_mi"]] == 0))

  testthat::expect_error(
    splineClusterDetector::get_nearby_locations(
      center_location = "not-a-location",
      distance_matrix = dm,
      radius_miles = 10
    )
  )
})

testthat::test_that(
  "st_injects adds exactly requested injections and valid dates",
  {
    if (!requireNamespace("withr", quietly = TRUE)) {
      testthat::skip("withr not available")
    }

    dm <- splineClusterDetector::county_distance_matrix(
      st = "OH",
      unit = "miles",
      source = "built_in"
    )[["distance_matrix"]]
    center <- rownames(dm)[1]
    locs <- rownames(dm)[1:8]
    dates <- seq.Date(as.Date("2025-01-01"), by = "day", length.out = 20)

    cases <- data.table::CJ(location = locs, date = dates)
    cases[, count := 2L]

    withr::local_seed(42)
    out <- splineClusterDetector::st_injects(
      cases = cases,
      distance_matrix = dm,
      target_loc = center,
      center_decile = 1,
      radius_miles = 100,
      nr_cases = 25,
      nr_days = 5,
      end_date = as.Date("2025-01-20")
    )

    testthat::expect_type(out, "list")
    testthat::expect_true(
      all(c("case_counts_inj", "inject_tbl") %in% names(out))
    )
    testthat::expect_s3_class(out[["case_counts_inj"]], "data.table")
    testthat::expect_s3_class(out[["inject_tbl"]], "data.table")
    testthat::expect_equal(sum(out[["inject_tbl"]][["count"]]), 25)
    testthat::expect_equal(
      sum(out[["case_counts_inj"]][["count"]]),
      sum(cases[["count"]]) + 25
    )
    testthat::expect_gte(
      min(out[["inject_tbl"]][["date"]]),
      as.Date("2025-01-16")
    )
    testthat::expect_lte(
      max(out[["inject_tbl"]][["date"]]),
      as.Date("2025-01-20")
    )

    nearby <- splineClusterDetector::get_nearby_locations(
      center,
      dm,
      radius_miles = 100
    )
    testthat::expect_true(
      all(out[["inject_tbl"]][["location"]] %in% nearby[["nearby_locs"]])
    )
  }
)

testthat::test_that("st_injects edge and error handling", {
  dm <- splineClusterDetector::county_distance_matrix(
    st = "OH",
    unit = "miles",
    source = "built_in"
  )[["distance_matrix"]]
  center <- rownames(dm)[1]
  locs <- rownames(dm)[1:3]
  dates <- seq.Date(as.Date("2025-01-01"), by = "day", length.out = 5)

  base_cases <- data.table::CJ(location = locs, date = dates)
  base_cases[, count := 1L]

  out0 <- splineClusterDetector::st_injects(
    cases = data.table::copy(base_cases),
    distance_matrix = dm,
    target_loc = center,
    center_decile = 1,
    radius_miles = 50,
    nr_cases = 0,
    nr_days = 2,
    end_date = as.Date("2025-01-05")
  )
  testthat::expect_equal(sum(out0[["inject_tbl"]][["count"]]), 0)
  testthat::expect_equal(
    sum(out0[["case_counts_inj"]][["count"]]),
    sum(base_cases[["count"]])
  )

  if (!requireNamespace("withr", quietly = TRUE)) {
    testthat::skip("withr not available")
  }
  withr::local_seed(123)
  out_rand <- splineClusterDetector::st_injects(
    cases = data.table::copy(base_cases),
    distance_matrix = dm,
    target_loc = NULL,
    center_decile = 1,
    radius_miles = 50,
    nr_cases = 3,
    nr_days = 2,
    end_date = as.Date("2025-01-05")
  )
  testthat::expect_equal(sum(out_rand[["inject_tbl"]][["count"]]), 3)

  bad_cases <- data.table::copy(base_cases)[, .(location, date)]
  testthat::expect_error(
    splineClusterDetector::st_injects(
      cases = bad_cases,
      distance_matrix = dm,
      target_loc = center,
      center_decile = 1,
      radius_miles = 50,
      nr_cases = 5,
      nr_days = 2,
      end_date = as.Date("2025-01-05")
    ),
    "required columns"
  )

  bad_date_cases <- data.table::copy(base_cases)
  bad_date_cases[, date := as.character(date)]
  testthat::expect_error(
    splineClusterDetector::st_injects(
      cases = bad_date_cases,
      distance_matrix = dm,
      target_loc = center,
      center_decile = 1,
      radius_miles = 50,
      nr_cases = 5,
      nr_days = 2,
      end_date = as.Date("2025-01-05")
    ),
    "Date"
  )
})
