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

.make_example_inputs <- function() {
  cases <- .load_example_data()
  dm <- gsClusterDetect::county_distance_matrix(
    "OH",
    source = "built_in"
  )[["distance_matrix"]]
  list(cases = cases, dm = dm)
}

testthat::test_that(
  "get_test_dates returns contiguous dates ending at end_date",
  {
    td <- gsClusterDetect::get_test_dates(
      end_date = as.Date("2025-01-31"),
      test_length = 7
    )

    testthat::expect_length(td, 7)
    testthat::expect_equal(td[[length(td)]], as.Date("2025-01-31"))
    testthat::expect_true(all(diff(td) == 1))

    testthat::expect_error(
      gsClusterDetect::get_test_dates(
        end_date = as.Date("2025-01-31"),
        test_length = 0
      )
    )
  }
)

testthat::test_that(
  "get_baseline_dates returns expected length and boundaries",
  {
    bd <- gsClusterDetect::get_baseline_dates(
      end_date = as.Date("2025-01-31"),
      test_length = 7,
      baseline_length = 60,
      guard = 2
    )

    testthat::expect_length(bd, 60)
    testthat::expect_true(all(diff(bd) == 1))
    testthat::expect_equal(max(bd), as.Date("2025-01-22"))
    testthat::expect_equal(min(bd), as.Date("2024-11-24"))
  }
)

testthat::test_that("find_clusters smoke test returns cluster object", {
  x <- .make_example_inputs()
  out <- gsClusterDetect::find_clusters(
    cases = x[["cases"]],
    distance_matrix = x[["dm"]],
    detect_date = as.Date("2025-01-31"),
    spline_lookup = "01",
    baseline_length = 60,
    max_test_window_days = 7,
    distance_limit = 50,
    baseline_adjustment = "add_one",
    use_fast = TRUE
  )

  testthat::expect_true("clusters" %in% class(out))
  testthat::expect_true(
    all(c("cluster_alert_table", "cluster_location_counts") %in% names(out))
  )
  testthat::expect_gt(nrow(out[["cluster_alert_table"]]), 0)
  testthat::expect_gt(nrow(out[["cluster_location_counts"]]), 0)
  testthat::expect_true(
    all(out[["cluster_alert_table"]][["detect_date"]] == as.Date("2025-01-31"))
  )
  testthat::expect_setequal(
    unique(out[["cluster_alert_table"]][["target"]]),
    unique(out[["cluster_location_counts"]][["target"]])
  )
})

testthat::test_that("find_clusters edge and error behavior", {
  x <- .make_example_inputs()

  testthat::expect_error(
    gsClusterDetect::find_clusters(
      cases = x[["cases"]],
      distance_matrix = x[["dm"]],
      detect_date = as.Date("2025-01-31"),
      max_test_window_days = 0
    ),
    "must be positive"
  )

  testthat::expect_error(
    gsClusterDetect::find_clusters(
      cases = x[["cases"]],
      distance_matrix = x[["dm"]],
      detect_date = as.Date("2025-01-31"),
      baseline_adjustment = "bad-option"
    ),
    "arg"
  )

  no_clusters <- gsClusterDetect::find_clusters(
    cases = x[["cases"]],
    distance_matrix = x[["dm"]],
    detect_date = as.Date("2025-01-31"),
    spline_lookup = "01",
    baseline_length = 60,
    max_test_window_days = 7,
    distance_limit = 50,
    min_clust_cases = 1e9,
    use_fast = TRUE
  )
  testthat::expect_null(no_clusters)

  bad_cases <- data.table::copy(x[["cases"]])[, .(location, date)]
  testthat::expect_error(
    gsClusterDetect::find_clusters(
      cases = bad_cases,
      distance_matrix = x[["dm"]],
      detect_date = as.Date("2025-01-31")
    ),
    "case_grid_info"
  )

  interim_err <- gsClusterDetect::find_clusters(
    cases = bad_cases,
    distance_matrix = x[["dm"]],
    detect_date = as.Date("2025-01-31"),
    return_interim = TRUE
  )
  testthat::expect_type(interim_err, "list")
  testthat::expect_true(!is.null(attr(interim_err, "error")))
  testthat::expect_equal(attr(interim_err, "error")$step, "case_grid_info")

  out_slow <- gsClusterDetect::find_clusters(
    cases = x[["cases"]],
    distance_matrix = x[["dm"]],
    detect_date = as.Date("2025-01-31"),
    spline_lookup = "01",
    baseline_length = 60,
    max_test_window_days = 7,
    distance_limit = 50,
    baseline_adjustment = "add_one",
    use_fast = FALSE,
    return_interim = TRUE
  )
  testthat::expect_type(out_slow, "list")
  if (!is.null(attr(out_slow, "error"))) {
    testthat::expect_true(
      attr(out_slow, "error")$step %in%
        c("compress_clusters", "adding_location_counts")
    )
  } else {
    testthat::expect_true("result" %in% names(out_slow))
  }
})
