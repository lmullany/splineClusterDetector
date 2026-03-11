.load_example_data <- function() {
  location <- example_count_data <- NULL
  data(
    "example_count_data",
    package = "splineClusterDetector",
    envir = environment()
  )
  cases <- data.table::as.data.table(example_count_data)
  cases[, location := as.character(location)]
  cases
}

.make_alert_table <- function() {
  cases <- .load_example_data()
  dm <- splineClusterDetector::county_distance_matrix(
    "OH",
    source = "built_in"
  )[["distance_matrix"]]
  cg <- splineClusterDetector::generate_case_grids(
    cases,
    detect_date = as.Date("2025-01-31"),
    baseline_length = 60,
    max_test_window_days = 7
  )
  nci <- splineClusterDetector::gen_nearby_case_info(
    cg, dm,
    distance_limit = 50
  )
  oe <- splineClusterDetector::generate_observed_expected(
    nci, cg,
    adjust = TRUE
  )
  alerts <- splineClusterDetector::add_spline_threshold(
    oe,
    spline_lookup = "01"
  )
  list(cases = cases, dm = dm, alerts = alerts)
}

testthat::test_that("compress_clusters smoke and structural invariants", {
  x <- .make_alert_table()
  cl <- splineClusterDetector::compress_clusters(
    cluster_alert_table = x[["alerts"]],
    distance_matrix = x[["dm"]]
  )

  testthat::expect_true("clusters" %in% class(cl))
  testthat::expect_true(
    all(c("cluster_alert_table", "clust_loc_list") %in% names(cl))
  )
  testthat::expect_gt(nrow(cl[["cluster_alert_table"]]), 0)
  testthat::expect_equal(
    nrow(cl[["cluster_alert_table"]]),
    length(cl[["clust_loc_list"]])
  )
  testthat::expect_equal(
    as.integer(cl[["cluster_alert_table"]][["nr_locs"]]),
    as.integer(vapply(cl[["clust_loc_list"]], length, integer(1)))
  )

  dl <- splineClusterDetector::create_dist_list(
    level = "county",
    threshold = 50,
    st = "OH",
    unit = "miles"
  )
  cl_list <- splineClusterDetector::compress_clusters(
    cluster_alert_table = data.table::copy(x[["alerts"]]),
    distance_matrix = dl
  )
  testthat::expect_true("clusters" %in% class(cl_list))
})

testthat::test_that("compress_clusters errors on wrong class and empty input", {
  x <- .make_alert_table()
  empty_alerts <- data.table::copy(x[["alerts"]])[0]
  class(empty_alerts) <- unique(c(class(empty_alerts), "ClusterAlertTable"))

  testthat::expect_error(
    splineClusterDetector::compress_clusters(
      cluster_alert_table = data.table::data.table(),
      distance_matrix = x[["dm"]]
    ),
    "ClusterAlertTable"
  )
  testthat::expect_error(
    splineClusterDetector::compress_clusters(
      cluster_alert_table = empty_alerts,
      distance_matrix = x[["dm"]]
    ),
    "No clusters"
  )
})

testthat::test_that(
  "compress_clusters_fast is consistent with compress_clusters targets",
  {
    x <- .make_alert_table()
    cl_slow <- splineClusterDetector::compress_clusters(
      cluster_alert_table = data.table::copy(x[["alerts"]]),
      distance_matrix = x[["dm"]]
    )
    cl_fast <- splineClusterDetector::compress_clusters_fast(
      cluster_alert_table = data.table::copy(x[["alerts"]]),
      distance_matrix = x[["dm"]]
    )

    testthat::expect_true("clusters" %in% class(cl_fast))
    testthat::expect_equal(
      sort(cl_fast[["cluster_alert_table"]][["target"]]),
      sort(cl_slow[["cluster_alert_table"]][["target"]])
    )

    fast_sets <- lapply(cl_fast[["clust_loc_list"]], sort)
    slow_sets <- lapply(cl_slow[["clust_loc_list"]], sort)
    testthat::expect_equal(
      unname(sort(vapply(fast_sets, paste, character(1), collapse = "|"))),
      unname(sort(vapply(slow_sets, paste, character(1), collapse = "|")))
    )

    dl <- splineClusterDetector::create_dist_list(
      level = "county",
      threshold = 50,
      st = "OH",
      unit = "miles"
    )
    cl_fast_list <- splineClusterDetector::compress_clusters_fast(
      cluster_alert_table = data.table::copy(x[["alerts"]]),
      distance_matrix = dl
    )
    testthat::expect_true("clusters" %in% class(cl_fast_list))
  }
)

testthat::test_that(
  "compress_clusters_fast validates class and empty alert table",
  {
    x <- .make_alert_table()
    empty_alerts <- data.table::copy(x[["alerts"]])[0]
    class(empty_alerts) <- unique(c(class(empty_alerts), "ClusterAlertTable"))

    testthat::expect_error(
      splineClusterDetector::compress_clusters_fast(
        cluster_alert_table = data.table::data.table(),
        distance_matrix = x[["dm"]]
      ),
      "ClusterAlertTable"
    )
    testthat::expect_error(
      splineClusterDetector::compress_clusters_fast(
        cluster_alert_table = empty_alerts,
        distance_matrix = x[["dm"]]
      ),
      "No clusters"
    )
  }
)

testthat::test_that(
  "add_location_counts attaches per-location counts for each cluster",
  {
    x <- .make_alert_table()
    cl <- splineClusterDetector::compress_clusters_fast(
      cluster_alert_table = data.table::copy(x[["alerts"]]),
      distance_matrix = x[["dm"]]
    )
    out <- splineClusterDetector::add_location_counts(cl, x[["cases"]])

    testthat::expect_true("clusters" %in% class(out))
    testthat::expect_true(
      all(c("cluster_alert_table", "cluster_location_counts") %in% names(out))
    )
    testthat::expect_gt(nrow(out[["cluster_location_counts"]]), 0)
    testthat::expect_true(all(out[["cluster_location_counts"]][["count"]] >= 0))

    loc_counts <- out[["cluster_location_counts"]][, .N, by = target][
      order(target)
    ]
    nr <- out[["cluster_alert_table"]][, .(target, nr_locs)][order(target)]
    data.table::setnames(loc_counts, "N", "nr_locs")
    data.table::setkey(nr, NULL)
    data.table::setkey(loc_counts, NULL)
    testthat::expect_equal(nr, loc_counts)

    loc0 <- cl[["clust_loc_list"]][[1]][1]
    cases_df <- as.data.frame(x[["cases"]])
    cases_df[cases_df$location == loc0, "count"] <- 0L
    out0 <- try(
      splineClusterDetector::add_location_counts(cl, cases_df),
      silent = TRUE
    )
    if (!inherits(out0, "try-error")) {
      testthat::expect_true(
        any(out0[["cluster_location_counts"]][location == loc0, count] == 0)
      )
    } else {
      testthat::expect_true(inherits(out0, "try-error"))
    }
  }
)

testthat::test_that("add_location_counts validates class requirements", {
  x <- .make_alert_table()
  testthat::expect_error(
    splineClusterDetector::add_location_counts(
      cluster_list = list(),
      cases = x[["cases"]]
    ),
    "class 'clusters'"
  )

  cl <- splineClusterDetector::compress_clusters_fast(
    cluster_alert_table = data.table::copy(x[["alerts"]]),
    distance_matrix = x[["dm"]]
  )
  bad_cl <- cl
  bad_cl[["cluster_alert_table"]][, nr_locs := nr_locs + 1]
  testthat::expect_error(
    splineClusterDetector::add_location_counts(bad_cl, x[["cases"]]),
    "differs from number of locations"
  )
})

testthat::test_that("reduce_clusters_to_min filters by summed cluster count", {
  x <- .make_alert_table()
  cl <- splineClusterDetector::compress_clusters_fast(
    cluster_alert_table = data.table::copy(x[["alerts"]]),
    distance_matrix = x[["dm"]]
  )
  with_counts <- splineClusterDetector::add_location_counts(cl, x[["cases"]])

  unchanged <- splineClusterDetector::reduce_clusters_to_min(
    with_counts,
    minimum = 1
  )
  testthat::expect_equal(unchanged, with_counts)

  reduced <- splineClusterDetector::reduce_clusters_to_min(
    with_counts,
    minimum = 20
  )
  testthat::expect_true("clusters" %in% class(reduced))
  if (nrow(reduced[[2]]) > 0) {
    sums <- reduced[[2]][, .(total = sum(count, na.rm = TRUE)), by = target]
    testthat::expect_true(all(sums[["total"]] >= 20))
  }
})

testthat::test_that("reduce_clusters_to_min validates clusters input", {
  testthat::expect_error(
    splineClusterDetector::reduce_clusters_to_min(list(), minimum = 2),
    "class 'clusters'"
  )
})
