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

.has_ggplot2 <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

.has_plotly <- function() {
  requireNamespace("plotly", quietly = TRUE)
}

.skip_if_no_ggplot2 <- function() {
  if (!.has_ggplot2()) {
    testthat::skip("{ggplot2} is not installed")
  }
}

.skip_if_no_plotly <- function() {
  if (!.has_plotly()) {
    testthat::skip("{plotly} is not installed")
  }
}

testthat::test_that("generate_summary_table smoke, edge, and error checks", {
  d <- .load_example_data()

  s <- gsClusterDetect::generate_summary_table(
    data = d,
    end_date = as.Date("2025-01-31"),
    baseline_length = 60,
    test_length = 7
  )

  testthat::expect_s3_class(s, "data.frame")
  testthat::expect_true(
    all(c(
      "Statistic (rounded means)",
      "Baseline Interval",
      "Test Interval"
    ) %in% names(s))
  )
  testthat::expect_true("Nr. Dates" %in% s[["Statistic (rounded means)"]])
  testthat::expect_equal(
    as.numeric(
      s[s[["Statistic (rounded means)"]] == "Nr. Dates", "Baseline Interval"]
    ),
    60
  )
  testthat::expect_equal(
    as.numeric(
      s[s[["Statistic (rounded means)"]] == "Nr. Dates", "Test Interval"]
    ),
    7
  )

  one_loc <- unique(d[["location"]])[1]
  s_one <- gsClusterDetect::generate_summary_table(
    data = d,
    end_date = as.Date("2025-01-31"),
    locations = one_loc,
    baseline_length = 10,
    test_length = 3
  )
  testthat::expect_s3_class(s_one, "data.frame")

  testthat::expect_error(
    gsClusterDetect::generate_summary_table(
      data = d,
      cut_vec = c(0, 1, 2),
      cut_labels = c("a")
    ),
    "cutVec"
  )
  testthat::expect_error(
    gsClusterDetect::generate_summary_table(
      data = d[, .(location, date)],
      end_date = as.Date("2025-01-31")
    ),
    "date, location, or count"
  )
  testthat::expect_error(
    gsClusterDetect::generate_summary_table(
      data = d,
      locations = "does-not-exist",
      end_date = as.Date("2025-01-31")
    ),
    "No data"
  )
})

testthat::test_that("generate_heatmap_data smoke, invariant, and errors", {
  d <- .load_example_data()

  h <- gsClusterDetect::generate_heatmap_data(
    data = d,
    end_date = as.Date("2025-01-31"),
    baseline_length = 30,
    test_length = 7
  )

  testthat::expect_s3_class(h, "data.table")
  testthat::expect_true(
    all(c("location", "date", "count", "count_cat") %in% names(h))
  )
  testthat::expect_true(!is.null(attr(h, "start_date")))
  testthat::expect_equal(attr(h, "baseline_length"), 30)
  testthat::expect_true(all(!is.na(h[["count_cat"]])))

  h_small <- gsClusterDetect::generate_heatmap_data(
    data = d,
    end_date = as.Date("2025-01-31"),
    locations = unique(d[["location"]])[1:2],
    baseline_length = 10,
    test_length = 3
  )
  testthat::expect_true(length(unique(h_small[["location"]])) <= 2)

  testthat::expect_error(
    gsClusterDetect::generate_heatmap_data(
      data = d,
      break_points = c(0, 1),
      break_labels = c("a", "b")
    ),
    "Break points"
  )
  testthat::expect_error(
    gsClusterDetect::generate_heatmap_data(
      data = d,
      locations = "does-not-exist"
    ),
    "No data"
  )
})

testthat::test_that("generate_heatmap returns expected plot object classes", {
  d <- .load_example_data()
  h <- gsClusterDetect::generate_heatmap_data(
    data = d,
    end_date = as.Date("2025-01-31"),
    baseline_length = 30,
    test_length = 7
  )

  if (.has_ggplot2()) {
    p_gg <- gsClusterDetect::generate_heatmap(h, plot_type = "ggplot")
    testthat::expect_s3_class(p_gg, "ggplot")
  } else if (.has_plotly()) {
    testthat::expect_warning(
      p_gg <- gsClusterDetect::generate_heatmap(h, plot_type = "ggplot"),
      "Falling back"
    )
    testthat::expect_s3_class(p_gg, "plotly")
  } else {
    testthat::expect_error(
      gsClusterDetect::generate_heatmap(h, plot_type = "ggplot"),
      class = "plotting_libraries_not_found"
    )
  }

  if (.has_plotly()) {
    p_pl <- gsClusterDetect::generate_heatmap(h, plot_type = "plotly")
    testthat::expect_s3_class(p_pl, "plotly")
  } else if (.has_ggplot2()) {
    testthat::expect_warning(
      p_pl <- gsClusterDetect::generate_heatmap(h, plot_type = "plotly"),
      "Falling back"
    )
    testthat::expect_s3_class(p_pl, "ggplot")
  } else {
    testthat::expect_error(
      gsClusterDetect::generate_heatmap(h, plot_type = "plotly"),
      class = "plotting_libraries_not_found"
    )
  }

  testthat::expect_error(
    gsClusterDetect::generate_heatmap(h, plot_type = "base"),
    "arg"
  )
})

testthat::test_that(
  "generate_heatmap errors if neither plotting library is installed",
  {
    d <- .load_example_data()
    h <- gsClusterDetect::generate_heatmap_data(
      data = d,
      end_date = as.Date("2025-01-31"),
      baseline_length = 30,
      test_length = 7
    )

    testthat::local_mocked_bindings(
      requireNamespace = function(pkg, quietly = TRUE) FALSE,
      .package = "base"
    )

    testthat::expect_error(
      gsClusterDetect::generate_heatmap(h, plot_type = "ggplot"),
      class = "plotting_libraries_not_found"
    )
    testthat::expect_error(
      gsClusterDetect::generate_heatmap(h, plot_type = "plotly"),
      class = "plotting_libraries_not_found"
    )
  }
)

testthat::test_that("generate_heatmap warns and falls back if plotly missing", {
  .skip_if_no_ggplot2()
  d <- .load_example_data()
  h <- gsClusterDetect::generate_heatmap_data(
    data = d,
    end_date = as.Date("2025-01-31"),
    baseline_length = 30,
    test_length = 7
  )

  orig_require_namespace <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, quietly = TRUE) {
      if (identical(pkg, "plotly")) {
        return(FALSE)
      }
      orig_require_namespace(pkg, quietly = quietly)
    },
    .package = "base"
  )

  testthat::expect_warning(
    p <- gsClusterDetect::generate_heatmap(h, plot_type = "plotly"),
    "Falling back"
  )
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that(
  "generate_heatmap warns and falls back if ggplot2 missing",
  {
    .skip_if_no_plotly()
    d <- .load_example_data()
    h <- gsClusterDetect::generate_heatmap_data(
      data = d,
      end_date = as.Date("2025-01-31"),
      baseline_length = 30,
      test_length = 7
    )

    orig_require_namespace <- base::requireNamespace
    testthat::local_mocked_bindings(
      requireNamespace = function(pkg, quietly = TRUE) {
        if (identical(pkg, "ggplot2")) {
          return(FALSE)
        }
        orig_require_namespace(pkg, quietly = quietly)
      },
      .package = "base"
    )

    testthat::expect_warning(
      p <- gsClusterDetect::generate_heatmap(h, plot_type = "ggplot"),
      "Falling back"
    )
    testthat::expect_s3_class(p, "plotly")
  }
)

testthat::test_that("generate_time_series_data smoke, edge, and error checks", {
  d <- .load_example_data()

  tsd <- gsClusterDetect::generate_time_series_data(
    data = d,
    end_date = as.Date("2025-01-31"),
    baseline_length = 60,
    test_length = 7
  )

  testthat::expect_s3_class(tsd, "data.table")
  testthat::expect_true(all(c("date", "date_count") %in% names(tsd)))
  testthat::expect_gt(nrow(tsd), 0)
  testthat::expect_true(all(diff(tsd[["date"]]) >= 1))
  testthat::expect_true(all(tsd[["date_count"]] >= 0))

  tsd_one <- gsClusterDetect::generate_time_series_data(
    data = d,
    end_date = as.Date("2025-01-31"),
    locations = unique(d[["location"]])[1],
    baseline_length = 10,
    test_length = 3
  )
  testthat::expect_gt(nrow(tsd_one), 0)

  testthat::expect_error(
    gsClusterDetect::generate_time_series_data(
      data = d[, .(location, date)]
    ),
    "required columns"
  )
  testthat::expect_error(
    gsClusterDetect::generate_time_series_data(
      data = d,
      locations = "does-not-exist"
    ),
    "No data"
  )
})

testthat::test_that(
  "generate_time_series_plot returns ggplot and plotly outputs",
  {
    d <- .load_example_data()
    tsd <- gsClusterDetect::generate_time_series_data(
      data = d,
      end_date = as.Date("2025-01-31"),
      baseline_length = 30,
      test_length = 7
    )

    if (.has_ggplot2()) {
      p_gg <- gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        end_date = as.Date("2025-01-31"),
        plot_type = "ggplot",
        locations = "OH"
      )
      testthat::expect_s3_class(p_gg, "ggplot")
    } else if (.has_plotly()) {
      testthat::expect_warning(
        p_gg <- gsClusterDetect::generate_time_series_plot(
          time_series_data = tsd,
          end_date = as.Date("2025-01-31"),
          plot_type = "ggplot",
          locations = "OH"
        ),
        "Falling back"
      )
      testthat::expect_s3_class(p_gg, "plotly")
    } else {
      testthat::expect_error(
        gsClusterDetect::generate_time_series_plot(
          time_series_data = tsd,
          end_date = as.Date("2025-01-31"),
          plot_type = "ggplot",
          locations = "OH"
        ),
        class = "plotting_libraries_not_found"
      )
    }

    if (.has_plotly()) {
      p_pl <- gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        end_date = as.Date("2025-01-31"),
        plot_type = "plotly",
        locations = "OH"
      )
      testthat::expect_s3_class(p_pl, "plotly")
    } else if (.has_ggplot2()) {
      testthat::expect_warning(
        p_pl <- gsClusterDetect::generate_time_series_plot(
          time_series_data = tsd,
          end_date = as.Date("2025-01-31"),
          plot_type = "plotly",
          locations = "OH"
        ),
        "Falling back"
      )
      testthat::expect_s3_class(p_pl, "ggplot")
    } else {
      testthat::expect_error(
        gsClusterDetect::generate_time_series_plot(
          time_series_data = tsd,
          end_date = as.Date("2025-01-31"),
          plot_type = "plotly",
          locations = "OH"
        ),
        class = "plotting_libraries_not_found"
      )
    }

    if (.has_ggplot2()) {
      p_default_end <- gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        plot_type = "ggplot",
        locations = "OH"
      )
      testthat::expect_s3_class(p_default_end, "ggplot")
    } else if (.has_plotly()) {
      testthat::expect_warning(
        p_default_end <- gsClusterDetect::generate_time_series_plot(
          time_series_data = tsd,
          plot_type = "ggplot",
          locations = "OH"
        ),
        "Falling back"
      )
      testthat::expect_s3_class(p_default_end, "plotly")
    } else {
      testthat::expect_error(
        gsClusterDetect::generate_time_series_plot(
          time_series_data = tsd,
          plot_type = "ggplot",
          locations = "OH"
        ),
        class = "plotting_libraries_not_found"
      )
    }

    testthat::expect_error(
      gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        plot_type = "base"
      ),
      "arg"
    )
  }
)

testthat::test_that(
  "generate_time_series_plot errors if neither plotting library is installed",
  {
    d <- .load_example_data()
    tsd <- gsClusterDetect::generate_time_series_data(
      data = d,
      end_date = as.Date("2025-01-31"),
      baseline_length = 30,
      test_length = 7
    )

    testthat::local_mocked_bindings(
      requireNamespace = function(pkg, quietly = TRUE) FALSE,
      .package = "base"
    )

    testthat::expect_error(
      gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        plot_type = "ggplot",
        locations = "OH"
      ),
      class = "plotting_libraries_not_found"
    )
    testthat::expect_error(
      gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        plot_type = "plotly",
        locations = "OH"
      ),
      class = "plotting_libraries_not_found"
    )
  }
)

testthat::test_that(
  "generate_time_series_plot warns and falls back if plotly missing",
  {
    .skip_if_no_ggplot2()
    d <- .load_example_data()
    tsd <- gsClusterDetect::generate_time_series_data(
      data = d,
      end_date = as.Date("2025-01-31"),
      baseline_length = 30,
      test_length = 7
    )

    orig_require_namespace <- base::requireNamespace
    testthat::local_mocked_bindings(
      requireNamespace = function(pkg, quietly = TRUE) {
        if (identical(pkg, "plotly")) {
          return(FALSE)
        }
        orig_require_namespace(pkg, quietly = quietly)
      },
      .package = "base"
    )

    testthat::expect_warning(
      p <- gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        plot_type = "plotly",
        locations = "OH"
      ),
      "Falling back"
    )
    testthat::expect_s3_class(p, "ggplot")
  }
)

testthat::test_that(
  "generate_time_series_plot warns and falls back if ggplot2 missing",
  {
    .skip_if_no_plotly()
    d <- .load_example_data()
    tsd <- gsClusterDetect::generate_time_series_data(
      data = d,
      end_date = as.Date("2025-01-31"),
      baseline_length = 30,
      test_length = 7
    )

    orig_require_namespace <- base::requireNamespace
    testthat::local_mocked_bindings(
      requireNamespace = function(pkg, quietly = TRUE) {
        if (identical(pkg, "ggplot2")) {
          return(FALSE)
        }
        orig_require_namespace(pkg, quietly = quietly)
      },
      .package = "base"
    )

    testthat::expect_warning(
      p <- gsClusterDetect::generate_time_series_plot(
        time_series_data = tsd,
        plot_type = "ggplot",
        locations = "OH"
      ),
      "Falling back"
    )
    testthat::expect_s3_class(p, "plotly")
  }
)
