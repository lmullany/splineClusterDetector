# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
testthat::test_that(
  "zip_distance_matrix returns square non-negative symmetric matrix",
  {
    testthat::skip_if_not_installed("sf")

    z <- splineClusterDetector::zip_distance_matrix("OH", unit = "miles")

    testthat::expect_type(z, "list")
    testthat::expect_true(all(c("loc_vec", "distance_matrix") %in% names(z)))
    testthat::expect_gt(length(z[["loc_vec"]]), 0)
    testthat::expect_equal(
      dim(z[["distance_matrix"]]),
      c(length(z[["loc_vec"]]), length(z[["loc_vec"]]))
    )
    testthat::expect_true(all(diag(z[["distance_matrix"]]) == 0))
    testthat::expect_true(all(z[["distance_matrix"]] >= 0))

    idx <- seq_len(min(25, nrow(z[["distance_matrix"]])))
    testthat::expect_equal(
      unname(z[["distance_matrix"]][idx, idx]),
      unname(t(z[["distance_matrix"]][idx, idx])),
      tolerance = 1e-8
    )
  }
)

testthat::test_that(
  "zip_distance_matrix validates arguments and state content",
  {
    testthat::expect_error(
      splineClusterDetector::zip_distance_matrix("OH", unit = "bad-unit"),
      "arg"
    )
    testthat::expect_error(splineClusterDetector::zip_distance_matrix("ZZ"))
  }
)

testthat::test_that(
  "county_distance_matrix (built_in source) returns valid distance output",
  {
    testthat::skip_if_not_installed("sf")

    cty <- splineClusterDetector::county_distance_matrix(
      st = "OH",
      unit = "miles",
      source = "built_in"
    )

    testthat::expect_type(cty, "list")
    testthat::expect_true(
      all(c("loc_vec", "distance_matrix") %in% names(cty))
    )
    testthat::expect_gt(length(cty[["loc_vec"]]), 0)
    testthat::expect_equal(
      dim(cty[["distance_matrix"]]),
      c(length(cty[["loc_vec"]]), length(cty[["loc_vec"]]))
    )
    testthat::expect_true(all(diag(cty[["distance_matrix"]]) == 0))
    testthat::expect_true(all(cty[["distance_matrix"]] >= 0))
  }
)

testthat::test_that(
  "county_distance_matrix validates source and unit arguments",
  {
    testthat::expect_error(
      splineClusterDetector::county_distance_matrix(
        "OH",
        unit = "yards",
        source = "built_in"
      ),
      "arg"
    )
    testthat::expect_error(
      splineClusterDetector::county_distance_matrix(
        "OH",
        source = "bad-source"
      ),
      "arg"
    )
  }
)

us <- splineClusterDetector::us_distance_matrix(unit = "miles")

testthat::test_that("us_distance_matrix smoke and invariant checks", {
  testthat::skip_if_not_installed("sf")

  testthat::expect_type(us, "list")
  testthat::expect_true(all(c("loc_vec", "distance_matrix") %in% names(us)))
  testthat::expect_equal(
    dim(us[["distance_matrix"]]),
    c(length(us[["loc_vec"]]), length(us[["loc_vec"]]))
  )
  testthat::expect_true(all(diag(us[["distance_matrix"]]) == 0))
  testthat::expect_true(all(us[["distance_matrix"]] >= 0))
})

testthat::test_that(
  "county_distance_matrix routes US state to us_distance_matrix",
  {
    testthat::skip_if_not_installed("sf")

    c_us <- splineClusterDetector::county_distance_matrix("US", unit = "miles")

    testthat::expect_equal(length(c_us[["loc_vec"]]), length(us[["loc_vec"]]))
    testthat::expect_equal(
      dim(c_us[["distance_matrix"]]),
      dim(us[["distance_matrix"]])
    )
  }
)

testthat::test_that("create_dist_list returns named numeric neighbor vectors", {
  testthat::skip_if_not_installed("sf")

  d <- splineClusterDetector::create_dist_list(
    level = "county",
    threshold = 10,
    st = "OH",
    unit = "miles"
  )

  testthat::expect_type(d, "list")
  testthat::expect_gt(length(d), 0)
  testthat::expect_true(!is.null(names(d)))
  testthat::expect_true(all(names(d) != ""))
  testthat::expect_true(all(vapply(d, is.numeric, logical(1))))
  testthat::expect_true(
    all(vapply(d, function(x) !is.null(names(x)), logical(1)))
  )
  testthat::expect_true(all(vapply(d, function(x) all(x >= 0), logical(1))))
})

testthat::test_that("create_dist_list edge and error handling", {
  testthat::skip_if_not_installed("sf")

  d0 <- splineClusterDetector::create_dist_list(
    level = "county",
    threshold = 0,
    st = "OH",
    unit = "miles"
  )
  testthat::expect_gt(length(d0), 0)
  testthat::expect_true(all(vapply(d0, function(x) any(x == 0), logical(1))))

  testthat::expect_error(
    splineClusterDetector::create_dist_list(level = "city", st = "OH"),
    "arg"
  )
  testthat::expect_error(
    splineClusterDetector::create_dist_list(
      level = "county",
      unit = "yards",
      st = "OH"
    ),
    "arg"
  )
  # should not be null threshold
  testthat::expect_error(
    splineClusterDetector::create_dist_list(
      level = "zip",
      st = "OH",
      threshold = NULL
    )
  )
})

testthat::test_that("tract_generator validates st format", {
  testthat::skip_if_not_installed("tigris")

  testthat::expect_error(
    splineClusterDetector:::tract_generator(st = "2"),
    "2-character value"
  )
  testthat::expect_error(
    splineClusterDetector:::tract_generator(st = "MARYLAND"),
    "2-character value"
  )
  testthat::expect_error(
    splineClusterDetector:::tract_generator(st = "M1"),
    "two digits or two letters"
  )
})


testthat::test_that("tract_distance_matrix validates st format and unit", {
  testthat::skip_if_not_installed("tigris")

  testthat::expect_error(
    splineClusterDetector:::tract_distance_matrix(st = "M"),
    "2-character state abbreviation"
  )
  testthat::expect_error(
    splineClusterDetector:::tract_distance_matrix(st = "M1"),
    "two letters"
  )
  testthat::expect_error(
    splineClusterDetector:::tract_distance_matrix(st = "MD", unit = "yards"),
    "arg"
  )
})

testthat::test_that(
  "custom_distance_matrix returns valid symmetric matrix with labels",
  {
    testthat::skip_if_not_installed("sf")

    df <- data.frame(
      geoid = c("A", "B", "C"),
      latitude = c(39.0, 39.0, 40.0),
      longitude = c(-76.0, -75.0, -76.0)
    )

    dm_mi <- splineClusterDetector:::custom_distance_matrix(
      df = df,
      unit = "miles",
      label_var = "geoid",
      lat_var = "latitude",
      long_var = "longitude"
    )
    dm_km <- splineClusterDetector:::custom_distance_matrix(
      df = df,
      unit = "kilometers",
      label_var = "geoid",
      lat_var = "latitude",
      long_var = "longitude"
    )

    testthat::expect_type(dm_mi, "list")
    testthat::expect_true(
      all(c("loc_vec", "distance_matrix") %in% names(dm_mi))
    )
    testthat::expect_equal(dm_mi[["loc_vec"]], c("A", "B", "C"))
    testthat::expect_true(is.matrix(dm_mi[["distance_matrix"]]))
    testthat::expect_equal(dim(dm_mi[["distance_matrix"]]), c(3, 3))
    testthat::expect_equal(
      rownames(dm_mi[["distance_matrix"]]),
      c("A", "B", "C")
    )
    testthat::expect_equal(
      colnames(dm_mi[["distance_matrix"]]),
      c("A", "B", "C")
    )
    testthat::expect_equal(
      as.numeric(diag(dm_mi[["distance_matrix"]])),
      c(0, 0, 0),
      tolerance = 1e-10
    )
    testthat::expect_equal(
      unname(dm_mi[["distance_matrix"]]),
      unname(t(dm_mi[["distance_matrix"]])),
      tolerance = 1e-8
    )
    testthat::expect_true(all(dm_mi[["distance_matrix"]] >= 0))

    # unit conversion check (allowing small numeric differences)
    testthat::expect_equal(
      dm_km[["distance_matrix"]][1, 2],
      dm_mi[["distance_matrix"]][1, 2] * 1.609344,
      tolerance = 1e-5
    )
  }
)

testthat::test_that("custom_distance_matrix supports custom lat/long names", {
  testthat::skip_if_not_installed("sf")

  df <- data.frame(
    label = c("x1", "x2"),
    lat = c(39.0, 40.0),
    lon = c(-76.0, -76.0)
  )

  dm <- splineClusterDetector:::custom_distance_matrix(
    df = df,
    label_var = "label",
    lat_var = "lat",
    long_var = "lon"
  )

  testthat::expect_type(dm, "list")
  testthat::expect_true(all(c("loc_vec", "distance_matrix") %in% names(dm)))
  testthat::expect_equal(dm[["loc_vec"]], c("x1", "x2"))
  testthat::expect_equal(dim(dm[["distance_matrix"]]), c(2, 2))
  testthat::expect_equal(rownames(dm[["distance_matrix"]]), c("x1", "x2"))
  testthat::expect_true(dm[["distance_matrix"]][1, 2] > 0)
})

testthat::test_that(
  "custom_distance_matrix validates required columns and values",
  {
    testthat::expect_error(
      splineClusterDetector:::custom_distance_matrix(
        df = list(a = 1),
        label_var = "id",
        lat_var = "x",
        long_var = "y"
      ),
      "class data.frame"
    )

    testthat::expect_error(
      splineClusterDetector:::custom_distance_matrix(
        df = data.frame(id = c("a", "b"), latitude = c(39, 40)),
        label_var = "id",
        lat_var = "latitude",
        long_var = "y"
      ),
      class = "column_not_found"
    )

    testthat::expect_error(
      splineClusterDetector:::custom_distance_matrix(
        df = data.frame(
          id = c("a", "a"),
          latitude = c(39, 40),
          longitude = c(-76, -76)
        ),
        label_var = "id",
        lat_var = "latitude",
        long_var = "longitude"
      ),
      "must be unique"
    )

    testthat::expect_error(
      splineClusterDetector:::custom_distance_matrix(
        df = data.frame(
          id = c("a", NA),
          latitude = c(39, 40),
          longitude = c(-76, -76)
        ),
        label_var = "id",
        lat_var = "latitude",
        long_var = "longitude"
      ),
      "Label column contains NA"
    )

    testthat::expect_error(
      splineClusterDetector:::custom_distance_matrix(
        df = data.frame(
          id = c("a", "b"),
          latitude = c("39", "40"),
          longitude = c(-76, -76)
        ),
        label_var = "id",
        lat_var = "latitude",
        long_var = "longitude"
      ),
      "must be numeric"
    )

    testthat::expect_error(
      splineClusterDetector:::custom_distance_matrix(
        df = data.frame(
          id = c("a", "b"),
          latitude = c(39, NA_real_),
          longitude = c(-76, -76)
        ),
        label_var = "id",
        lat_var = "latitude",
        long_var = "longitude"
      ),
      "cannot contain NA"
    )

    testthat::expect_error(
      splineClusterDetector:::custom_distance_matrix(
        df = data.frame(
          id = c("a", "b"),
          latitude = c(39, 40),
          longitude = c(-76, -76)
        ),
        label_var = "id",
        lat_var = "latitude",
        long_var = "longitude",
        unit = "yards"
      ),
      "arg"
    )
  }
)

testthat::test_that(
  "create_custom_dist_list returns sparse named neighbor vectors",
  {
    testthat::skip_if_not_installed("sf")

    df <- data.frame(
      geoid = c("A", "B", "C"),
      latitude = c(39.0, 39.0, 40.0),
      longitude = c(-76.0, -75.0, -76.0)
    )

    d <- splineClusterDetector::create_custom_dist_list(
      df = df,
      label_var = "geoid",
      lat_var = "latitude",
      long_var = "longitude",
      threshold = 80,
      unit = "miles"
    )

    testthat::expect_type(d, "list")
    testthat::expect_equal(length(d), 3)
    testthat::expect_setequal(names(d), c("A", "B", "C"))
    testthat::expect_true(all(vapply(d, is.numeric, logical(1))))
    testthat::expect_true(
      all(vapply(d, function(x) !is.null(names(x)), logical(1)))
    )
    testthat::expect_true(
      all(vapply(d, function(x) all(x >= 0), logical(1)))
    )
    testthat::expect_true(
      all(vapply(d, function(x) all(x <= 80 + 1e-8), logical(1)))
    )
    testthat::expect_true(
      all(vapply(names(d), function(nm) nm %in% names(d[[nm]]), logical(1)))
    )
  }
)

testthat::test_that(
  "create_custom_dist_list respects threshold and custom var names",
  {
    testthat::skip_if_not_installed("sf")

    df <- data.frame(
      id = c("x1", "x2", "x3"),
      lat = c(39.0, 39.0, 40.5),
      lon = c(-76.0, -76.0, -76.0)
    )

    d0 <- splineClusterDetector::create_custom_dist_list(
      df = df,
      label_var = "id",
      lat_var = "lat",
      long_var = "lon",
      threshold = 0
    )
    testthat::expect_equal(length(d0), 3)
    testthat::expect_true(
      all(vapply(d0, function(x) length(x) >= 1, logical(1)))
    )
    testthat::expect_true(
      all(vapply(d0, function(x) any(x == 0), logical(1)))
    )

    d_km <- splineClusterDetector::create_custom_dist_list(
      df = df,
      label_var = "id",
      lat_var = "lat",
      long_var = "lon",
      threshold = 1,
      unit = "kilometers"
    )
    testthat::expect_type(d_km, "list")
    testthat::expect_equal(length(d_km), 3)
  }
)

testthat::test_that("create_custom_dist_list validates inputs", {
  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = list(a = 1),
      label_var = "id",
      lat_var = "latitude",
      long_var = "longitude",
      threshold = 10
    ),
    "class data.frame"
  )

  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = data.frame(
        id = c("a", "b"),
        latitude = c(1, 2),
        longitude = c(3, 4)
      ),
      label_var = c("id", "other"),
      lat_var = "latitude",
      long_var = "longitude",
      threshold = 10
    ),
    "single column name"
  )

  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = data.frame(
        id = c("a", "b"),
        latitude = c(1, 2),
        longitude = c(3, 4)
      ),
      label_var = "id",
      lat_var = "latitude",
      long_var = "longitude",
      threshold = -1
    ),
    "non-negative numeric"
  )

  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = data.frame(
        id = c("a", "b"),
        latitude = c(1, 2),
        longitude = c(3, 4)
      ),
      label_var = "id",
      threshold = 10,
      unit = "yards",
      lat_var = "latitude",
      long_var = "longitude"
    ),
    "arg"
  )

  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = data.frame(
        id = c("a", "a"),
        latitude = c(1, 2),
        longitude = c(3, 4)
      ),
      label_var = "id",
      threshold = 10,
      lat_var = "latitude",
      long_var = "longitude"
    ),
    "must be unique"
  )

  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = data.frame(
        id = c("a", NA),
        latitude = c(1, 2),
        longitude = c(3, 4)
      ),
      label_var = "id",
      threshold = 10,
      lat_var = "latitude",
      long_var = "longitude"
    ),
    "Label column contains NA"
  )

  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = data.frame(
        id = c("a", "b"),
        latitude = c("1", "2"),
        longitude = c(3, 4)
      ),
      label_var = "id",
      threshold = 10,
      lat_var = "latitude",
      long_var = "longitude"
    ),
    "must be numeric"
  )

  testthat::expect_error(
    splineClusterDetector::create_custom_dist_list(
      df = data.frame(
        id = c("a", "b"),
        latitude = c(1, NA_real_),
        longitude = c(3, 4)
      ),
      label_var = "id",
      threshold = 10,
      lat_var = "latitude",
      long_var = "longitude"
    ),
    "cannot contain NA"
  )
})
