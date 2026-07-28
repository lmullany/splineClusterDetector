.load_example_data <- function() {
  location <- example_count_data <- NULL

  data(
    "example_count_data",
    package = "gsClusterDetect",
    envir = environment()
  )

  cases <- data.table::as.data.table(example_count_data)
  cases[, location := as.character(location)]

  list(
    cases = cases,
    dm = create_dist_list("county", 50, "OH")
  )
}

.load_example_clusters <- function() {
  d <- .load_example_data()

  find_clusters(
    cases = d[["cases"]],
    distance_matrix = d[["dm"]],
    detect_date = d[["cases"]][, max(date)]
  )
}

.load_example_shape <- function() {
  s <- gsClusterDetect:::county_sf
  s[s$STATEFP == "39", ]
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

.expect_named_elements <- function(x, nms) {
  testthat::expect_true(all(nms %in% names(x)))
}


testthat::test_that("validate_sf_file works for valid inputs", {
  s <- .load_example_shape()

  testthat::expect_no_error(validate_sf_file(s, "GEOID"))
  testthat::expect_no_error(validate_sf_file(s, "GEOID", label_id = NULL))
  testthat::expect_no_error(validate_sf_file(s, "GEOID", label_id = "NAME"))
})


testthat::test_that("validate_sf_file catches invalid inputs", {
  s <- .load_example_shape()

  testthat::expect_error(validate_sf_file(as.data.frame(s), "GEOID"))
  testthat::expect_error(validate_sf_file(s, "not_a_column"))
  testthat::expect_error(validate_sf_file(s, c("GEOID", "NAME")))
  testthat::expect_error(validate_sf_file(s, NA_character_))
  testthat::expect_error(validate_sf_file(s, ""))
  testthat::expect_error(
    validate_sf_file(s, "GEOID", label_id = "not_a_column")
  )

  # STATEFP is constant for Ohio counties, so it should not uniquely identify
  # rows.
  testthat::expect_error(validate_sf_file(s, "STATEFP"))
})


testthat::test_that("validate_sf_file allows non-unique label_id", {
  s <- .load_example_shape()

  # STATEFP is not unique, but it should be allowed as a display label.
  testthat::expect_no_error(validate_sf_file(s, "GEOID", label_id = "STATEFP"))
})


testthat::test_that("validate_sf_file catches missing s_id values", {
  s <- .load_example_shape()
  s$bad_id <- s$GEOID
  s$bad_id[1] <- NA_character_

  testthat::expect_error(validate_sf_file(s, "bad_id"))
})


testthat::test_that("validate_clusters works for valid cluster object", {
  cl <- .load_example_clusters()

  testthat::expect_no_error(validate_clusters(cl))
})


testthat::test_that(
  "validate_clusters catches invalid class, names, and length",
  {
    cl <- .load_example_clusters()

    true_class <- class(cl)
    true_names <- names(cl)

    class(cl) <- "list"
    testthat::expect_error(validate_clusters(cl))

    class(cl) <- true_class
    names(cl) <- c("a", "b")
    testthat::expect_error(validate_clusters(cl))

    names(cl) <- true_names
    cl_short <- cl[1]
    class(cl_short) <- true_class
    testthat::expect_error(validate_clusters(cl_short))
  }
)


testthat::test_that("validate_clusters catches missing required columns", {
  cl <- .load_example_clusters()

  cl_missing_alert <- cl
  cl_missing_alert$cluster_alert_table$cluster_center <- NULL
  testthat::expect_error(validate_clusters(cl_missing_alert))

  cl_missing_locations <- cl
  cl_missing_locations$cluster_location_counts$location <- NULL
  testthat::expect_error(validate_clusters(cl_missing_locations))
})


testthat::test_that(
  "validate_clusters catches missing and duplicated cluster locations",
  {
    cl <- .load_example_clusters()

    cl_missing_location <- cl
    cl_missing_location$cluster_location_counts$location[1] <- NA_character_
    testthat::expect_error(validate_clusters(cl_missing_location))

    cl_missing_center <- cl
    cl_missing_center$cluster_location_counts$cluster_center[1] <- NA_character_
    testthat::expect_error(validate_clusters(cl_missing_center))

    cl_dup_location <- cl
    cl_dup_location$cluster_location_counts$location[2] <-
      cl_dup_location$cluster_location_counts$location[1]
    testthat::expect_error(validate_clusters(cl_dup_location))
  }
)


testthat::test_that("get_point_data returns one point per input row", {
  s <- .load_example_shape()

  pts <- get_point_data(s)

  testthat::expect_s3_class(pts, "sf")
  testthat::expect_equal(nrow(pts), nrow(s))
  testthat::expect_true(all(sf::st_geometry_type(pts) %in% c("POINT")))
})


testthat::test_that("get_point_data preserves original CRS", {
  s <- .load_example_shape()

  pts <- get_point_data(s)

  testthat::expect_equal(sf::st_crs(pts), sf::st_crs(s))
})


testthat::test_that("get_point_data accepts user supplied point_crs", {
  s <- .load_example_shape()

  pts <- get_point_data(s, point_crs = 5070)

  testthat::expect_s3_class(pts, "sf")
  testthat::expect_equal(nrow(pts), nrow(s))
  testthat::expect_equal(sf::st_crs(pts), sf::st_crs(s))
})


testthat::test_that("get_point_data errors without CRS", {
  s <- .load_example_shape()
  sf::st_crs(s) <- NA

  testthat::expect_error(get_point_data(s))
})


testthat::test_that("prepare_map_data returns expected structure", {
  cl <- .load_example_clusters()
  s <- .load_example_shape()

  md <- prepare_map_data(cl, s, "GEOID")

  .expect_named_elements(
    md,
    c(
      "map_data",
      "color_values",
      "id_col",
      "cluster_centers",
      "point_data",
      "label_id"
    )
  )

  testthat::expect_s3_class(md$map_data, "sf")
  testthat::expect_equal(md$id_col, "GEOID")
  testthat::expect_equal(md$label_id, "GEOID")
  testthat::expect_equal(nrow(md$map_data), nrow(s))
  testthat::expect_equal(nrow(md$point_data), nrow(s))
})


testthat::test_that(
  "prepare_map_data builds color values for cluster centers",
  {
    cl <- .load_example_clusters()
    s <- .load_example_shape()

    md <- prepare_map_data(cl, s, "GEOID")

    testthat::expect_true(is.character(md$cluster_centers))
    testthat::expect_true(length(md$cluster_centers) > 0)
    testthat::expect_true(is.character(md$color_values))
    testthat::expect_equal(names(md$color_values), md$cluster_centers)
  }
)


testthat::test_that(
  "prepare_map_data uses s_id as label when label_id is NULL",
  {
    cl <- .load_example_clusters()
    s <- .load_example_shape()

    md <- prepare_map_data(cl, s, "GEOID")

    testthat::expect_true("label_text" %in% names(md$point_data))
    testthat::expect_equal(md$point_data$label_text, md$point_data$GEOID)
  }
)


testthat::test_that(
  "prepare_map_data uses label_id as display label when supplied",
  {
    cl <- .load_example_clusters()
    s <- .load_example_shape()

    md <- prepare_map_data(cl, s, "GEOID", label_id = "NAME")

    testthat::expect_equal(md$label_id, "NAME")
    testthat::expect_equal(
      md$point_data$label_text, s$NAME[match(md$point_data$GEOID, s$GEOID)]
    )
  }
)


testthat::test_that(
  "prepare_map_data creates hover text for locations",
  {
    cl <- .load_example_clusters()
    s <- .load_example_shape()

    md <- prepare_map_data(cl, s, "GEOID", label_id = "NAME")

    clustered <- md$point_data[!is.na(md$point_data$cluster_center), ]
    non_clustered <- md$point_data[is.na(md$point_data$cluster_center), ]

    testthat::expect_true(nrow(clustered) > 0)
    testthat::expect_true(nrow(non_clustered) > 0)

    testthat::expect_true(
      any(grepl("Cluster center:", clustered$hover_text, fixed = TRUE))
    )
    testthat::expect_true(
      any(grepl("Count:", clustered$hover_text, fixed = TRUE))
    )

    testthat::expect_false(
      any(grepl("Cluster center:", non_clustered$hover_text, fixed = TRUE))
    )
    testthat::expect_true(
      all(grepl("Location:", non_clustered$hover_text, fixed = TRUE))
    )
  }
)


testthat::test_that(
  "prepare_map_data errors if cluster locations are not in shape file",
  {
    cl <- .load_example_clusters()
    s <- .load_example_shape()

    s <- s[!s$GEOID %in% cl$cluster_location_counts$location[1], ]

    testthat::expect_error(prepare_map_data(cl, s, "GEOID"))
  }
)


testthat::test_that("prepare_map_data does not modify input sf object", {
  cl <- .load_example_clusters()
  s <- .load_example_shape()
  s_before <- s

  invisible(prepare_map_data(cl, s, "GEOID"))

  testthat::expect_equal(s, s_before)
})


testthat::test_that("sf_to_plotly_polygons returns expected columns and ids", {
  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID")

  poly_df <- sf_to_plotly_polygons(md)

  testthat::expect_true(is.data.frame(poly_df))
  testthat::expect_true(
    all(c("GEOID", "cluster_center", "count", "x", "y") %in% names(poly_df))
  )
  testthat::expect_true(all(s$GEOID %in% unique(poly_df$GEOID)))
  testthat::expect_true(any(is.na(poly_df$x)))
  testthat::expect_true(any(is.na(poly_df$y)))
})


testthat::test_that("sf_to_plotly_polygons respects arbitrary s_id name", {
  cl <- .load_example_clusters()
  s <- .load_example_shape()

  s$custom_id <- s$GEOID

  md <- prepare_map_data(cl, s, "custom_id")
  poly_df <- sf_to_plotly_polygons(md)

  testthat::expect_true("custom_id" %in% names(poly_df))
  testthat::expect_false("GEOID" %in% names(poly_df))
  testthat::expect_true(all(s$custom_id %in% unique(poly_df$custom_id)))
})


testthat::test_that("sf_to_plotly_polygons catches invalid prepared objects", {
  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID")

  testthat::expect_error(sf_to_plotly_polygons(NULL))

  md_missing <- md
  md_missing$map_data <- NULL
  testthat::expect_error(sf_to_plotly_polygons(md_missing))

  md_bad <- md
  md_bad$map_data <- as.data.frame(md_bad$map_data)
  testthat::expect_error(sf_to_plotly_polygons(md_bad))
})


testthat::test_that("map_clusters_ggplot returns ggplot object", {
  .skip_if_no_ggplot2()

  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID")

  p <- map_clusters_ggplot(md)

  testthat::expect_s3_class(p, "ggplot")
})


testthat::test_that("map_clusters_ggplot can label all locations", {
  .skip_if_no_ggplot2()

  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID", label_id = "NAME")

  p_none <- map_clusters_ggplot(md, label = "none")
  p_all <- map_clusters_ggplot(md, label = "all")

  testthat::expect_s3_class(p_none, "ggplot")
  testthat::expect_s3_class(p_all, "ggplot")
  testthat::expect_true(length(p_all$layers) > length(p_none$layers))
})


testthat::test_that("map_clusters_ggplot supports cluster label modes", {
  .skip_if_no_ggplot2()

  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID", label_id = "NAME")

  p_centers <- map_clusters_ggplot(md, label = "cluster_centers")
  p_locations <- map_clusters_ggplot(md, label = "cluster_locations")

  testthat::expect_s3_class(p_centers, "ggplot")
  testthat::expect_s3_class(p_locations, "ggplot")
})


testthat::test_that("map_clusters_ggplot errors for invalid label choice", {
  .skip_if_no_ggplot2()

  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID")

  testthat::expect_error(map_clusters_ggplot(md, label = "bad_label"))
})


testthat::test_that("map_clusters_plotly returns plotly object", {
  .skip_if_no_plotly()

  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID")

  p <- map_clusters_plotly(md)

  testthat::expect_s3_class(p, "plotly")
  testthat::expect_s3_class(p, "htmlwidget")
})


testthat::test_that("map_clusters_plotly includes hover point trace", {
  .skip_if_no_plotly()

  cl <- .load_example_clusters()
  s <- .load_example_shape()
  md <- prepare_map_data(cl, s, "GEOID", label_id = "NAME")

  p <- map_clusters_plotly(md)
  pb <- plotly::plotly_build(p)

  trace_modes <- vapply(
    pb$x$data, function(z) z$mode %||% NA_character_, character(1)
  )
  trace_types <- vapply(
    pb$x$data, function(z) z$type %||% NA_character_, character(1)
  )

  testthat::expect_true(
    any(trace_types == "scatter" & trace_modes == "markers")
  )
})


testthat::test_that("map_clusters dispatches to ggplot engine", {
  .skip_if_no_ggplot2()

  cl <- .load_example_clusters()
  s <- .load_example_shape()

  p <- map_clusters(cl, s, "GEOID", engine = "ggplot")

  testthat::expect_s3_class(p, "ggplot")
})


testthat::test_that("map_clusters dispatches to plotly engine", {
  .skip_if_no_plotly()

  cl <- .load_example_clusters()
  s <- .load_example_shape()

  p <- map_clusters(cl, s, "GEOID", engine = "plotly")

  testthat::expect_s3_class(p, "plotly")
})


testthat::test_that("map_clusters validates engine and label choices", {
  cl <- .load_example_clusters()
  s <- .load_example_shape()

  testthat::expect_error(map_clusters(cl, s, "GEOID", engine = "not_an_engine"))
  testthat::expect_error(
    map_clusters(cl, s, "GEOID", engine = "ggplot", label = "bad_label")
  )
})
