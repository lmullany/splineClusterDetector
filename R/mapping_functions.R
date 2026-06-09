#' Validates cluster object
#'
#' Function ingests an object and checks its validity as an object returned
#' from \code{find_clusters()}
#'
#' @param cl object to validate
#' @keywords internal

validate_clusters <- function(cl) {
  if (!inherits(cl, "clusters")) {
    cli::cli_abort(
      "`cl` must be an object of class 'clusters' as returned by `find_clusters()`."
    )
  }

  if (!is.list(cl) || length(cl) != 2) {
    cli::cli_abort(
      "`cl` must be a list of length 2 as returned by `find_clusters()`."
    )
  }

  expected_names <- c("cluster_alert_table", "cluster_location_counts")

  if (!identical(names(cl), expected_names)) {
    cli::cli_abort(paste0(
      "`cl` has unexpected names. Expected: ",
      paste(expected_names, collapse = ", "), "."
    ))
  }

  required_alert_cols <- c(
    "cluster_center",
    "cluster_start_date",
    "cluster_center_observed"
  )

  required_location_cols <- c(
    "location",
    "count",
    "cluster_center"
  )

  check_vars(cl$cluster_alert_table, required_alert_cols)
  check_vars(cl$cluster_location_counts, required_location_cols)

  if (anyNA(cl$cluster_location_counts$location)) {
    cli::cli_abort("`cl$cluster_location_counts$location` contains missing values.")
  }

  if (anyNA(cl$cluster_location_counts$cluster_center)) {
    cli::cli_abort("`cl$cluster_location_counts$cluster_center` contains missing values.")
  }

  if (anyDuplicated(cl$cluster_location_counts$location)) {
    cli::cli_abort(
      "`cl$cluster_location_counts$location` must contain at most one row per mapped location."
    )
  }

  invisible(TRUE)
}


#' Validates sf file
#'
#' Function ingests an object and check if this is of class
#' sf, and has an unique identifer for its rows
#'
#' @param s shape file; must be of class sf
#' @param s_id string unique identifier of `s`
#' @keywords internal
validate_sf_file <- function(s, s_id) {
  if (!inherits(s, "sf")) {
    cli::cli_abort("`s` must be an object of class `sf`.")
  }

  if (!is.character(s_id) || length(s_id) != 1L || is.na(s_id) || !nzchar(s_id)) {
    cli::cli_abort("`s_id` must be a single non-missing string.")
  }

  check_vars(s, s_id)

  if (anyNA(s[[s_id]])) {
    cli::cli_abort("`s[[s_id]]` contains missing values.")
  }

  if (length(unique(s[[s_id]])) != nrow(s)) {
    cli::cli_abort("`s_id` must uniquely identify rows of `s`.")
  }

  if (!inherits(sf::st_geometry(s), "sfc")) {
    cli::cli_abort("`s` must have a valid sf geometry column.")
  }

  if (any(sf::st_is_empty(s))) {
    cli::cli_abort("`s` contains empty geometries.")
  }

  invisible(TRUE)
}


#' Compute representative hover points for sf polygons
#'
#' Computes one point per input feature for use in plotly hover layers.
#' If the input geometry is longitude/latitude, the point-on-surface operation
#' is performed in a projected CRS and then transformed back to the original CRS.
#'
#' @param x sf object
#' @param hover_crs CRS to use when `x` is longitude/latitude. If NULL, EPSG:3857
#'   is used as a general-purpose fallback.
#'
#' @keywords internal
get_hover_points <- function(x, hover_crs = NULL) {
  if (!inherits(x, "sf")) {
    cli::cli_abort("`x` must be an sf object.")
  }

  original_crs <- sf::st_crs(x)

  if (is.na(original_crs)) {
    cli::cli_abort(
      "`x` must have a valid coordinate reference system before hover points can be computed."
    )
  }

  use_projection <- sf::st_is_longlat(x)

  if (use_projection) {
    if (is.null(hover_crs)) {
      hover_crs <- 3857
    }

    x_for_points <- sf::st_transform(x, hover_crs)

    hover_points <- suppressWarnings(sf::st_point_on_surface(x_for_points))

    hover_points <- sf::st_transform(hover_points, original_crs)
  } else {
    hover_points <- suppressWarnings(sf::st_point_on_surface(x))
  }

  hover_points
}


#' Prepare cluster result and shape file for mapping
#'
#' Function ingest an clusters object as returned by \code{find_clusters()}, a
#' shape file, and a unique identifier for the locations in that shape file, and
#' prepares these objects for mapping.
#'
#' @param cl an object of class "clusters" as returned by \code{find_clusters()}
#' @param s shape file; must be of class sf
#' @param s_id string unique identifier of `s`
#' @param hover_crs optional coordinate reference system used to compute
#'   representative hover points when `s` is in longitude/latitude coordinates.
#'   If `NULL`, EPSG:3857 is used as a general-purpose fallback. The resulting
#'   hover points are transformed back to the CRS of `s` before plotting

#'
#' @export
prepare_map_data <- function(cl, s, s_id, hover_crs = NULL) {
  validate_clusters(cl)
  validate_sf_file(s, s_id)

  s_ids <- as.character(s[[s_id]])
  cl_ids <- as.character(cl$cluster_location_counts$location)

  missing_from_shape <- setdiff(cl_ids, s_ids)

  if (length(missing_from_shape) > 0L) {
    cli::cli_abort(c(
      "Some cluster locations are not present in the shape file.",
      "x" = paste(head(missing_from_shape, 10), collapse = ", ")
    ))
  }

  s <- data.table::setDT(data.table::copy(s))
  s[, (s_id) := as.character(get(s_id))]

  cl_locs <- data.table::copy(cl$cluster_location_counts)
  cl_locs[, location := as.character(location)]

  md <- sf::st_as_sf(
    data.table::merge.data.table(
      s,
      cl_locs,
      by.x = s_id,
      by.y = "location",
      all.x = TRUE
    )
  )

  cluster_centers <- cl[["cluster_alert_table"]][
    order(cluster_start_date, cluster_center_observed),
    cluster_center
  ]

  cluster_centers <- as.character(cluster_centers)

  color_values <- setNames(
    grDevices::colorRampPalette(c("lightblue", "darkblue"))(length(cluster_centers)),
    cluster_centers
  )

  hover_points <- get_hover_points(md, hover_crs = hover_crs)

  coords <- sf::st_coordinates(hover_points)

  hover_df <- data.frame(
    location_id = md[[s_id]],
    cluster_center = md$cluster_center,
    count = md$count,
    hover_x = coords[, 1],
    hover_y = coords[, 2],
    stringsAsFactors = FALSE
  )

  names(hover_df)[1] <- s_id

  hover_df$hover_text <- ifelse(
    is.na(hover_df$cluster_center),
    paste0("Location: ", hover_df[[s_id]]),
    paste0(
      "Location: ", hover_df[[s_id]],
      "<br>Cluster center: ", hover_df$cluster_center,
      "<br>Count: ", hover_df$count
    )
  )

  list(
    map_data = md,
    color_values = color_values,
    id_col = s_id,
    cluster_centers = cluster_centers,
    hover_data = hover_df
  )
}

#' Convert sf to plotly polygons
#'
#' Helper function that takes the shape file merged with cluster information
#' as returned by the "map_data" element of \code{prepare_map_data()} and
#' converts each location to long polygon coordinate rows suitable for plotly.
#'
#' This function only prepares drawing geometry. Hover information is prepared
#' separately in \code{prepare_map_data()} as the "hover_data" element.
#'
#' @param md list as returned by \code{prepare_map_data()}
#'
#' @keywords internal
sf_to_plotly_polygons <- function(md) {
  required_names <- c(
    "map_data",
    "color_values",
    "id_col",
    "cluster_centers",
    "hover_data"
  )

  if (!is.list(md) || !all(required_names %in% names(md))) {
    cli::cli_abort(
      "`md` must be a named list returned by `prepare_map_data()`."
    )
  }

  s_id <- md[["id_col"]]
  x <- md[["map_data"]]

  if (!inherits(x, "sf")) {
    cli::cli_abort("`md$map_data` must be an sf object.")
  }

  if (!is.character(s_id) || length(s_id) != 1L || is.na(s_id) || !nzchar(s_id)) {
    cli::cli_abort("`md$id_col` must be a single non-missing string.")
  }

  check_vars(x, c(s_id, "cluster_center", "count"))

  x <- sf::st_cast(x, "MULTIPOLYGON", warn = FALSE)

  out <- vector("list", nrow(x))

  for (i in seq_len(nrow(x))) {
    geom <- sf::st_geometry(x)[[i]]
    location_value <- x[[s_id]][i]

    pieces <- vector("list", length(geom) * 2L)
    k <- 1L

    for (poly_i in seq_along(geom)) {
      # For a MULTIPOLYGON, geom[[poly_i]] is one POLYGON.
      # poly[[1]] is the exterior ring.
      poly <- geom[[poly_i]]
      ring <- poly[[1]]

      d_poly <- data.frame(
        location_id = location_value,
        cluster_center = x$cluster_center[i],
        count = x$count[i],
        x = ring[, 1],
        y = ring[, 2],
        stringsAsFactors = FALSE
      )

      names(d_poly)[1] <- s_id

      d_break <- data.frame(
        location_id = location_value,
        cluster_center = x$cluster_center[i],
        count = x$count[i],
        x = NA_real_,
        y = NA_real_,
        stringsAsFactors = FALSE
      )

      names(d_break)[1] <- s_id

      pieces[[k]] <- d_poly
      pieces[[k + 1L]] <- d_break
      k <- k + 2L
    }

    out[[i]] <- do.call(rbind, pieces)
  }

  do.call(rbind, out)
}

#' Map Clusters
#'
#' Utility for mapping clusters. The function ingest an object `cl` as returned
#' by \code{find_clusters()} and a shape file provisioned by the caller, along
#' with a string name of a column in the shape file that uniquely defines the
#' locations. Note that this column will be used to merge with the clusters and
#' therefore must aligned with the labels in `cl` cluster locations. The function
#' returns a basic plotly map object that can be further modified by the user
#' @param cl an object of class "clusters" as returned by \code{find_clusters()}
#' @param s shape file; must be of class sf
#' @param s_id string unique identifier of `s`
#' @param hover_crs optional coordinate reference system used to compute
#'   representative hover points when `s` is in longitude/latitude coordinates.
#'   If `NULL`, EPSG:3857 is used as a general-purpose fallback. The resulting
#'   hover points are transformed back to the CRS of `s` before plotting
#'
#' @export

map_clusters <- function(cl, s, s_id, hover_crs = NULL) {
  md <- prepare_map_data(cl, s, s_id, hover_crs = hover_crs)

  cluster_centers <- md[["cluster_centers"]]
  color_values <- md[["color_values"]]
  hover_df <- md[["hover_data"]]

  poly_df <- sf_to_plotly_polygons(md)

  fig <- plotly::plot_ly()

  # ------------------------------------------------------------
  # 1. Base layer: all outlines, no hover
  # ------------------------------------------------------------

  fig <- fig %>%
    plotly::add_trace(
      data = poly_df,
      type = "scatter",
      mode = "lines",
      x = ~x,
      y = ~y,
      line = list(
        color = "black",
        width = 0.2
      ),
      hoverinfo = "skip",
      showlegend = FALSE
    )

  # ------------------------------------------------------------
  # 2. Overlay layer: fill clustered locations
  # ------------------------------------------------------------

  for (cc in cluster_centers) {
    dat_cc <- poly_df[
      !is.na(poly_df$cluster_center) &
        poly_df$cluster_center == cc,
    ]

    if (nrow(dat_cc) == 0L) {
      next
    }

    fig <- fig %>%
      plotly::add_trace(
        data = dat_cc,
        type = "scatter",
        mode = "lines",
        x = ~x,
        y = ~y,
        fill = "toself",
        fillcolor = color_values[[cc]],
        line = list(
          color = "black",
          width = 0.2
        ),
        name = cc,
        showlegend = FALSE,
        hoverinfo = "skip"
      )
  }

  # ------------------------------------------------------------
  # 3. Hover layer: one invisible marker per location
  # ------------------------------------------------------------

  fig <- fig %>%
    plotly::add_trace(
      data = hover_df,
      type = "scatter",
      mode = "markers",
      x = ~hover_x,
      y = ~hover_y,
      marker = list(
        size = 12,
        color = "rgba(0,0,0,0)",
        line = list(
          color = "rgba(0,0,0,0)"
        )
      ),
      text = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      showlegend = FALSE
    )

  # ------------------------------------------------------------
  # 4. Layout
  # ------------------------------------------------------------

  fig <- fig %>%
    plotly::layout(
      xaxis = list(
        visible = FALSE,
        scaleanchor = "y",
        scaleratio = 1
      ),
      yaxis = list(
        visible = FALSE
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )

  fig
}
