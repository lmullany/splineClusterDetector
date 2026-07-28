#' Validates cluster object
#'
#' Function ingests an object and checks its validity as an object returned
#' from \code{find_clusters()}
#'
#' @param cl object to validate
#' @keywords internal

validate_clusters <- function(cl) {
  if (!inherits(cl, "clusters")) {
    cli::cli_abort(paste0(
      "`cl` must be an object of class 'clusters'",
      " as returned by `find_clusters()`."
    ))
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
    cli::cli_abort(
      "`cl$cluster_location_counts$location` contains missing values."
    )
  }

  if (anyNA(cl$cluster_location_counts$cluster_center)) {
    cli::cli_abort(
      "`cl$cluster_location_counts$cluster_center` contains missing values."
    )
  }

  if (anyDuplicated(cl$cluster_location_counts$location)) {
    cli::cli_abort(paste0(
      "`cl$cluster_location_counts$location` ",
      "must contain at most one row per mapped location."
    ))
  }

  invisible(TRUE)
}


#' Validates sf file
#'
#' Function ingests an object and check if this is of class sf, and has an
#' unique identifer for its rows
#'
#' @param s shape file; must be of class sf
#' @param s_id string unique identifier of `s`
#' @param label_id string column of `s` that indicates display label for the row
#'   in `s` (default is NULL)
#' @keywords internal
validate_sf_file <- function(s, s_id, label_id = NULL) {
  if (!inherits(s, "sf")) {
    cli::cli_abort("`s` must be an object of class `sf`.")
  }

  if (
    !is.character(s_id) ||
      length(s_id) != 1L ||
      is.na(s_id) ||
      !nzchar(s_id)
  ) {
    cli::cli_abort("`s_id` must be a single non-missing string.")
  }

  check_vars(s, c(s_id, label_id))

  if (!is.null(label_id)) {
    if (
      !is.character(label_id) ||
        length(label_id) != 1L ||
        is.na(label_id) ||
        !nzchar(label_id)
    ) {
      cli::cli_abort("`label_id` must be NULL or a  single non-missing string.")
    }
  }

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


#' Compute representative data points for sf polygons
#'
#' Computes one point per input feature for use in plotly hover layers. If the
#' input geometry is longitude/latitude, the point-on-surface operation is
#' performed in a projected CRS and then transformed back to the original CRS.
#'
#' @param x sf object
#' @param point_crs CRS to use when `x` is longitude/latitude. If NULL,
#'   EPSG:3857 is used as a general-purpose fallback.
#'
#' @keywords internal
get_point_data <- function(x, point_crs = NULL) {
  if (!inherits(x, "sf")) {
    cli::cli_abort("`x` must be an sf object.")
  }

  original_crs <- sf::st_crs(x)

  if (is.na(original_crs)) {
    cli::cli_abort(
      paste0(
        "`x` must have a valid coordinate reference ",
        "system before points can be computed."
      )
    )
  }

  use_projection <- sf::st_is_longlat(x)

  if (use_projection) {
    if (is.null(point_crs)) {
      point_crs <- 3857
    }

    x_for_points <- sf::st_transform(x, point_crs)

    points <- suppressWarnings(sf::st_point_on_surface(x_for_points))

    points <- sf::st_transform(points, original_crs)
  } else {
    points <- suppressWarnings(sf::st_point_on_surface(x))
  }

  points
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
#' @param label_id string column of `s` that indicates display label for the row
#'   in `s` (default is NULL)
#' @param point_crs optional coordinate reference system used to compute
#'   representative points when `s` is in longitude/latitude coordinates.
#'   If `NULL`, EPSG:3857 is used as a general-purpose fallback. The resulting
#'   points are transformed back to the CRS of `s` before plotting

#'
#' @export
#' @examplesIf requireNamespace("tigris", quietly = TRUE)
#' # example code
#' # get some data
#' dd <- example_count_data[, max(date)]
#' # get a distance matrix
#' dm <- create_dist_list("county", 50, st = "OH")
#' # find the clusters
#' cl <- find_clusters(
#'   cases = example_count_data,
#'   detect_date = dd,
#'   distance_matrix = dm
#' )
#' # get shape file
#' ohio_shape <- tigris::counties("OH", cb = TRUE, class = "sf")
#'
#' # prepare map data
#' md <- prepare_map_data(cl, ohio_shape, "GEOID")
#' md <- prepare_map_data(cl, ohio_shape, "GEOID", label_id = "NAME")

#'
prepare_map_data <- function(cl, s, s_id, label_id = NULL, point_crs = NULL) {
  location <- cluster_start_date <- cluster_center_observed <- NULL
  cluster_center <- NULL
  validate_clusters(cl)
  validate_sf_file(s, s_id, label_id)

  s_ids <- as.character(s[[s_id]])
  cl_ids <- as.character(cl$cluster_location_counts$location)

  missing_from_shape <- setdiff(cl_ids, s_ids)

  if (length(missing_from_shape) > 0L) {
    cli::cli_abort(c(
      "Some cluster locations are not present in the shape file.",
      "x" = paste(utils::head(missing_from_shape, 10), collapse = ", ")
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

  color_values <- stats::setNames(
    grDevices::colorRampPalette(
      c("lightblue", "darkblue")
    )(length(cluster_centers)),
    cluster_centers
  )

  point_data <- get_point_data(md, point_crs = point_crs)

  coords <- sf::st_coordinates(point_data)

  point_df <- data.frame(
    location_id = md[[s_id]],
    label_text = {
      if (is.null(label_id)) md[[s_id]] else md[[label_id]]
    },
    cluster_center = md$cluster_center,
    count = md$count,
    hover_x = coords[, 1],
    hover_y = coords[, 2],
    stringsAsFactors = FALSE
  )

  names(point_df)[1] <- s_id

  point_df$hover_text <- ifelse(
    is.na(point_df$cluster_center),
    paste0("Location: ", point_df$label_id),
    paste0(
      "Location: ", point_df$label_text,
      "<br>Cluster center: ", point_df$cluster_center,
      "<br>Count: ", point_df$count
    )
  )

  list(
    map_data = md,
    color_values = color_values,
    id_col = s_id,
    cluster_centers = cluster_centers,
    point_data = point_df,
    label_id = {
      if (is.null(label_id)) s_id else label_id
    }
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
    "point_data",
    "label_id"
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

  if (
    !is.character(s_id) || length(s_id) != 1L || is.na(s_id) || !nzchar(s_id)
  ) {
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
#' therefore must aligned with the labels in `cl` cluster locations. The
#' function returns a basic plotly map object that can be further modified by
#' the user
#' @param cl an object of class "clusters" as returned by \code{find_clusters()}
#' @param s shape file; must be of class sf
#' @param s_id string unique identifier of `s`
#' @param label_id string column of `s` that indicates display label for the row
#'   in `s` (default is NULL)
#' @param label for \code{engine = "ggplot"}, indicates which locations should
#'   receive visible text labels. Valid choices are \code{"none"},
#'   \code{"cluster_centers"}, \code{"cluster_locations"}, and \code{"all"}. The
#'   default is \code{"none"}. This argument is ignored when \code{engine =
#'   "plotly"} because plotly maps always include hover labels for all
#'   locations.
#' @param engine string label to indicate plotting engine; either "plotly"
#'   (default) or "ggplot"
#' @param point_crs optional coordinate reference system used to compute
#'   representative points when `s` is in longitude/latitude coordinates. If
#'   `NULL`, EPSG:3857 is used as a general-purpose fallback. The resulting
#'   points are transformed back to the CRS of `s` before plotting
#'
#'
#' @examples
#' if (
#'   requireNamespace("tigris", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)
#' ) {
#'   # get some data
#'   dd <- example_count_data[, max(date)]
#'   # get a distance matrix
#'   dm <- create_dist_list("county", 50, st = "OH")
#'   # find the clusters
#'   cl <- find_clusters(
#'     cases = example_count_data,
#'     detect_date = dd,
#'     distance_matrix = dm
#'   )
#'   # get shape file
#'   ohio_shape <- tigris::counties("OH", cb = TRUE, class = "sf")
#'
#'   # prepare map data
#'   md <- map_clusters(cl, ohio_shape, "GEOID")
#' }
#' @export

map_clusters <- function(
  cl,
  s,
  s_id,
  label_id = NULL,
  label = c("none", "cluster_centers", "cluster_locations", "all"),
  engine = c("plotly", "ggplot"),
  point_crs = NULL
) {
  # get the plotting engine
  engine <- match.arg(engine)

  # resolve engine based on available backends
  engine <- resolve_plot_backend(engine, "map_clusters()")


  label <- match.arg(label)

  # prepare the map data
  md <- prepare_map_data(cl, s, s_id, label_id, point_crs = point_crs)


  if (engine == "plotly") {
    map_clusters_plotly(md)
  } else {
    map_clusters_ggplot(md, label = label)
  }
}

#' Render Cluster Map with ggplot2
#'
#' Internal helper used by \code{map_clusters()} when \code{engine = "ggplot"}.
#' The function takes prepared map data from \code{prepare_map_data()} and
#' returns a static \pkg{ggplot2} map. Locations are drawn from the merged sf
#' object, with clustered locations filled according to their cluster center.
#'
#' @param md named list as returned by \code{prepare_map_data()}.
#' @param label choice for labeling (defaults to "none")
#'
#' @return A \pkg{ggplot2} object.
#'
#' @keywords internal
map_clusters_ggplot <- function(
  md,
  label = c("none", "cluster_centers", "cluster_locations", "all")
) {
  label <- match.arg(label)

  label_text <- hover_x <- hover_y <- cluster_center <- NULL

  p <- ggplot2::ggplot(md[["map_data"]]) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = cluster_center),
      color = "black", linewidth = 0.2
    ) +
    ggplot2::scale_fill_manual(values = md[["color_values"]], na.value = NA) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  # labeling
  if (label != "none") {
    ld <- md[["point_data"]]
    # filter the point data, if label !="all"
    if (label == "cluster_centers") {
      ld <- ld[
        !is.na(ld$cluster_center) & ld[[md[["id_col"]]]] == ld$cluster_center,
      ]
    }
    if (label == "cluster_locations") {
      ld <- ld[!is.na(ld$cluster_center), ]
    }

    p <- p +
      ggplot2::geom_text(
        data = ld,
        ggplot2::aes(
          x = hover_x,
          y = hover_y,
          label = label_text
        ),
        size = 3
      )
  }
  p
}

#' Render Cluster Map with plotly
#'
#' Internal helper used by \code{map_clusters()} when \code{engine = "plotly"}.
#' The function takes prepared map data from \code{prepare_map_data()} and
#' returns an interactive \pkg{plotly} map. Polygon outlines and cluster fills
#' are drawn as scatter traces, while hover labels are provided by a separate
#' invisible point layer generated from the representative point data.
#'
#' @param md named list as returned by \code{prepare_map_data()}.
#'
#' @return A \pkg{plotly} htmlwidget object.
#'
#' @keywords internal
map_clusters_plotly <- function(md) {
  cluster_centers <- md[["cluster_centers"]]
  color_values <- md[["color_values"]]
  point_df <- md[["point_data"]]

  poly_df <- sf_to_plotly_polygons(md)

  fig <- plotly::plot_ly()

  # ------------------------------------------------------------
  # 1. Base layer: all outlines, no hover
  # ------------------------------------------------------------

  fig <- fig |>
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

    fig <- fig |>
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

  fig <- fig |>
    plotly::add_trace(
      data = point_df,
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

  fig <- fig |>
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
