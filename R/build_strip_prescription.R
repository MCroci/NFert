#' Build a strip-by-strip prescription map from a field polygon
#'
#' Generates a set of parallel, machine-width strips that tile a field
#' polygon along a user-supplied \emph{A-B line} (or the longest edge of
#' the field, if no A-B line is given) and assigns a target N rate to
#' every strip according to the chosen variability method. The output
#' is ready to be passed to \code{\link{export_prescription}} for SHP /
#' GeoJSON / ISOXML / John Deere / Trimble export.
#'
#' The builder works entirely in a metric CRS (Web Mercator by default);
#' the final \code{sf} object is returned in the original CRS of the
#' input field so that downstream tools see consistent coordinates.
#'
#' @section Variability methods:
#' \describe{
#'   \item{\code{"uniform"}}{Every strip receives the same \code{n_target}
#'     dose. Useful as a baseline.}
#'   \item{\code{"calibration"}}{Samples the mean \code{vi_raster} value
#'     per strip and maps it to a dose via a two- or three-point
#'     calibration curve. The dose decreases with vigour (higher VI =
#'     less N).}
#'   \item{\code{"nni"}}{Samples the mean \code{nni_raster} value per
#'     strip and translates it into the three agronomic zones of
#'     Lemaire & Gastal: deficient (NNI < \code{thr_lo}) gets
#'     \code{max_dose}, optimal (\code{thr_lo} <= NNI <= \code{thr_hi})
#'     gets \code{n_target}, excessive (NNI > \code{thr_hi}) gets
#'     \code{min_dose}.}
#'   \item{\code{"classes"}}{Assigns doses equal to
#'     \code{seq(min_dose, max_dose, length.out = n_classes)} based on
#'     the k-quantile class of the mean \code{vi_raster} value per
#'     strip. Useful when a categorical map is preferred.}
#' }
#' Regardless of the method, the mean dose across the field is rescaled
#' to match \code{n_target} (mass-balance constraint) when
#' \code{preserve_mean = TRUE} (default).
#'
#' @section A-B line handling:
#' When \code{ab_line} is \code{NULL}, the A-B direction is taken from
#' the longest edge of the polygon's minimum-area bounding rectangle
#' (via \code{sf::st_minimum_rotated_rectangle()}). This typically
#' recovers the "long side" convention farmers use in the field. When
#' \code{ab_line} is a \code{sf} \code{LINESTRING}, its first segment
#' defines the driving direction.
#'
#' @param field \code{sf} POLYGON (single feature) of the field boundary.
#' @param machine_width Numeric, working width of the spreader /
#'   spray boom in metres (default 24).
#' @param cell_length Optional numeric. If supplied and >0, every strip
#'   is further subdivided into rectangular cells of this length
#'   (metres) along the A-B direction, producing a true 2-D
#'   prescription grid. If \code{NULL} or 0, each strip is left as a
#'   single polygon (the classic "strip map").
#' @param angle_deg Optional numeric. Azimuth of the A-B line in
#'   degrees (0 = east, 90 = north). Overrides \code{ab_line} when
#'   supplied.
#' @param ab_line Optional \code{sf} LINESTRING giving the driving
#'   direction. If both \code{ab_line} and \code{angle_deg} are
#'   \code{NULL}, the long side of the field is used.
#' @param variability One of \code{"uniform"}, \code{"calibration"},
#'   \code{"nni"}, \code{"classes"}.
#' @param vi_raster \code{terra::SpatRaster} or \code{raster::RasterLayer}
#'   with vegetation index (0-1). Required for \code{"calibration"} and
#'   \code{"classes"}.
#' @param nni_raster Same, with NNI values. Required for
#'   \code{"nni"}.
#' @param n_target Target field-level mean dose (kg N ha\eqn{^{-1}}).
#' @param min_dose,max_dose Numeric caps (kg N ha\eqn{^{-1}}).
#' @param vi_low,vi_high Two-point calibration anchors
#'   (\code{variability = "calibration"}). Below \code{vi_low} the dose
#'   is capped to \code{max_dose}; above \code{vi_high} to
#'   \code{min_dose}.
#' @param thr_lo,thr_hi NNI thresholds (\code{variability = "nni"}).
#' @param n_classes Integer number of equal-spaced dose classes for
#'   \code{variability = "classes"} (default 5).
#' @param preserve_mean Logical. If \code{TRUE} (default) the strip-
#'   level dose is rescaled so the area-weighted mean matches
#'   \code{n_target}. Guarantees mass-balance compliance with the
#'   balance-based field ceiling.
#' @param crs_metric EPSG code of a metric CRS used internally (default
#'   3857, Web Mercator). Use a local UTM for higher accuracy on larger
#'   fields.
#'
#' @return An \code{sf} object of strip polygons with columns
#'   \code{strip_id}, \code{dose} (kg N ha\eqn{^{-1}}), \code{area_ha},
#'   \code{mean_vi} or \code{mean_nni} when available. CRS matches the
#'   input.
#'
#' @seealso \code{\link{export_prescription}},
#'   \code{\link{variable_rate_N}},
#'   \code{\link{compute_NNI_from_S2}}
#'
#' @examples
#' \dontrun{
#' # Pick one feature from the demo farm
#' ex <- system.file("extdata/example_farm.geojson", package = "NFert")
#' farm <- sf::st_read(ex, quiet = TRUE)
#' field <- farm[1, ]  # 5.2 ha silage-maize plot
#'
#' # Uniform 180 kg N/ha, 24 m spreader
#' rx <- build_strip_prescription(field, machine_width = 24,
#'                                 variability = "uniform",
#'                                 n_target = 180)
#'
#' # VI-based 40-180 band calibration, 36 m machine
#' rx2 <- build_strip_prescription(field, machine_width = 36,
#'                                  variability = "calibration",
#'                                  vi_raster = my_ndvi,
#'                                  n_target = 160,
#'                                  min_dose = 40, max_dose = 200)
#'
#' export_prescription(rx, "strips.shp", format = "shp")
#' }
#' @export
build_strip_prescription <- function(
  field,
  machine_width = 24,
  cell_length   = NULL,
  angle_deg     = NULL,
  ab_line       = NULL,
  variability   = c("uniform", "calibration", "nni", "classes"),
  vi_raster     = NULL,
  nni_raster    = NULL,
  n_target      = 160,
  min_dose      = 40,
  max_dose      = 220,
  vi_low        = 0.35,
  vi_high       = 0.80,
  thr_lo        = 0.90,
  thr_hi        = 1.10,
  n_classes     = 5,
  preserve_mean = TRUE,
  crs_metric    = 3857
) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required for build_strip_prescription().")

  variability <- match.arg(variability)
  stopifnot(machine_width > 0, n_target >= 0,
             min_dose <= max_dose)

  # --- Normalise field to one polygon -------------------------------
  if (inherits(field, "sf")) {
    if (nrow(field) > 1)
      field <- field[1, , drop = FALSE]
  } else if (inherits(field, "sfc") || inherits(field, "sfg")) {
    field <- sf::st_sf(geometry = sf::st_sfc(field))
  } else {
    stop("`field` must be an sf / sfc / sfg polygon.")
  }
  crs_in <- sf::st_crs(field)

  # Reproject to metric CRS
  fld <- sf::st_transform(field, crs_metric)
  if (!sf::st_is_valid(fld)) fld <- sf::st_make_valid(fld)

  # --- Resolve the A-B azimuth --------------------------------------
  ab_azim <- if (!is.null(angle_deg) && is.finite(angle_deg))
    angle_deg * pi / 180
  else .resolve_ab_azimuth(fld, ab_line, crs_metric)

  # Field centroid (rotation centre)
  ctr <- sf::st_coordinates(sf::st_centroid(fld))[1, ]

  # --- Rotate the field so the A-B line is parallel to the X axis --
  rot <- function(geom, angle, pivot) {
    # angle in radians; positive = counter-clockwise
    R <- matrix(c(cos(angle), sin(angle),
                  -sin(angle), cos(angle)),
                nrow = 2, byrow = TRUE)
    (geom - pivot) * R + pivot
  }
  fld_rot_geom <- rot(sf::st_geometry(fld), -ab_azim, ctr)
  fld_rot <- sf::st_sf(geometry = fld_rot_geom, crs = crs_metric)
  if (!sf::st_is_valid(fld_rot)) fld_rot <- sf::st_make_valid(fld_rot)

  bb <- sf::st_bbox(fld_rot)

  # --- Generate horizontal strips (+ optional along-strip cells) ---
  pad_y <- machine_width * 0.5
  y_breaks <- seq(bb$ymin - pad_y, bb$ymax + pad_y, by = machine_width)
  if (utils::tail(y_breaks, 1) < bb$ymax)
    y_breaks <- c(y_breaks, utils::tail(y_breaks, 1) + machine_width)

  # X-axis breaks: a single x-span when cell_length is NULL / 0,
  # otherwise subdivide the strip into cell_length chunks.
  use_grid <- !is.null(cell_length) &&
              is.finite(cell_length) && cell_length > 0
  if (use_grid) {
    pad_x <- cell_length * 0.5
    x_breaks <- seq(bb$xmin - pad_x, bb$xmax + pad_x, by = cell_length)
    if (utils::tail(x_breaks, 1) < bb$xmax)
      x_breaks <- c(x_breaks, utils::tail(x_breaks, 1) + cell_length)
  } else {
    pad_x <- machine_width * 0.5
    x_breaks <- c(bb$xmin - pad_x, bb$xmax + pad_x)
  }

  polys <- list()
  for (j in seq_len(length(y_breaks) - 1)) {
    y_lo <- y_breaks[j]; y_hi <- y_breaks[j + 1]
    for (i in seq_len(length(x_breaks) - 1)) {
      x_lo <- x_breaks[i]; x_hi <- x_breaks[i + 1]
      polys[[length(polys) + 1L]] <- sf::st_polygon(list(rbind(
        c(x_lo, y_lo), c(x_hi, y_lo),
        c(x_hi, y_hi), c(x_lo, y_hi),
        c(x_lo, y_lo))))
    }
  }
  strips_rot_sfc <- sf::st_sfc(polys, crs = crs_metric)

  # --- Rotate strips back and clip to the field --------------------
  strips_sfc <- rot(strips_rot_sfc, ab_azim, ctr)
  strips_sfc <- sf::st_set_crs(strips_sfc, crs_metric)
  strips <- sf::st_sf(strip_id = seq_along(strips_sfc),
                      geometry = strips_sfc)
  strips <- suppressWarnings(sf::st_intersection(strips, fld))
  strips <- strips[sf::st_geometry_type(strips) %in%
                    c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]
  strips$strip_id <- seq_len(nrow(strips))
  strips$area_ha  <- as.numeric(sf::st_area(strips)) / 1e4

  # --- Per-strip value sampling for variability methods ------------
  mean_vi  <- rep(NA_real_, nrow(strips))
  mean_nni <- rep(NA_real_, nrow(strips))

  if (variability %in% c("calibration", "classes") && !is.null(vi_raster)) {
    mean_vi <- .strip_zonal_mean(strips, vi_raster)
    strips$mean_vi <- mean_vi
  }
  if (variability == "nni" && !is.null(nni_raster)) {
    mean_nni <- .strip_zonal_mean(strips, nni_raster)
    strips$mean_nni <- mean_nni
  }

  # --- Compute per-strip dose --------------------------------------
  strips$dose <- switch(variability,
    uniform     = rep(n_target, nrow(strips)),
    calibration = .dose_from_calibration(mean_vi, min_dose, max_dose,
                                          vi_low, vi_high),
    nni         = .dose_from_nni(mean_nni, n_target,
                                  min_dose, max_dose, thr_lo, thr_hi),
    classes     = .dose_from_classes(mean_vi, min_dose, max_dose,
                                      n_classes)
  )
  strips$dose[!is.finite(strips$dose)] <- n_target
  strips$dose <- pmin(pmax(strips$dose, min_dose), max_dose)

  # Preserve the field-level mean (mass-balance constraint)
  if (isTRUE(preserve_mean) && sum(strips$area_ha) > 0) {
    mean_curr <- sum(strips$dose * strips$area_ha, na.rm = TRUE) /
                 sum(strips$area_ha, na.rm = TRUE)
    if (is.finite(mean_curr) && mean_curr > 0) {
      strips$dose <- strips$dose * (n_target / mean_curr)
      strips$dose <- pmin(pmax(strips$dose, min_dose), max_dose)
    }
  }

  strips$dose <- round(strips$dose, 1)

  # --- Back to input CRS --------------------------------------------
  strips <- sf::st_transform(strips, crs_in)
  strips
}

# ---------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------

.resolve_ab_azimuth <- function(fld, ab_line, crs_metric) {
  if (!is.null(ab_line)) {
    ab <- sf::st_transform(ab_line, crs_metric)
    cc <- sf::st_coordinates(ab)
    if (nrow(cc) < 2) stop("`ab_line` must contain at least two vertices.")
    return(atan2(cc[2, "Y"] - cc[1, "Y"],
                 cc[2, "X"] - cc[1, "X"]))
  }
  # Default: azimuth of the longest side of the minimum rotated bbox
  mrr <- tryCatch(sf::st_minimum_rotated_rectangle(fld),
                  error = function(e) sf::st_convex_hull(fld))
  cc  <- sf::st_coordinates(mrr)[, 1:2, drop = FALSE]
  # Close the ring and compute segment lengths + azimuths
  n   <- nrow(cc) - 1
  lens <- numeric(n); azs <- numeric(n)
  for (i in seq_len(n)) {
    dx <- cc[i + 1, 1] - cc[i, 1]
    dy <- cc[i + 1, 2] - cc[i, 2]
    lens[i] <- sqrt(dx^2 + dy^2)
    azs[i]  <- atan2(dy, dx)
  }
  azs[which.max(lens)]
}

.strip_zonal_mean <- function(strips, rast) {
  if (inherits(rast, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE))
      stop("Package 'terra' is required for SpatRaster zonal stats.")
    v <- terra::extract(rast, terra::vect(strips),
                        fun = mean, na.rm = TRUE)
    # terra::extract returns a data.frame with ID + one column per band
    as.numeric(v[, 2])
  } else if (inherits(rast, c("RasterLayer", "RasterStack", "RasterBrick"))) {
    if (!requireNamespace("raster", quietly = TRUE))
      stop("Package 'raster' is required for RasterLayer zonal stats.")
    as.numeric(raster::extract(rast, sf::as_Spatial(strips),
                                fun = mean, na.rm = TRUE))
  } else {
    rep(NA_real_, nrow(strips))
  }
}

.dose_from_calibration <- function(vi, min_dose, max_dose,
                                   vi_low, vi_high) {
  if (all(is.na(vi))) return(rep(NA_real_, length(vi)))
  # Linear, decreasing: vi_low -> max_dose, vi_high -> min_dose
  slope <- (min_dose - max_dose) / (vi_high - vi_low)
  intercept <- max_dose - slope * vi_low
  d <- slope * vi + intercept
  pmin(pmax(d, min_dose), max_dose)
}

.dose_from_nni <- function(nni, n_target, min_dose, max_dose,
                           thr_lo, thr_hi) {
  d <- rep(n_target, length(nni))
  d[!is.na(nni) & nni <  thr_lo] <- max_dose   # deficient
  d[!is.na(nni) & nni >  thr_hi] <- min_dose   # excessive
  d
}

.dose_from_classes <- function(vi, min_dose, max_dose, n_classes) {
  if (all(is.na(vi))) return(rep(NA_real_, length(vi)))
  breaks <- stats::quantile(vi, probs = seq(0, 1, length.out = n_classes + 1),
                             na.rm = TRUE, names = FALSE)
  breaks[1] <- breaks[1] - 1e-9
  bin <- cut(vi, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  doses <- seq(max_dose, min_dose, length.out = n_classes)
  doses[bin]
}
