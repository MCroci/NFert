.as_raster_layer_ndvi <- function(x) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package 'raster' is required for NDVI raster operations.", call. = FALSE)
  }
  if (inherits(x, "RasterLayer")) {
    return(x)
  }
  if (inherits(x, c("RasterBrick", "RasterStack"))) {
    nl <- raster::nlayers(x)
    if (nl < 1L) {
      stop("NDVI raster has no layers.", call. = FALSE)
    }
    layer <- 1L
    nm <- names(x)
    if (length(nm) >= nl) {
      nm_use <- nm[seq_len(nl)]
      idx <- which(!is.na(nm_use) & tolower(trimws(nm_use)) == "ndvi")
      if (length(idx)) {
        layer <- idx[1L]
      } else {
        idx <- grep("^ndvi", nm_use, ignore.case = TRUE)
        if (length(idx)) layer <- idx[1L]
      }
    }
    return(raster::raster(x, layer = layer))
  }
  stop("Input must be a raster::RasterLayer, RasterBrick, or RasterStack (NDVI layer).",
       call. = FALSE)
}

#' Nitrogen Rate Estimation from NDVI using Calibration
#'
#' This function estimates nitrogen (N) application rates based on NDVI values using either
#' a two-point or three-point calibration method.
#'
#' @param raster A `raster::RasterLayer` with NDVI values, or a `RasterBrick` /
#'   `RasterStack` (the layer named `NDVI` is used if present, otherwise the first layer).
#' @param minN The minimum N rate (kg/ha) corresponding to the minimum NDVI.
#' @param maxN The maximum N rate (kg/ha) corresponding to the maximum NDVI.
#' @param meanN The mean N rate (kg/ha) corresponding to the mean NDVI (only used for three-point calibration).
#' @param calibration_type The type of calibration to perform: "two-point" or "three-point". Default is "two-point".
#' @param plot A logical value indicating whether to create plots (default is FALSE).
#'
#' @return A RasterLayer object with the estimated N rates based on the chosen calibration method.
#' @export
#'
#' @examples
#' # Load example NDVI raster (replace with your own)
#' # ndvi_raster <- raster::raster(system.file("extdata/s2.tif", package = "NFert"))
#'
#' # Two-point calibration
#' # n_rate_raster_2pt <- estimate_N_rate_from_calibration_curve(ndvi_raster, minN = 40, maxN = 60)
#'
#' # Three-point calibration
#' # n_rate_raster_3pt <- estimate_N_rate_from_calibration_curve(ndvi_raster, minN = 40, meanN = 50,
#' # maxN = 60, calibration_type = "three-point")

estimate_N_rate_from_calibration_curve <- function(raster, minN, maxN, meanN = NULL,
                                                   calibration_type = "two-point", plot = FALSE) {
  raster <- .as_raster_layer_ndvi(raster)
  ndvi_values <- raster::getValues(raster)
  if (!is.numeric(ndvi_values)) {
    stop("NDVI raster values must be numeric.", call. = FALSE)
  }

  # Extract NDVI values and calculate statistics
  ndvi_min <- min(ndvi_values, na.rm = TRUE)
  ndvi_max <- max(ndvi_values, na.rm = TRUE)

  # Prepare calibration data
  if (calibration_type == "two-point") {
    df <- data.frame(
      x = c(ndvi_min, ndvi_max),
      y = c(maxN, minN)  # Inverse relationship NDVI-N rate
    )
  } else if (calibration_type == "three-point") {
    if (is.null(meanN)) {
      stop("Mean N rate (meanN) is required for three-point calibration.")
    }
    ndvi_mean <- mean(ndvi_values, na.rm = TRUE)
    df <- data.frame(
      x = c(ndvi_min, ndvi_mean, ndvi_max),
      y = c(maxN, meanN, minN)  # Inverse relationship
    )
  } else {
    stop("Invalid calibration_type. Choose 'two-point' or 'three-point'.")
  }

  # Fit linear model
  model <- stats::lm(y ~ x, data = df)

  # Predict N rates
  predicted <- stats::predict(model, newdata = data.frame(x = ndvi_values))
  n_rate_raster <- raster::raster(raster)
  n_rate_raster[] <- predicted
  names(n_rate_raster) <- "Nrate"

  # Generate plots if requested
  if (plot) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))

    graphics::par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

    # Calibration curve plot
    plot_range <- c(minN - 5, maxN + 5)
    ndvi_range <- range(ndvi_values, na.rm = TRUE)

    # Main calibration plot
    graphics::plot(df$x, df$y, pch = 19, col = "red", cex = 2,
                   xlim = ndvi_range, ylim = plot_range,
                   xlab = "NDVI", ylab = "N Rate (kg/ha)",
                   main = "Calibration Curve")
    graphics::abline(model, col = "blue", lty = 2, lwd = 2)

    # Add predicted points
    graphics::points(ndvi_values, predicted, pch = ".", col = "blue", cex = 1)

    # Histogram of N rates
    graphics::hist(predicted, main = "N Rate Distribution",
                   xlab = "N Rate (kg/ha)", col = "skyblue", border = "white")
  }

  return(n_rate_raster)
}
