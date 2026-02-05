#' Estimate Nitrogen Rate using Holland & Schepers Method
#'
#' This function estimates nitrogen (N) application rates based on NDVI values using the Holland & Schepers (H&S) algorithm.
#' It calculates a sufficiency index (SI) for each pixel in the NDVI raster, then adjusts a base N rate to determine the recommended N dose.
#'
#' @param ndvi_raster A RasterLayer object containing NDVI values (0-1 scale).
#' @param base_N_rate The base N rate (kg/ha) to be adjusted (default = 50).
#' @param plot Logical indicating whether to create diagnostic plots (default = TRUE).
#'
#' @return A list containing:
#' - dose_raster: RasterLayer with estimated N rates (kg/ha)
#' - sufficiency_index_raster: RasterLayer with calculated sufficiency indices
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' # Load NDVI raster (replace with your data)
#' ndvi_raster <- raster(system.file("extdata/sample_ndvi.tif", package = "yourpackage"))
#'
#' # Calculate N rates with plotting
#' result <- estimate_N_rate_from_holland_schepers(ndvi_raster, base_N_rate = 60)
#'
#' # Visualize results
#' plot(result$dose_raster, main = "Recommended N Rates")
#' plot(result$sufficiency_index_raster, main = "Sufficiency Index")
#' }

estimate_N_rate_from_holland_schepers <- function(ndvi_raster, base_N_rate = 50, plot = TRUE) {
  # Validate inputs
  if (!inherits(ndvi_raster, "RasterLayer")) {
    stop("Input must be a single-layer Raster object.")
  }
  if (!raster::isLonLat(ndvi_raster) && is.na(raster::projection(ndvi_raster))) {
    warning("Raster projection information missing. Ensure NDVI values are in 0-1 range.")
  }
  if (base_N_rate <= 0 || base_N_rate > 300) {
    stop("Base N rate must be between 1-300 kg/ha.")
  }

  # Calculate reference percentiles
  q95 <- raster::quantile(ndvi_raster, probs = 0.95, na.rm = TRUE)
  q05 <- raster::quantile(ndvi_raster, probs = 0.05, na.rm = TRUE)

  # Handle low variability case
  delta_SI <- 1 - (q05 / q95)
  if (delta_SI <= 0.01) {
    warning(paste("Low NDVI variability (q95:", round(q95, 3),
                  " q05:", round(q05, 3),
                  "). Results may be unreliable."))
  }

  # Calculate Sufficiency Index components
  SI <- ndvi_raster / q95
  SI[SI > 1] <- 1  # Cap SI at 1
  sufficiency_index <- sqrt((1 - SI) / delta_SI)

  # Calculate final N dose
  dose_raster <- base_N_rate * sufficiency_index
  names(dose_raster) <- "Nrate_kg_ha"

  # Generate diagnostic plots
  if (plot) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

    # NDVI distribution
    raster::hist(ndvi_raster, main = "NDVI Distribution",
                 xlab = "NDVI", col = "darkgreen")

    # SI vs NDVI relationship
    plotSample <- raster::sampleRegular(ndvi_raster, size = 5000)
    plot(plotSample, raster::values(sufficiency_index)[!is.na(plotSample)],
         pch = ".", col = "blue",
         xlab = "NDVI", ylab = "Sufficiency Index",
         main = "SI-NDVI Relationship")

    # Dose distribution
    raster::hist(dose_raster, main = "N Rate Distribution",
                 xlab = "N Rate (kg/ha)", col = "darkred")

    # Spatial distribution
    raster::plot(dose_raster, main = "Spatial N Rate Distribution",
                 col = grDevices::hcl.colors(100, "YlOrBr", rev = TRUE))
  }

  return(list(
    dose_raster = dose_raster,
    sufficiency_index_raster = sufficiency_index
  ))
}
