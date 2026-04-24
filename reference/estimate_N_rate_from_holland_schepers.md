# Estimate Nitrogen Rate using Holland & Schepers Method

This function estimates nitrogen (N) application rates based on NDVI
values using the Holland & Schepers (H&S) algorithm. It calculates a
sufficiency index (SI) for each pixel in the NDVI raster, then adjusts a
base N rate to determine the recommended N dose.

## Usage

``` r
estimate_N_rate_from_holland_schepers(
  ndvi_raster,
  base_N_rate = 50,
  plot = TRUE
)
```

## Arguments

- ndvi_raster:

  A `raster::RasterLayer` with NDVI (0–1), or a `RasterBrick` /
  `RasterStack` (layer named `NDVI` if present, else first layer).

- base_N_rate:

  The base N rate (kg/ha) to be adjusted (default = 50).

- plot:

  Logical indicating whether to create diagnostic plots (default =
  TRUE).

## Value

A list containing:

- dose_raster: RasterLayer with estimated N rates (kg/ha)

- sufficiency_index_raster: RasterLayer with calculated sufficiency
  indices

## Examples

``` r
if (FALSE) { # \dontrun{
library(raster)
# Load NDVI raster (replace with your data)
ndvi_raster <- raster(system.file("extdata/sample_ndvi.tif", package = "yourpackage"))

# Calculate N rates with plotting
result <- estimate_N_rate_from_holland_schepers(ndvi_raster, base_N_rate = 60)

# Visualize results
plot(result$dose_raster, main = "Recommended N Rates")
plot(result$sufficiency_index_raster, main = "Sufficiency Index")
} # }
```
