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

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with NDVI (0–1); the layer named `NDVI` is used if present, else the
  first layer. Legacy `raster` objects are accepted and converted with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).

- base_N_rate:

  The base N rate (kg/ha) to be adjusted (default = 50).

- plot:

  Logical indicating whether to create diagnostic plots (default =
  TRUE).

## Value

A list containing:

- dose_raster:
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with estimated N rates (kg/ha)

- sufficiency_index_raster:
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with calculated sufficiency indices

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)
# Load NDVI raster (replace with your data)
ndvi_raster <- terra::rast(system.file("extdata/sample_ndvi.tif", package = "yourpackage"))

# Calculate N rates with plotting
result <- estimate_N_rate_from_holland_schepers(ndvi_raster, base_N_rate = 60)

# Visualize results
terra::plot(result$dose_raster, main = "Recommended N Rates")
terra::plot(result$sufficiency_index_raster, main = "Sufficiency Index")
} # }
```
