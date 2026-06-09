# Extra GPR sanity checks (bundled scene pipeline lives in test-compute_NNI_from_S2.R)

test_that("estimate_biophysical errors when raster has wrong band count", {
  skip_if_not_installed("terra")
  skip_if_not_installed("jsonlite")
  td <- tempfile(fileext = ".tif")
  r <- terra::rast(ncols = 4, nrows = 4, nlyrs = 3, crs = "EPSG:4326")
  terra::values(r) <- runif(terra::ncell(r) * 3, 100, 8000)
  terra::writeRaster(r, td, overwrite = TRUE)
  expect_error(
    estimate_biophysical(
      raster_path = td,
      output_dir = tempdir(),
      variables = "LAI",
      block_rows = 16
    ),
    "Raster has"
  )
})
