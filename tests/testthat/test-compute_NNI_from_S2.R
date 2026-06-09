# End-to-end test of the Sentinel-2 -> NNI pipeline.
#
# The bundled scene is `inst/extdata/sentinel-2/S2_field001_20240711.tif`,
# a 10-band L2A reflectance subset (peak vegetation, July 11 2024) that
# fits in ~5 KB. It drives `estimate_biophysical()` against the bundled
# GPR JSON models in `inst/extdata/SENTINEL2_L2A_*.json` and feeds the
# LAI / Cm / CNC_Cprot / FVC outputs to `compute_NNI_from_S2()`.
#
# Tests are skipped if the suggested packages `terra` or `jsonlite` are
# not installed (both pipeline stages depend on them).

scene_path <- system.file("extdata/sentinel-2/S2_field001_20240711.tif",
                          package = "NFert")

test_that("Sentinel-2 demo scene is bundled", {
  expect_true(nzchar(scene_path))
  expect_true(file.exists(scene_path))
})

test_that("estimate_biophysical -> compute_NNI_from_S2 runs end-to-end", {
  skip_if_not_installed("terra")
  skip_if_not_installed("jsonlite")

  # 1. GPR retrieval (LAI / Cm / CNC_Cprot / FVC) -- output to tempdir
  out_dir <- file.path(tempdir(), "NFert-NNI-test")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  maps <- estimate_biophysical(
    raster_path = scene_path,
    output_dir  = out_dir,
    variables   = c("LAI", "Cm", "CNC_Cprot", "FVC"),
    block_rows  = 32         # small block, scene is tiny
  )

  expect_type(maps, "list")
  for (v in c("LAI", "Cm", "CNC_Cprot", "FVC")) {
    expect_s4_class(maps[[v]], "SpatRaster")
    # The on-disk path attribute must still point to a real file.
    expect_true(file.exists(attr(maps, "paths")[[v]]),
                info = sprintf("Missing on-disk file for %s", v))
  }

  # 2. NNI pipeline -- maize calibration
  nni <- compute_NNI_from_S2(
    lai_rast  = maps$LAI,
    cm_rast   = maps$Cm,
    cnc_rast  = maps$CNC_Cprot,
    cnc_layer = "CNC_Cprot",
    crop      = "maize",
    fvc       = maps$FVC
  )

  expect_type(nni, "list")
  for (lyr in c("W", "N_actual", "N_crit", "NNI", "zones", "mask"))
    expect_true(lyr %in% names(nni),
                info = sprintf("Missing layer: %s", lyr))

  # NNI raster must be SpatRaster
  expect_s4_class(nni$NNI,   "SpatRaster")
  expect_s4_class(nni$zones, "SpatRaster")

  # Sanity checks on values (only over masked-in pixels)
  nni_vals <- terra::values(nni$NNI, na.rm = TRUE)
  if (length(nni_vals) > 0) {
    expect_true(all(is.finite(nni_vals)))
    # Plausible range for a peak-summer scene, allow generous bounds.
    expect_true(min(nni_vals) >= 0)
    expect_true(max(nni_vals) <= 5)
  }

  zone_vals <- terra::values(nni$zones, na.rm = TRUE)
  if (length(zone_vals) > 0) {
    expect_setequal(unique(as.integer(zone_vals)),
                    intersect(1:3, unique(as.integer(zone_vals))))
  }
})

test_that("compute_NNI_from_S2 errors on unknown crop", {
  skip_if_not_installed("terra")
  skip_if_not_installed("jsonlite")

  out_dir <- file.path(tempdir(), "NFert-NNI-test-bad-crop")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  maps <- estimate_biophysical(
    raster_path = scene_path,
    output_dir  = out_dir,
    variables   = c("LAI", "Cm", "CNC_Cprot"),
    block_rows  = 32
  )
  expect_error(
    compute_NNI_from_S2(
      lai_rast  = maps$LAI,
      cm_rast   = maps$Cm,
      cnc_rast  = maps$CNC_Cprot,
      cnc_layer = "CNC_Cprot",
      crop      = "Triticum_unobtainium"
    ),
    "Unknown crop"
  )
})
