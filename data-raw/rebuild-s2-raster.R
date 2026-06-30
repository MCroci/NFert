# ---------------------------------------------------------------------------
# data-raw/rebuild-s2-raster.R
#
# Rebuild data/s2.rast.rda as a terra PackedSpatRaster.
#
# SpatRaster objects hold an external C++ pointer and cannot be serialised to
# an .rda directly (they become a "NULL pointer" on load). We therefore store
# the example raster wrapped with terra::wrap() (a PackedSpatRaster, which IS
# serialisable) and unwrap it on use with terra::rast(s2.rast).
#
# This removes the dependency on the {raster} package: s2.rast now loads with
# {terra} only. Run once after changing the source raster:
#   source("data-raw/rebuild-s2-raster.R")
# ---------------------------------------------------------------------------

stopifnot(file.exists("DESCRIPTION"))
if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Package 'terra' is required.")
}

# Source of truth: the previous serialisation. Historically this was a
# {raster} RasterBrick; convert it to a SpatRaster (terra::rast() accepts a
# Raster* object). If the stored object is already packed, just unwrap it.
e <- new.env()
suppressWarnings(load("data/s2.rast.rda", envir = e))
obj <- e$s2.rast

sr <- if (inherits(obj, "PackedSpatRaster")) {
  terra::rast(obj)
} else if (inherits(obj, "SpatRaster")) {
  obj
} else {
  # legacy raster::RasterBrick / RasterLayer
  terra::rast(obj)
}

# Pack for serialisation.
s2.rast <- terra::wrap(sr)
save(s2.rast, file = "data/s2.rast.rda", compress = "xz", version = 2)
message(sprintf(
  "Rebuilt data/s2.rast.rda as PackedSpatRaster (%d layer(s): %s).",
  terra::nlyr(sr), paste(names(sr), collapse = ", ")))
