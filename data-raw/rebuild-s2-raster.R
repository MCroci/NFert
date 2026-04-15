# ---------------------------------------------------------------------------
# data-raw/rebuild-s2-raster.R
#
# Rebuild s2.rast for compatibility with the current `raster` package (>=3.5
# introduced the "srs" slot in RasterBrick that older serialisations lack).
# Run once if devtools::document() warns about an invalid validObject() with
# message "no slot of name 'srs'".
#
# Usage (from RStudio with the package as project root):
#   source("data-raw/rebuild-s2-raster.R")
# ---------------------------------------------------------------------------

stopifnot(file.exists("DESCRIPTION"))
if (!requireNamespace("raster", quietly = TRUE)) {
  stop("Package 'raster' is required.")
}

# Load the existing raster (suppress the validObject warning during load)
suppressWarnings(load("data/s2.rast.rda"))

# Force a clean rebuild by extracting values + extent + crs and constructing
# a fresh RasterBrick / RasterLayer with the current class definition.
if (inherits(s2.rast, "RasterBrick")) {
  vals <- raster::getValues(s2.rast)
  ext  <- raster::extent(s2.rast)
  crs  <- raster::crs(s2.rast)
  nr   <- nrow(s2.rast); nc <- ncol(s2.rast); nl <- raster::nlayers(s2.rast)
  fresh <- raster::brick(nrows = nr, ncols = nc, nl = nl,
                         xmn = ext@xmin, xmx = ext@xmax,
                         ymn = ext@ymin, ymx = ext@ymax,
                         crs = crs)
  raster::values(fresh) <- vals
  names(fresh) <- names(s2.rast)
  s2.rast <- fresh
} else if (inherits(s2.rast, "RasterLayer")) {
  vals <- raster::getValues(s2.rast)
  ext  <- raster::extent(s2.rast)
  crs  <- raster::crs(s2.rast)
  fresh <- raster::raster(nrows = nrow(s2.rast), ncols = ncol(s2.rast),
                          xmn = ext@xmin, xmx = ext@xmax,
                          ymn = ext@ymin, ymx = ext@ymax,
                          crs = crs)
  raster::values(fresh) <- vals
  names(fresh) <- names(s2.rast)
  s2.rast <- fresh
}

save(s2.rast, file = "data/s2.rast.rda", compress = "xz", version = 2)
message("Rebuilt data/s2.rast.rda with current raster class definition.")
