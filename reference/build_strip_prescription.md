# Build a strip-by-strip prescription map from a field polygon

Generates a set of parallel, machine-width strips that tile a field
polygon along a user-supplied *A-B line* (or the longest edge of the
field, if no A-B line is given) and assigns a target N rate to every
strip according to the chosen variability method. The output is ready to
be passed to
[`export_prescription`](https://mcroci.github.io/NFert/reference/export_prescription.md)
for SHP / GeoJSON / ISOXML / John Deere / Trimble export.

## Usage

``` r
build_strip_prescription(
  field,
  machine_width = 24,
  cell_length = NULL,
  angle_deg = NULL,
  ab_line = NULL,
  variability = c("uniform", "calibration", "nni", "classes"),
  vi_raster = NULL,
  nni_raster = NULL,
  n_target = 160,
  min_dose = 40,
  max_dose = 220,
  vi_low = 0.35,
  vi_high = 0.8,
  thr_lo = 0.9,
  thr_hi = 1.1,
  n_classes = 5,
  preserve_mean = TRUE,
  crs_metric = 3857
)
```

## Arguments

- field:

  `sf` POLYGON (single feature) of the field boundary.

- machine_width:

  Numeric, working width of the spreader / spray boom in metres (default
  24).

- cell_length:

  Optional numeric. If supplied and \>0, every strip is further
  subdivided into rectangular cells of this length (metres) along the
  A-B direction, producing a true 2-D prescription grid. If `NULL` or 0,
  each strip is left as a single polygon (the classic "strip map").

- angle_deg:

  Optional numeric. Azimuth of the A-B line in degrees (0 = east, 90 =
  north). Overrides `ab_line` when supplied.

- ab_line:

  Optional `sf` LINESTRING giving the driving direction. If both
  `ab_line` and `angle_deg` are `NULL`, the long side of the field is
  used.

- variability:

  One of `"uniform"`, `"calibration"`, `"nni"`, `"classes"`.

- vi_raster:

  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or `raster::RasterLayer` with vegetation index (0-1). Required for
  `"calibration"` and `"classes"`.

- nni_raster:

  Same, with NNI values. Required for `"nni"`.

- n_target:

  Target field-level mean dose (kg N ha\\^{-1}\\).

- min_dose, max_dose:

  Numeric caps (kg N ha\\^{-1}\\).

- vi_low, vi_high:

  Two-point calibration anchors (`variability = "calibration"`). Below
  `vi_low` the dose is capped to `max_dose`; above `vi_high` to
  `min_dose`.

- thr_lo, thr_hi:

  NNI thresholds (`variability = "nni"`).

- n_classes:

  Integer number of equal-spaced dose classes for
  `variability = "classes"` (default 5).

- preserve_mean:

  Logical. If `TRUE` (default) the strip- level dose is rescaled so the
  area-weighted mean matches `n_target`. Guarantees mass-balance
  compliance with the balance-based field ceiling.

- crs_metric:

  EPSG code of a metric CRS used internally (default 3857, Web
  Mercator). Use a local UTM for higher accuracy on larger fields.

## Value

An `sf` object of strip polygons with columns `strip_id`, `dose` (kg N
ha\\^{-1}\\), `area_ha`, `mean_vi` or `mean_nni` when available. CRS
matches the input.

## Details

The builder works entirely in a metric CRS (Web Mercator by default);
the final `sf` object is returned in the original CRS of the input field
so that downstream tools see consistent coordinates.

## Variability methods

- `"uniform"`:

  Every strip receives the same `n_target` dose. Useful as a baseline.

- `"calibration"`:

  Samples the mean `vi_raster` value per strip and maps it to a dose via
  a two- or three-point calibration curve. The dose decreases with
  vigour (higher VI = less N).

- `"nni"`:

  Samples the mean `nni_raster` value per strip and translates it into
  the three agronomic zones of Lemaire & Gastal: deficient (NNI \<
  `thr_lo`) gets `max_dose`, optimal (`thr_lo` \<= NNI \<= `thr_hi`)
  gets `n_target`, excessive (NNI \> `thr_hi`) gets `min_dose`.

- `"classes"`:

  Assigns doses equal to
  `seq(min_dose, max_dose, length.out = n_classes)` based on the
  k-quantile class of the mean `vi_raster` value per strip. Useful when
  a categorical map is preferred.

Regardless of the method, the mean dose across the field is rescaled to
match `n_target` (mass-balance constraint) when `preserve_mean = TRUE`
(default).

## A-B line handling

When `ab_line` is `NULL`, the A-B direction is taken from the longest
edge of the polygon's minimum-area bounding rectangle (via
[`sf::st_minimum_rotated_rectangle()`](https://r-spatial.github.io/sf/reference/geos_unary.html)).
This typically recovers the "long side" convention farmers use in the
field. When `ab_line` is a `sf` `LINESTRING`, its first segment defines
the driving direction.

## See also

[`export_prescription`](https://mcroci.github.io/NFert/reference/export_prescription.md),
[`variable_rate_N`](https://mcroci.github.io/NFert/reference/variable_rate_N.md),
[`compute_NNI_from_S2`](https://mcroci.github.io/NFert/reference/compute_NNI_from_S2.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Pick one feature from the demo farm
ex <- system.file("extdata/example_farm.geojson", package = "NFert")
farm <- sf::st_read(ex, quiet = TRUE)
field <- farm[1, ]  # 5.2 ha silage-maize plot

# Uniform 180 kg N/ha, 24 m spreader
rx <- build_strip_prescription(field, machine_width = 24,
                                variability = "uniform",
                                n_target = 180)

# VI-based 40-180 band calibration, 36 m machine
rx2 <- build_strip_prescription(field, machine_width = 36,
                                 variability = "calibration",
                                 vi_raster = my_ndvi,
                                 n_target = 160,
                                 min_dose = 40, max_dose = 200)

export_prescription(rx, "strips.shp", format = "shp")
} # }
```
