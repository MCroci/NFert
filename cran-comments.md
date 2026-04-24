# CRAN Submission Comments for NFert 0.13.0

## Package Information
- Package: NFert
- Version: 0.13.0
- Title: N, P and K fertilization following Emilia-Romagna DPI 2026
- Author: Michele Croci [aut, cre] <michele.croci@unicatt.it>
- Maintainer: Michele Croci <michele.croci@unicatt.it>

## Test Environments
- Local: Windows 10, R 4.5.1
- R-hub checks (to be run before submission)

## R CMD check results
(Will be updated after running devtools::check())

## Changes in this version (0.11.0) - Farm-level GeoJSON workflow

### Major
- New `farm_balance()` reads a vector layer of plots (GeoJSON,
  Shapefile, GeoPackage or sf object) and runs `N_balance()` on every
  feature, adding `N_target`, `MAS_cap`, `MAS_ok`, `N_total_kg` and
  optional `P2O5_target` / `K2O_target` columns. An `output` path
  writes the enriched layer back to disk.
- New Shiny tab "Farm / Azienda" visualises the plots on an
  interactive Leaflet map colour-ramped by N target, with per-plot
  bar chart, KPIs (total ha, total kg N, MAS breaches) and downloads.
- Runnable example `inst/extdata/example_farm.geojson` (8 plots,
  35.5 ha) carries all N_balance() columns so the workflow is
  testable out of the box.

### Dependencies
- Added `sf` and `leaflet` to Suggests.

## Changes in version 0.10.0 - Shiny web interface

### Major
- New `run_app()` launches a full-featured Shiny interface exposing
  N/P/K balance, distribution plan, variable-rate and S2->NNI
  pipelines through interactive forms and maps. Ships with six tabs,
  live MAS/ZVN validation, one-click VRT raster, and a "Research
  mode" toggle for coefficient overrides.
- The app lives in `inst/shinyapp/` with a modular layout; all
  dependencies (shiny >= 1.7.0, shinyjs, DT, ggplot2) are listed under
  Suggests and loaded lazily by `run_app()`.

### Dependencies
- Suggests: shiny (>= 1.7.0), shinyjs, DT, ggplot2 added. Base package
  still has no hard dependency beyond `raster`.

## Changes in version 0.9.0 - Crop-aware S2->NNI pipeline

### Major
- New `compute_NNI_from_S2()` end-to-end function combines the GPR
  biophysical retrievals with a crop-specific critical-N dilution curve
  and returns a pixel-level NNI map, the critical-N layer, and a
  deficient / optimal / excessive zone raster in a single call. FVC and
  SCL rasters can be supplied for automatic masking.
- New `crop_params_NNI()` exposes first-guess parameters (dilution
  curve a/b, leaf allocation alpha_leaf, N:Chl ratio k_NChl, w_min and
  fvc_min) for eight crops, with full documentation of the calibration
  caveats (alpha_leaf and k_NChl need 30-50 plot-level ground-truth
  samples to avoid 15-30% biases).
- `biophysical_to_NNI_inputs()` is now explicit about the leaf-to-total
  biomass partitioning (previous formulation conflated leaf biomass
  with aboveground biomass). This is a breaking change for the handful
  of scripts that called the helper directly; the high-level
  `compute_NNI_from_S2()` is recommended.

## Changes in version 0.8.0 - GPR biophysical retrieval from Sentinel-2

### Major
- New `estimate_biophysical()` applies pre-trained Gaussian Process
  Regression (GPR) models to a 10-band Sentinel-2 L2A surface-reflectance
  raster and returns canopy-level biophysical maps (LAI, Cm, Cw, FVC,
  Cab, laiCab, CNC_Cab, CNC_Cprot). It is a pure-R port of the
  `pyeogpr` pipeline (Estevez et al. 2022) validated at machine
  precision against the reference Python implementation (correlation
  1.0, relative error < 1.4e-8). Depends only on `terra` and
  `jsonlite` (both listed under Suggests and loaded lazily).
- New `biophysical_to_NNI_inputs()` helper closes the remote-sensing
  pipeline to NNI: it converts `LAI`, `Cm` and `CNC_Cprot` into the
  aboveground biomass W and plant N concentration N_actual required by
  `compute_NNI()`, so Sentinel-2 scenes can be turned directly into
  NNI maps and fed back into the balance-constrained variable-rate
  prescription.

### Dependencies
- Added `terra` and `jsonlite` to Suggests. All other existing
  dependencies unchanged.

## Changes in version 0.7.0 - Remote sensing diagnosis

### Major
- New multi-index vegetation-index engine `compute_vi()` supporting
  NDVI, NDRE, GNDVI, CIred-edge, MCARI and MSAVI2 from a multi-band
  `RasterStack` (Sentinel-2 L2A or UAV multispectral), with
  configurable band mapping and optional integer-DN rescaling.
- The variable-rate pipeline (`variable_rate_N()`,
  `estimate_N_rate_from_calibration_curve()`,
  `estimate_N_rate_from_holland_schepers()`) is now explicitly
  index-agnostic: users can feed any normalised 0-1 VI instead of
  NDVI.
- New `compute_NNI()`, `diagnose_N_status()` and `critical_N_curve()`
  implementing the Nitrogen Nutrition Index of Lemaire & Gastal
  (1997). Species-specific critical-N curves bundled for wheat,
  maize, rice, rapeseed, grass, sorghum and sunflower (Justes 1994;
  Plenet & Lemaire 2000; Sheehy 1998; Colnenne 1998; Duru 1997; van
  Oosterom 2010; Debaeke 2012). Italian and English synonyms both
  accepted.
- New vignette `remote-sensing-diagnosis` with the full VI -> NNI ->
  diagnosis-guided VRT loop.

## Changes relative to the 0.3.0 CRAN baseline

### 0.4.0 (BREAKING) - English-only naming
- Italian column names and dataset names renamed to English
  equivalents. Italian originals are preserved in `_it` companion
  columns where helpful. See NEWS.md for the full rename table.
- Bilingual crop names via `resolve_crop()` and `list_crops()`.
- Italian function names kept as backward-compatible aliases
  (`scheda_N()` / `scheda_PK()`).

### 0.5.0 - Precision fertilization
- `variable_rate_N()` wrapper with mass-balance constraint
  (calibration and Holland & Schepers methods).

### 0.5.1 - Shiny dashboard removed
- Scripting-only API; dependencies reduced to a single hard Imports
  (`raster`) plus `testthat`, `knitr`, `rmarkdown` in Suggests.

### 0.6.0 - Spatial N balance
- `spatial_N_balance()` pixel-wise over a soil `RasterStack`.
- Cremonesi real-field rasters shipped in `inst/extdata/`.
- `spatial-balance` vignette linking spatial balance and VRT.

### 0.7.0 - Remote sensing diagnosis (this release)
- Multi-index VI engine (`compute_vi()`).
- Nitrogen Nutrition Index (`compute_NNI()`, `diagnose_N_status()`,
  `critical_N_curve()`).
- Index-agnostic VRT.
- `remote-sensing-diagnosis` vignette.

## Package Dependencies
- Imports: raster
- Suggests: testthat (>= 3.0.0), knitr, rmarkdown

## Testing
- Benchmark tests reproducing Fert_Office v1.26 (Febbraio 2026) for
  grano duro (pianta intera, 6 t/ha, FL medio impasto):
  - `tests/testthat/test-dpi2026-benchmark.R` (N = 142.246 kg/ha,
    scheda = 200)
  - `tests/testthat/test-PK-balance.R` (P = 112.9 kg P2O5/ha,
    K = 139.4 kg K2O/ha)
- Additional unit tests in `tests/testthat/test-*.R`.

## Documentation
- All exported functions have complete roxygen documentation with
  examples. `man/*.Rd` files must be regenerated via
  `devtools::document()` before building the source tarball.
- Vignettes:
  - `vignettes/NFert.Rmd` - N baseline and precision ag overview.
  - `vignettes/NFert-PK-and-distribution.Rmd` - PK balance and
    distribution plan (header updated to 0.7.0).
  - `vignettes/spatial-balance.Rmd` - spatial balance + VRT.
  - `vignettes/remote-sensing-diagnosis.Rmd` (new in 0.7.0) -
    multi-VI engine and NNI diagnosis.
- README.md updated to describe the remote-sensing scope.
- CITATION updated to 2026, v0.7.0.
- NEWS.md with full history from 0.1.0 to 0.7.0.

## Potential Issues Addressed
- All functions have proper error handling and input validation.
- Namespace properly generated via roxygen2.
- No package dependencies on unavailable software.
- Numeric matches vs Fert_Office v1.26 verified within 1-2 kg/ha
  tolerance.

## Reverse Dependencies
None.
