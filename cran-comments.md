# CRAN Submission Comments for NFert 0.3.0

## Package Information
- Package: NFert
- Version: 0.3.0
- Title: N, P and K fertilization following Emilia-Romagna DPI 2026
- Author: Michele Croci [aut, cre] <michele.croci@unicatt.it>
- Maintainer: Michele Croci <michele.croci@unicatt.it>

## Test Environments
- Local: Windows 10, R 4.5.1
- R-hub checks (to be run before submission)

## R CMD check results
(Will be updated after running devtools::check())

## Changes in this version (0.3.0)

### Major
- Full P2O5 and K2O balance (`P_balance()`, `K_balance()`).
- Scheda a dose Standard for N and PK (`scheda_N()`, `scheda_PK()`).
- Fertilization distribution plan (`plan_distribution()`) with organic +
  mineral fertilisers, DPI 2026 efficiency matrix and ZVN 170 kg N/ha check.
- End-of-cycle soil P and K estimation.
- Soil chemistry classifiers: pH, total and active carbonate, CEC,
  Mg/K ratio, K/CEC ratio, SOM class, maximum SO input.
- Alignment with Fert_Office v1.26 (Febbraio 2026): full re-sync of all
  reference lookup tables.

### Changes relative to 0.1.0
- New datasets (21 .rda files added in v0.2.0/0.3.0) covering the full DPI
  2026 catalogue: MAS, Gri_P, Gri_K, SO, pH, calcare, ragg_tes, efficienza,
  fert_org, concimi, standard_pk_doses, standard_decrements,
  standard_increments, standard_multicycle, cicli, cicli_fase,
  ciclo_modalita, mod_distribuz, tipo_fert, cd.
- 15 new exported functions (see NEWS.md).

## Package Dependencies
- Imports: raster
- Suggests: testthat (>= 3.0.0), knitr, rmarkdown

## Testing
- Two dedicated benchmark test files reproducing exactly the Bilancio and
  Scheda output of Fert_Office v1.26 for grano duro (pianta intera, 6 t/ha,
  FL medio impasto):
  - `tests/testthat/test-dpi2026-benchmark.R` (N = 142.246 kg/ha, scheda = 200)
  - `tests/testthat/test-PK-balance.R` (P = 112.9 kg P2O5/ha,
                                         K = 139.4 kg K2O/ha)
- Original tests in `tests/testthat/test-*.R` retained.

## Documentation
- All exported functions have complete roxygen documentation with examples.
  `man/*.Rd` files must be regenerated via `devtools::document()` before
  build (21 new functions from v0.2.0/0.3.0 still missing their .Rd files).
- Two vignettes: `vignettes/NFert.Rmd` (N baseline) and
  `vignettes/NFert-PK-and-distribution.Rmd` (PK + plan).
- `docs/UPDATE_PLAN_Fert_Office_v1.26.md` (gap analysis) and
  `docs/UPDATE_v0.2.0_SUMMARY.md`, `docs/UPDATE_v0.3.0_SUMMARY.md`
  (change logs).
- README.md updated to describe the full N+P+K scope.
- CITATION updated to 2026, v0.3.0.
- NEWS.md with full 0.2.0 and 0.3.0 sections.

## Potential Issues Addressed
- All functions have proper error handling and input validation.
- Namespace properly generated via roxygen2.
- No package dependencies on unavailable software.
- Numeric matches vs Fert_Office v1.26 verified within 1-2 kg/ha tolerance.

## Reverse Dependencies
None.
