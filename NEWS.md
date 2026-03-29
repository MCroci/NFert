# NFert News

## Version 0.1.0 (2024-XX-XX) - First CRAN release

### Major Features
* Initial release of NFert package
* Nitrogen balance aligned with **Disciplinari di Produzione Integrata (DPI) Emilia-Romagna 2025–2026** (Allegato 2) and *Guida alla Fertilizzazione Minerale e Organica* (N, P, K); FertDPI / Fert_Office reference
* Comprehensive nitrogen balance calculation (crop demand, soil supply, leaching, immobilization, residues, organic, natural)
* **Maximum allowed doses (MAS)**: `get_MAS()` and `check_MAS()` for DPI 2026 crop ceilings (N and P₂O₅)
* Soil texture classification (USDA; DPI 3-group: sabbioso / franco / argilloso) via tri2, tri3, calc_soil_group_and_id_rag
* NDVI-based variable rate estimation (calibration curve and Holland & Schepers)
* Component functions for all balance terms

### New Functions
* `N_balance()`: Main function for comprehensive nitrogen balance calculation
* `calc_crop_N_demand()`: Calculate crop nitrogen requirements
* `soil_fertility()`: Estimate soil nitrogen availability
* `leaching_loss()`: Calculate nitrogen leaching losses
* `calc_N_immobilization_loss()`: Estimate nitrogen immobilization
* `organic_fertilization()`: Calculate nitrogen from organic fertilizers
* `natural_contribution()`: Estimate natural nitrogen deposition
* `nitrogen_from_previous_crop_residues()`: Estimate nitrogen from previous crops
* `tri2()` and `tri3()`: Soil texture classification
* `calc_soil_group_and_id_rag()`: Determine soil group and drainage index
* `estimate_N_rate_from_calibration_curve()`: NDVI-based rate estimation (calibration method)
* `estimate_N_rate_from_holland_schepers()`: NDVI-based rate estimation (H&S method)
* `calculate_N_fertilization()`: Calculate final nitrogen fertilization requirement from balance
* `get_MAS()`: Return maximum allowed N (MAS) for a crop; optional `edition = "2025"` (ZVN table from Guida) or `"2026"` (FertDPI style)
* `check_MAS()`: Check planned N dose against MAS (supports same `edition`)

### Improvements
* Comprehensive input validation in `N_balance()` function
* Optimized code removing duplicate function calls
* Improved error messages (translated to English)
* Added namespace prefixes for all external package functions
* Comprehensive test suite with testthat
* Improved documentation with data.R for all datasets
* Added README.md with usage examples
* Added LICENSE file (MIT)
* Added CITATION file for package citation

### Bug Fixes
* Fixed bug in `N_balance()` where `calc_crop_N_demand()` result (list) was incorrectly handled
* Fixed incorrect data.frame structure in `N_balance()` output
* Removed unused variables (silt, TRI2, TRI3) from `N_balance()`
* Fixed missing namespace prefixes in `estimate_N_rate_from_holland_schepers()`

### Data
* 13 internal datasets for lookup tables and coefficients
* All datasets documented in R/data.R

### Documentation
* Comprehensive function documentation with examples
* Vignette included with detailed usage examples
* README.md with quick start guide
