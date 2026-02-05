# NFert News

## Version 0.1.0 (2024-XX-XX) - First CRAN release

### Major Features
* Initial release of NFert package
* Comprehensive nitrogen balance calculation following Emilia-Romagna Regional Recommendations
* NDVI-based nitrogen rate estimation using calibration curves and Holland & Schepers methods
* Soil texture classification functions (tri2 and tri3)
* Component functions for all nitrogen balance calculations

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
