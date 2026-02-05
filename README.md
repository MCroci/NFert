# NFert

**Nitrogen fertilization following Emilia-Romagna Recommendation**

[![R-CMD-check](https://github.com/yourusername/NFert/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/NFert/actions)

## Overview

`NFert` is an R package for calculating nitrogen fertilization rates following the Emilia-Romagna Regional Recommendation for agricultural crops. The package implements a comprehensive nitrogen balance model that accounts for various factors affecting nitrogen dynamics in crop production systems.

## Features

- **Nitrogen Balance Calculation**: Comprehensive calculation considering crop requirements, soil fertility, leaching losses, and natural contributions
- **Soil Analysis**: Functions for soil texture classification and fertility assessment
- **NDVI-Based Rate Estimation**: Two methods for estimating nitrogen rates from NDVI data:
  - Calibration curve method (two-point or three-point)
  - Holland & Schepers algorithm
- **Component Functions**: Modular functions for each component of the nitrogen balance

## Installation

```r
# Install from GitHub (once available)
# devtools::install_github("yourusername/NFert")
```

## Quick Start

```r
library(NFert)

# Calculate nitrogen balance for a maize crop
maize_n_balance <- N_balance(
  expected_yield_tons_ha = 15,
  crop = "Mais trinciato (classe 700)",
  ccp = "Spring-summer crop 100–130 days",
  sand = 50,
  clay = 35,
  Ntot = 1.2,
  SOM = 1.2,
  CN = 9.5,
  oxygen_availability = "Normal",
  winter_rain = 160,
  start_spring_rain = 40,
  prev_crop = "Winter cereals straw removal",
  source = "Cattle slurry",
  fertorg_frequency = "every year",
  location = "Plain adjacent to urbanized areas",
  forg_quantity = 100
)

print(maize_n_balance)
```

## Main Functions

- `N_balance()`: Main function for calculating comprehensive nitrogen balance
- `calc_crop_N_demand()`: Calculate crop nitrogen requirements
- `soil_fertility()`: Estimate soil nitrogen availability
- `leaching_loss()`: Calculate nitrogen leaching losses
- `calc_N_immobilization_loss()`: Estimate nitrogen immobilization
- `organic_fertilization()`: Calculate nitrogen from organic fertilizers
- `natural_contribution()`: Estimate natural nitrogen deposition
- `tri2()` and `tri3()`: Soil texture classification
- `estimate_N_rate_from_calibration_curve()`: NDVI-based rate estimation (calibration method)
- `estimate_N_rate_from_holland_schepers()`: NDVI-based rate estimation (H&S method)

## Nitrogen Balance Equation

The nitrogen fertilization is calculated using:

$$N_{fert} = A - B + C + D - E - F - G$$

Where:
- **A**: Crop nitrogen requirements
- **B**: Soil nitrogen supply (b1 + b2)
- **C**: Leaching losses (C1 + C2)
- **D**: Immobilization and dispersion losses
- **E**: Nitrogen from previous crop residues
- **F**: Nitrogen from organic fertilizations
- **G**: Natural nitrogen contributions

## Documentation

See the package vignette for detailed examples and usage:

```r
vignette("NFert")
```

## Authors

- Michele Croci (maintainer)
- Manuele Ragazzi
- Giorgio Impollonia
- Stefano Amaducci

## Citation

To cite this package in publications, use:

```r
citation("NFert")
```

## License

This package is licensed under the MIT License. See `LICENSE` file for details.

## Contact

For questions or issues, please contact:
- Email: michele.croci@unicatt.it
- Department: Department of Sustainable Plant Productions (DI.PRO.VE.S), Catholic University of the Sacred Heart
