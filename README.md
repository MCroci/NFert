# NFert

**Nitrogen fertilization following Emilia-Romagna Recommendation**

[![R-CMD-check](https://github.com/yourusername/NFert/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/NFert/actions)

## Overview

`NFert` is an R package for calculating nitrogen fertilization requirements in field and tree crops following the **Disciplinari di Produzione Integrata (DPI) Emilia-Romagna** (ed. 2026). It implements the **method of the balance** (Allegato 2) as in the regional guidelines and in the FertDPI / Fert_Office tool, and supports maximum allowed doses (MAS), soil texture grouping, and precision agriculture (NDVI-based variable rate).

## Normative reference

- **DPI Emilia-Romagna** (2025–2026) – Norme Generali; Allegato 2 (metodo del bilancio), Allegati 3, 4, 6, 7, 9; *Guida alla Fertilizzazione Minerale e Organica* (N, P, K)
- **FertDPI / Fert_Office** – strumento regionale; NFert riproduce la logica del bilancio azotato per uso in R
- **Reg. reg. 2/2024** – limiti MAS (Allegato 9); **Reg. reg. 3/2017** – utilizzazione agronomica degli effluenti e del digestato
- **Direttiva Nitrati (91/676/CEE)** – in ZVN limite 170 kg N/ha/anno da effluenti zootecnici; Reg. UE 2021/2115 (soglie minime efficienza)

## Features

- **Nitrogen balance (Allegato 2)**: N = A + C + D − B − E − F − G (crop demand, leaching, immobilization, soil supply, precession, previous organic, natural deposition)
- **Soil analysis**: Texture classification (USDA and DPI 3-group: tendenzialmente sabbioso / franco / argilloso), fertility (b1, b2)
- **MAS (Massimali)**: `get_MAS()` and `check_MAS()` for maximum allowed N and P₂O₅ per crop (DPI 2026)
- **Organic fertilization**: N from organics (efficiency depends on source, frequency, and in DPI also on soil texture and distribution technique)
- **Precision agriculture**: NDVI-based variable rate (calibration curve and Holland & Schepers)
- **Component functions**: All balance terms available as separate functions for teaching and scripting

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
- `tri2()` and `tri3()`: Soil texture classification (USDA; tri3 maps to DPI groupings)
- `get_MAS()` and `check_MAS()`: Maximum allowed doses (MAS) per crop (DPI 2026)
- `calculate_N_fertilization()`: Final N dose from balance result
- `estimate_N_rate_from_calibration_curve()`: NDVI-based rate estimation (calibration method)
- `estimate_N_rate_from_holland_schepers()`: NDVI-based rate estimation (H&S method)

## Nitrogen balance (DPI Allegato 2)

N da apportare = **A** + **C** + **D** − **B** − **E** − **F** − **G**

- **A**: Fabbisogno della coltura (resa × coefficiente asportazione)
- **B**: Fertilità del suolo (N mineralizzabile dalla S.O. + N pronto)
- **C**: Perdite per lisciviazione (C1 autunno-inverno, C2 fine inverno)
- **D**: Immobilizzazione e dispersione
- **E**: N da precessione colturale (residui, leguminose, paglie)
- **F**: N da fertilizzazione organiche anni precedenti
- **G**: Apporti naturali (precipitazioni)

NFert computes the balance and then subtracts current-year organic N (Forg) to give the mineral N to apply. In Nitrate Vulnerable Zones (ZVN), the 170 kg N/ha/year limit from livestock effluents applies at farm level.

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
