# Nitrogen Balance Calculation

Calculates the nitrogen balance of a crop considering various soil and
environmental factors.

## Usage

``` r
N_balance(
  expected_yield_tons_ha = 15,
  crop = "Grano tenero FF (granella)",
  ccp = "Spring-summer crop 100-130 days",
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
  forg_quantity = 100,
  organic_previous_year_N = 0,
  soil_group = NULL,
  distribution_efficiency = NULL,
  soil_seeding = c("traditional", "no-till"),
  greenhouse = FALSE,
  E_to_D = TRUE
)
```

## Arguments

- expected_yield_tons_ha:

  Expected crop yield in tons per hectare.

- crop:

  Name of the crop.

- ccp:

  Crop calendar period (e.g., "Spring-summer crop 100-130 days").

- sand:

  Percentage of sand in the soil.

- clay:

  Percentage of clay in the soil.

- Ntot:

  Total nitrogen content in the soil (%).

- SOM:

  Soil organic matter content (%).

- CN:

  Carbon to nitrogen ratio of the soil.

- oxygen_availability:

  Oxygen availability level in the soil (e.g., "Normal").

- winter_rain:

  Total winter rainfall (mm).

- start_spring_rain:

  Rainfall at the start of spring (mm).

- prev_crop:

  Previous crop grown in the field.

- source:

  Source of organic fertilizer (e.g., "Cattle slurry").

- fertorg_frequency:

  Frequency of organic fertilizer application (e.g., "every year").

- location:

  Location of the field relative to urban areas.

- forg_quantity:

  Quantity of organic fertilizer applied (m3/ha or t/ha, depending on
  source). Use `0` for no organic application: `Forg` is set to 0
  without calling
  [`organic_fertilization()`](https://mcroci.github.io/NFert/reference/organic_fertilization.md).

- organic_previous_year_N:

  Optional. Total N (kg/ha) from organic fertilization applied in the
  previous year (same source/frequency). If provided, term F (residual N
  from previous years' organic, DPI section 3.1.6) is included. Default
  0.

- soil_group:

  Optional override for DPI organic-N efficiency (texture class). When
  `NULL` (default), `N_balance()` uses the `soil.group` computed from
  `sand` and `clay` via
  [`calc_soil_group_and_id_rag()`](https://mcroci.github.io/NFert/reference/calc_soil_group_and_id_rag.md),
  so current-year organic (`Forg`) follows the same texture as `B`.

- distribution_efficiency:

  Optional. `"efficient"`, `"medium"`, or `"low"` (DPI distribution
  quality). When `NULL` (default), `N_balance()` uses `"medium"` for
  `Forg` (typical farm practice when not specified). Ignored for `Forg`
  when `forg_quantity == 0`.

- soil_seeding:

  One of `"traditional"` (default) or `"no-till"`. If `"no-till"`, DPI
  2026 applies a 3 kg/ha reduction to b1 (readily available N).

- greenhouse:

  Logical. If `TRUE`, 2 kg/ha are added to D (DPI 2026 greenhouse
  factor). Default `FALSE`.

- E_to_D:

  Logical. If `TRUE` (default), negative `E` (e.g. buried stalks = -40)
  is added to `D` as in Fert_Office v1.26 sheet C&D, and `E` in the
  output is set to 0. If `FALSE`, `E` stays negative and the balance
  uses
  [`calculate_N_fertilization()`](https://mcroci.github.io/NFert/reference/calculate_N_fertilization.md).

## Value

A data frame containing the calculated nitrogen balance components (all
in kg/ha):

- A: Total nitrogen demand of the crop (kg/ha).

- B: Total soil nitrogen supply (sum of b1 and b2) (kg/ha).

- b1: Readily available mineral nitrogen (DPI B2) (kg/ha).

- b2: Mineralizable N from soil organic matter (DPI B1) (kg/ha).

- C1: Nitrogen leaching loss in winter (kg/ha).

- C2: Nitrogen leaching loss in spring (kg/ha).

- D: Nitrogen immobilization loss (kg/ha). Includes the abs(E)
  contribution if `E_to_D = TRUE` and E \< 0, and `+2` if
  `greenhouse = TRUE`.

- E: Nitrogen from previous crop residues (kg/ha). If `E_to_D = TRUE`
  and the raw value is negative, this column reports 0 (moved to D).

- F: Nitrogen from previous years' organic fertilization (kg/ha).

- Forg: Nitrogen from organic fertilizer (current year, efficient N)
  (kg/ha).

- G: Natural nitrogen contribution (e.g., from rainfall) (kg/ha).

- surplus_pluviometrico: Logical flag (DPI 2026 standard sheet, surplus
  rain).

## Details

The nitrogen fertilization requirement (DPI formula) is: N_fert = A -
B + C1 + C2 + D - E - F - Forg - G

## Examples

``` r
N_balance(expected_yield_tons_ha = 15, crop = "Mais trinciato (classe 700)",
          ccp = "Spring-summer crop 100-130 days", sand = 50, clay = 35,
          Ntot = 1.2, SOM = 1.2, CN = 9.5, oxygen_availability = "Normal",
          winter_rain = 160, start_spring_rain = 40,
          prev_crop = "Winter cereals straw removal", source = "Cattle slurry",
          fertorg_frequency = "every year", location = "Plain adjacent to urbanized areas",
          forg_quantity = 100)
#>      A      B    b1    b2    C1 C2       D E F   Forg    G
#> 1 58.5 38.808 29.16 9.648 2.916  4 21.6424 0 0 180.45 13.4
#>   surplus_pluviometrico
#> 1                 FALSE
```
