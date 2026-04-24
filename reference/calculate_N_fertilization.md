# Calculate Nitrogen Fertilization Requirement

Calculates the final nitrogen fertilization requirement (N_fert) from a
nitrogen balance result.

## Usage

``` r
calculate_N_fertilization(n_balance, method = "standard")
```

## Arguments

- n_balance:

  A data frame returned by
  [`N_balance()`](https://mcroci.github.io/NFert/reference/N_balance.md)
  containing nitrogen balance components.

- method:

  The calculation method to use. Default is "standard" which uses the
  DPI formula: N_fert = A - B + C1 + C2 + D - E - F - Forg - G

## Value

A numeric value representing the nitrogen fertilization requirement in
kg/ha.

## Details

This function calculates the final nitrogen fertilization requirement
based on the nitrogen balance components. If the result is negative, it
returns 0 (no fertilization needed).

The standard (DPI) formula is:

    N_fert = A - B + C1 + C2 + D - E - F - Forg - G

Where:

- A: Crop nitrogen demand

- B: Soil nitrogen supply

- C1, C2: Leaching losses

- D: Immobilization losses

- E: Nitrogen from previous crop residues

- F: Nitrogen from previous years' organic fertilization (0 if not in
  balance)

- Forg: Nitrogen from organic fertilizer (current year)

- G: Natural nitrogen contribution

## Examples

``` r
# Calculate nitrogen balance
balance <- N_balance(
  expected_yield_tons_ha = 15,
  crop = "Mais trinciato (classe 700)",
  ccp = "Spring-summer crop 100-130 days",
  sand = 50, clay = 35,
  Ntot = 1.2, SOM = 1.2, CN = 9.5,
  oxygen_availability = "Normal",
  winter_rain = 160, start_spring_rain = 40,
  prev_crop = "Winter cereals straw removal",
  source = "Cattle slurry",
  fertorg_frequency = "every year",
  location = "Plain adjacent to urbanized areas",
  forg_quantity = 100
)

# Calculate required nitrogen fertilization
n_fert <- calculate_N_fertilization(balance)
print(paste("Required N fertilization:", round(n_fert, 2), "kg/ha"))
#> [1] "Required N fertilization: 50.55 kg/ha"
```
