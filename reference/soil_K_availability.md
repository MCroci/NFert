# Compute soil K availability terms for the K balance

Compute soil K availability terms for the K balance

## Usage

``` r
soil_K_availability(
  k_value,
  unit = c("K", "K2O"),
  soil_group,
  A_demand_K2O,
  clay_pct,
  include_leaching = TRUE,
  k_availability.table = nfert_data_get("k_availability.table")
)
```

## Arguments

- k_value, unit, soil_group:

  K analytical value and DPI texture group.

- A_demand_K2O:

  Crop K2O demand (kg/ha) from
  [`calc_crop_K_demand()`](https://mcroci.github.io/NFert/reference/calc_crop_K_demand.md).

- clay_pct:

  Clay percentage, used to compute leaching `H`.

- include_leaching:

  Logical, add K leaching to mantenimento term. Default TRUE.

- k_availability.table:

  Lookup table.

## Value

A list with `strategy`, `ID_Gri_K`, `B1`, `A_mantenimento`
(asportazione + H), `B2`, `H` (leaching).

## Examples

``` r
soil_K_availability(k_value = 150, unit = "K2O",
                    soil_group = "Medio impasto",
                    A_demand_K2O = 119.4, clay_pct = 18.5)
#> $strategy
#> [1] "Mantenimento"
#> 
#> $ID_Gri_K
#> [1] 3
#> 
#> $rating
#> [1] "medium"
#> 
#> $B1
#> [1] 0
#> 
#> $A_mantenimento
#> [1] 139.4
#> 
#> $B2
#> [1] 0
#> 
#> $H
#> [1] 20
#> 
```
