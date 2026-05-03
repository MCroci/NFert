# Phosphorus balance (DPI Emilia-Romagna 2026 metodo del bilancio)

Implements the P2O5 balance of Fert_Office v1.26 foglio `Bilancio`.

## Usage

``` r
P_balance(
  expected_yield_tons_ha,
  crop,
  olsen_value,
  olsen_unit = c("P", "P2O5"),
  soil_group = NULL,
  clay = NULL,
  sand = NULL,
  advance_P2O5 = 0,
  depth_cm = 30
)
```

## Arguments

- expected_yield_tons_ha:

  Expected yield (t/ha).

- crop:

  Crop name (matches `uptake_table$crop`).

- olsen_value:

  Analytical Olsen P (or P2O5) in ppm.

- olsen_unit:

  `"P"` or `"P2O5"`, default `"P"`.

- soil_group:

  DPI texture group; if `NULL` and `clay`/`sand` given, derived via
  [`calc_soil_group_and_id_rag()`](https://mcroci.github.io/NFert/reference/calc_soil_group_and_id_rag.md)
  and mapped to Italian Ragg.

- clay, sand:

  Soil clay/sand percentages (%).

- advance_P2O5:

  Advance anni futuri (default 0 kg/ha).

- depth_cm:

  Soil depth for enrichment calculation (default 30 cm).

## Value

A data frame with A, B1, A2, B2, `strategy`, `ID_Gri_P`,
`P2O5_required`.

## Details

Formula:


      P2O5 da apportare = A (asportazione) + B1 (arricchimento) + A2 (advance_allowed)
                           - B2 (riduzione)

with A set to 0 in "Riduzione" strategy and B2 used only as placeholder.

## Examples

``` r
P_balance(expected_yield_tons_ha = 6,
          crop = "Grano duro (pianta intera)",
          olsen_value = 15, olsen_unit = "P2O5",
          clay = 18.5, sand = 15.5)
#>      A A_fabbisogno     B1 A2 B2      strategy ID_Gri_P soil_weight_t_ha
#> 1 63.6         63.6 49.296  0  0 Arricchimento        2             3900
#>   P2O5_required
#> 1       112.896
```
