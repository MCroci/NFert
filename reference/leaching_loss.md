# Leaching Loss of Nitrogen

DPI 2026 (Allegato 2; foglio `C&D` di Fert_Office v1.26): loss of
readily available mineral nitrogen (\\b_1\\) from leaching.

## Usage

``` r
leaching_loss(
  winter_rain = 160,
  start_spring_rain = 40,
  oxygen_availability = "Normal",
  id_rag = 3,
  b1 = 29.16
)
```

## Arguments

- winter_rain:

  Winter rainfall (October 1 to January 31) in mm.

- start_spring_rain:

  Rainfall in February in mm.

- oxygen_availability:

  Oxygen availability level in the soil: one of `ca.table$availability`
  (e.g. `"Normal"`, `"Slow"`, `"Fast"`).

- id_rag:

  Soil drainage index (`ID_Rag`); retained for backward compatibility
  with earlier NFert signatures (not used in the \\C_a\\/\\C_b\\
  formulas above).

- b1:

  Readily available nitrogen in the soil (kg N/ha).

## Value

A list containing:

- C1:

  Autumn–winter leaching loss (kg N/ha).

- C2:

  February leaching loss (kg N/ha).

- surplus_pluviometrico:

  Logical. `TRUE` when `winter_rain + start_spring_rain >= 300` mm (DPI
  2026 scheda a dose standard: surplus-pluviometric flag).

## Details

- **C1** (\\C_a\\, autumn–winter):

  Depends on winter rainfall (1 October – 31 January), `winter_rain`
  (mm):

  - \\\leq 150\\ mm \\\rightarrow\\ no loss (0);

  - \\(150,\\250\]\\ mm \\\rightarrow\\ linear fraction of \\b_1\\:
    \\b_1 \times (R\_{\mathrm{win}} - 150) / 100\\;

  - \\\> 250\\ mm \\\rightarrow\\ loss of the entire \\b_1\\ pool
    (cannot exceed \\b_1\\).

- **C2** (\\C_b\\, February):

  Additional loss from February rainfall (`start_spring_rain`, mm)
  applied only to the *residual* readily available N after \\C_1\\, and
  only if `winter_rain > 150`. Formula:
  \\\min(\code{start_spring_rain}/10,\\ b_1 - C_1)\\.

Invariant: \\C_1 + C_2 \leq b_1\\.

`oxygen_availability` is validated against
[`ca.table`](https://mcroci.github.io/NFert/reference/NFert-data.md) for
consistency with other balance terms; the rainfall formulas above do not
use the legacy tabular column `cb.table$C` (that column referred to the
old, incorrect assignment of a fixed loss independent of rainfall).

## Examples

``` r
leaching_loss(winter_rain = 160, start_spring_rain = 40,
              oxygen_availability = "Normal", id_rag = 3, b1 = 29.16)
#> $C1
#> [1] 2.916
#> 
#> $C2
#> [1] 4
#> 
#> $surplus_pluviometrico
#> [1] FALSE
#> 
```
