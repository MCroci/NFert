# Leaching Loss of Nitrogen

Calculates the nitrogen leaching loss from soil based on two methods:

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

  Oxygen availability level in the soil (e.g., "Normal").

- id_rag:

  Soil drainage index (ID_Rag).

- b1:

  Readily available nitrogen in the soil (kg/ha).

## Value

A list containing: - C1: Nitrogen leaching loss in the autumn-winter
season (kg/ha). - C2: Nitrogen leaching loss after leaving winter
(kg/ha). - surplus_pluviometrico: Logical. TRUE when winter_rain +
start_spring_rain \>= 300 mm (DPI 2026 scheda a dose standard: attiva
l'incremento "Lisciviazione x surplus pluviometrico").

## Details

1.  Precipitation-Based Method (C1):

    - Considers winter rainfall (October 1 to January 31).

    - No loss if rainfall \< 150 mm.

    - Progressive loss of readily available nitrogen (b1) if rainfall is
      150-250 mm.

    - Complete loss of readily available nitrogen if rainfall \> 250 mm.

2.  Ease of Drainage Method (C2):

    - Estimates leaching based on soil drainage capacity (ID_Rag) and
      oxygen availability.

    - Uses lookup tables (`ca.table` and `cb.table`) for specific
      values.

## Examples

``` r
leaching_loss(winter_rain = 160, start_spring_rain = 40,
              oxygen_availability = "Normal", id_rag = 3, b1 = 29.16)
#> $C1
#> [1] 20
#> 
#> $C2
#> [1] 2.916
#> 
#> $surplus_pluviometrico
#> [1] FALSE
#> 
```
