# Soil Texture Class Determination

Determines the soil texture class (ID_Rag) based on clay and sand
percentages.

## Usage

``` r
tri2(clay = 10, sand = 35)
```

## Arguments

- clay:

  Percentage of clay in the soil.

- sand:

  Percentage of sand in the soil.

## Value

The soil texture class ID (ID_Rag) corresponding to the given clay and
sand percentages.

## Note

- This function uses the USDA soil texture triangle classification.

- Ensure the `clay` and `sand` percentages are valid (within 0-100) and
  their sum does not exceed 100.

## Examples

``` r
tri2(clay = 10, sand = 35)
#> [1] 6
tri2(clay = 25, sand = 20)
#> [1] 6
```
