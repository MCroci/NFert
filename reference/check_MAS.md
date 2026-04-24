# Check planned N against MAS

Checks whether a planned nitrogen dose (kg/ha) exceeds the Maximum
Allowed Dose (MAS) for the crop. In ZVN, also consider the 170 kg
N/ha/year limit from livestock effluents.

## Usage

``` r
check_MAS(crop, N_planned, mas_table = NULL, edition = c("2026", "2025"))
```

## Arguments

- crop:

  Character. Crop name (DPI nomenclature).

- N_planned:

  Numeric. Planned nitrogen dose (kg/ha).

- mas_table:

  Optional. Same as in `get_MAS`. Default uses built-in table.

- edition:

  Character. `"2026"` or `"2025"`; used only if `mas_table` is NULL.

## Value

List with `ok` (TRUE if N_planned \<= MAS or crop has no MAS_N),
`mas_N`, `N_planned`, `message`.
