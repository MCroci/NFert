# Calculate soil fertility parameters (B = B1 + B2 in DPI).

Estimates readily available and mineralized nitrogen in the soil. DPI
naming: b1 = readily available N from soil total N (DPI B2); b2 =
mineralized N from SOM (DPI B1). Total B = b1 + b2.

## Usage

``` r
soil_fertility(
  Ntot = 1.2,
  SOM = 2,
  soil.group = "Sandy textures",
  CN = 8,
  ccp = "Autumn-winter crop <150 days",
  soil_seeding = c("traditional", "no-till"),
  coefN_readily = nfert_data_get("coefN_readily"),
  coefN_mineralised = nfert_data_get("coefN_mineralised"),
  coef_time = nfert_data_get("coef_time")
)
```

## Arguments

- Ntot:

  Total nitrogen content in the soil (e.g. g/kg or per mille).

- SOM:

  Soil organic matter content (%).

- soil.group:

  Soil group (e.g., "Sandy textures").

- CN:

  Carbon-to-nitrogen ratio.

- ccp:

  Crop category and planting period (e.g., "Autumn-winter crop \<150
  days").

- soil_seeding:

  Seeding mode, one of `"traditional"` (default) or `"no-till"`. DPI
  2026 applies a detrazione of 3 kg N/ha on b1 (readily available N, "N
  pronto") for semina su sodo, because tillage-driven mineralisation is
  absent. See foglio B, cella `Detrazione per semina su sodo = 3` in
  Fert_Office v1.26.

- coefN_readily, coefN_mineralised, coef_time:

  Lookup tables.

## Value

A named list containing:

- b1: Readily available nitrogen (kg/ha), DPI B2.

- b2: Mineralized nitrogen from SOM (kg/ha), DPI B1.

- units: The units of the output values ("kg/ha").
