# NFert News

## Version 0.4.0 (2026-04-15) - English-only naming (BREAKING)

### Crop names: bilingual (Italian canonical + English alias)

* New `crop_en` column added to `uptake_table`, `mas.table`, `crops.table`
  and `standard_pk_doses.table`. The Italian DPI label remains the
  canonical join key in `crop`, while `crop_en` provides an English
  translation for the most common DPI 2026 crops (54 of 235 crops
  curated; the rest fall back to the Italian name).
* New `resolve_crop()` helper accepts either the Italian or the English
  crop name (case-insensitive) and returns the canonical Italian form.
* All public functions that look up by crop (`calc_crop_N_demand()`,
  `calc_crop_P_demand()`, `calc_crop_K_demand()`, `dose_standard_N()`,
  `dose_standard_PK()`, `get_MAS()`, `check_MAS()`) now route through
  `resolve_crop()`, so the user can pass English names indifferently.
* New `list_crops()` helper returns the full Italian/English mapping.

### BREAKING CHANGES

* All Italian column names replaced with English equivalents. Italian
  originals are preserved in `_it` companion columns where helpful.
* All Italian dataset names renamed:
  - `gruppo.table` -> `crop_groups.table`
  - `ragg_tes.table` -> `texture_groups.table`
  - `gri_p.table` -> `p_availability.table` (+ `gri_p_meta` -> `p_availability_meta`)
  - `gri_k.table` -> `k_availability.table`
  - `tipo_fert.table` -> `fertilizer_types.table`
  - `fert_org.table` -> `organic_fertilizers.table`
  - `efficienza.table` -> `efficiency.table`
  - `mod_distribuz.table` -> `distribution_modalities.table`
  - `ciclo_modalita.table` -> `cycle_modality.table`
  - `cicli.table` -> `cycles.table`
  - `cicli_fase.table` -> `cycle_phases.table`
  - `cd.table` -> `c_d.table`
  - `concimi.table` -> `mineral_fertilizers.table`
  - `calcare_tot.table` -> `total_carbonate.table`
  - `calcare_att.table` -> `active_carbonate.table`
* Italian function names kept as backward-compatible aliases:
  `scheda_N()` -> `dose_standard_N()`, `scheda_PK()` -> `dose_standard_PK()`.
* `scheda_PK()` class arguments renamed: `"Normale"` -> `"normal"`,
  `"Bassa"` -> `"low"`, `"Elevata"` -> `"high"`,
  `"Molto_Bassa"` -> `"very_low"`.
* `max_SO_input()` class argument renamed: `"Scarsa"` -> `"Poor"`,
  `"Normale"` -> `"Normal"`, `"Elevata"` -> `"Rich"`.
* `normalise_soil_group()$en` is now the canonical form (was Italian plural).
* Soil group values in `texture_groups.table`, `k_availability.table`,
  `so.table`, etc. are now English ("Sandy textures", "Loamy textures",
  "Clay textures").
* Several column renames inside tables (English-only naming):
  - `uptake_table`: `bilancio` -> `balance`, `yield_ref` -> `reference_yield`,
    `fabb_*_std` -> `std_*_demand`, `parte_asportata` -> `harvested_part`.
  - `mas.table`: `resa_standard` -> `standard_yield`, `N_standard` ->
    `standard_N`, `incremento_max` -> `max_increment`,
    `dose_max_N` -> `max_N_dose`, `N_max_teorico` -> `max_theoretical_N`,
    `ss_pct` -> `dry_matter_pct`, `asciutto_irriguo` -> `dry_or_irrigated`.
  - `crops.table`: `cic_id` -> `cycle_id`, `detrazione_sodo` ->
    `no_till_reduction`, `group_prog` -> `group_progressive`.
  - `texture_groups.table`: `peso_specifico` -> `specific_weight`,
    `f_imm_P` -> `P_immobilisation_factor`,
    `peso_*cm` -> `soil_weight_*cm`.
  - `organic_fertilizers.table`: `tipo` -> `type_id`,
    `avg_ss/min_ss/max_ss` -> `avg_dm/min_dm/max_dm`,
    `zootec_100pct` -> `fully_zootec`, `incremento_SO` -> `SO_increment`.
  - `mineral_fertilizers.table`: `concime` -> `fertilizer`,
    `ID_Conc_min` -> `ID_min`.
  - `coef_time`: `precessione_da_conteggiare` -> `precession_to_count`,
    `anticipazioni` -> `advance_allowed`, `allevamento` -> `husbandry`.
  - `standard_pk_doses.table`: `Resa_Standard` -> `standard_yield`,
    `N_Standard` -> `standard_N`, `P2O5_dot_*` -> `P2O5_*` (with English
    classes), `K2O_dot_*` -> `K2O_*`.
  - `standard_decrements.table` / `standard_increments.table`: full
    translation (e.g. `Rid_N_Resa` -> `red_N_yield`,
    `Inc_N_Surplus_pluvio` -> `inc_N_rain_surplus`, `N_Inc_Max` ->
    `inc_N_max`, etc.).
  - `standard_multicycle.table`: `N_I_anno_allev` ->
    `N_1st_husbandry_year`, `Inc_N_inizio_produz` ->
    `inc_N_start_production`, `piu_tagli` -> `multi_cuts`,
    `incremento_per_taglio` -> `increment_per_cut`.

### Migration notes

* User code passing Italian soil-group strings still works through
  `normalise_soil_group()` which accepts all conventions.
* User code calling `scheda_N()` / `scheda_PK()` continues to work.
* User code accessing data columns by Italian names must be updated to
  the English equivalent (full mapping in `data-raw/build-rda.R`).

## Version 0.3.0 (2026-04-15) - Full N + P + K balance and distribution plan

### Major

* **Phosphorus balance**: `P_balance()` implements DPI 2026 P2O5 balance
  (Fabbisogno A, Arricchimento B1, Anticipazioni A2, Riduzione B2) with
  Olsen-P class via `classify_P_olsen()` and soil P availability strategy
  via `soil_P_availability()` (Arricchimento / Mantenimento / Riduzione).
* **Potassium balance**: `K_balance()` with K2O availability by texture group
  via `classify_K()` and `soil_K_availability()`, plus `K_leaching_by_clay()`
  step function for clay-dependent leaching (foglio Gri_K).
* **Unit crop demand**: `calc_crop_P_demand()` and `calc_crop_K_demand()`
  parallel to `calc_crop_N_demand()`.
* **Scheda PK**: `scheda_PK()` returns the DPI 2026 standard-dose P2O5 and
  K2O values from `standard_pk_doses.table` with user-configurable
  decrements and increments.
* **Distribution plan**: `plan_distribution()` builds the full fertilisation
  plan (foglio Distribuz) combining organic matrices (via `fert_org.table`
  titres and `efficienza.table` N efficiency by soil x dose x sector) with
  mineral fertilisers (from `concimi.table`, 146 records), with alerts for
  Eccesso/Deficit vs the N/P/K balances and optional ZVN 170 kg N/ha check.
* **End-of-cycle soil dotation**: `estimate_soil_P_end_of_cycle()` and
  `estimate_soil_K_end_of_cycle()` project soil P2O5 and K2O ppm at the
  end of the cycle, usable as input for the following year.

### Bug fix — soil group naming uniformization

* The DPI texture grouping was inconsistently encoded across the lookup
  tables: `ragg_tes.table` used "Sabbioso/Franco/Argilloso" (singular),
  `gri_k.table`, `so.table` used "Sabbiosi/Medio impasto/Argillosi e limosi"
  (plural), and the legacy NFert 0.1.0 tables (`soil.table`, `coefN_*`)
  used English ("Sandy textures", etc.). This caused silent NA joins in
  `P_balance()` / `K_balance()` / `estimate_soil_*_end_of_cycle()`.
* Added `normalise_soil_group()` and `resolve_id_rag()` helpers that accept
  any of the three naming conventions (case-insensitive) and return the
  canonical Italian plural form plus the universal `ID_Rag` integer key.
* All PK functions, `plan_distribution()` and `max_SO_input()` now route
  every soil-group string through `normalise_soil_group()` and join on
  `ID_Rag`. The user can pass any form indifferently.

### Precision agriculture integration

* New `variable_rate_N()` wrapper that bridges the agronomic balance
  (`N_balance()` + `calculate_N_fertilization()` or `scheda_N()`) with the
  NDVI-based variable-rate estimators
  (`estimate_N_rate_from_calibration_curve()`,
  `estimate_N_rate_from_holland_schepers()`). Preserves the field-average
  dose by rescaling and supports an optional MAS per-pixel cap.

### New soil chemistry classifiers

* `classify_pH()`, `classify_carbonate_tot()`, `classify_carbonate_att()`,
  `classify_CEC()`, `ratio_Mg_K()`, `ratio_K_CEC()`, `classify_SOM()`,
  `max_SO_input()`.

### New datasets

* `standard_pk_doses.table` (239 rows, 14 cols): base PK doses per crop by
  soil dotation class (Molto_Bassa / Bassa / Normale / Elevata).
* `standard_decrements.table` (239 x 21), `standard_increments.table`
  (239 x 29), `standard_multicycle.table` (239 x 24): DPI 2026 adjustment
  factors from foglio `Standard` (exploded into three tables for
  readability).
* `concimi.table`: 146 mineral / organic-mineral fertilisers with titres
  (N, P2O5, K2O).

## Version 0.2.0 (2026-04-15) - Sync with Fert_Office v1.26 (DPI 2026, Feb 2026)

### Alignment with DPI Emilia-Romagna 2026

* **Full re-sync of reference data** from `Fert_Office_v1._26.xlsm`
  (Febbraio 2026, Regione Emilia-Romagna):
  `uptake_table` (236 crops, with N + P2O5 + K2O coefficients),
  `mas.table` (239 crops with N_standard, dose_max_N, RR 2/2024 note),
  `e.table`, `f.table`, `g.table` (English-first with Italian alias),
  `coef_time` (now including N/P/K allowed percentages, anticipazioni and
  allevamento flags per phase).

* **New lookup tables**: `crops.table`, `gruppo.table`, `ragg_tes.table`,
  `so.table`, `so_max_input`, `ph.table`, `calcare_tot.table`,
  `calcare_att.table`, `gri_p.table`, `gri_p_meta`, `gri_k.table`,
  `tipo_fert.table`, `fert_org.table`, `efficienza.table` (220 rows),
  `mod_distribuz.table`, `ciclo_modalita.table`, `cicli.table`,
  `cicli_fase.table`, `cd.table`.

### New functions

* `scheda_N()`: DPI 2026 simplified "Scheda a dose Standard" method for N,
  with catalogued decrement and increment factors and MAS cap.

### Fixes to the N balance (DPI 2026 semantics)

* `soil_fertility()` gains `soil_seeding` argument. When `"no-till"` the
  detrazione of 3 kg/ha (foglio B, Detrazione semina su sodo) is applied to b1.
* `calc_N_immobilization_loss()` gains `greenhouse` (adds 2 kg/ha,
  foglio C&D cella D23) and `E_residual` (fold negative E into D as per
  foglio C&D cella I21).
* `leaching_loss()` now returns `surplus_pluviometrico` (logical), true when
  pioggia 1/10 - 28/2 >= 300 mm (foglio C&D righe 34-35).
* `N_balance()` gains `soil_seeding`, `greenhouse`, `E_to_D` parameters and
  exposes `surplus_pluviometrico` in the output. When `E_to_D = TRUE` (default),
  a negative precessione `E` is summed into `D` and `E` in the output is set
  to 0, matching the exact columnar layout of the Fert_Office `Bilancio` sheet.

### Documentation

* `R/data.R` updated with the full list of new datasets and their role.
* `docs/UPDATE_PLAN_Fert_Office_v1.26.md` published with detailed gap analysis
  and roadmap for PK balance and distribution planning (phases D, E, F).

## Version 0.1.0 (2024-XX-XX) - First CRAN release

### Major Features
* Initial release of NFert package
* Nitrogen balance aligned with **Disciplinari di Produzione Integrata (DPI) Emilia-Romagna 2025–2026** (Allegato 2) and *Guida alla Fertilizzazione Minerale e Organica* (N, P, K); FertDPI / Fert_Office reference
* Comprehensive nitrogen balance calculation (crop demand, soil supply, leaching, immobilization, residues, organic, natural)
* **Maximum allowed doses (MAS)**: `get_MAS()` and `check_MAS()` for DPI 2026 crop ceilings (N and P₂O₅)
* Soil texture classification (USDA; DPI 3-group: sabbioso / franco / argilloso) via tri2, tri3, calc_soil_group_and_id_rag
* NDVI-based variable rate estimation (calibration curve and Holland & Schepers)
* Component functions for all balance terms

### New Functions
* `N_balance()`: Main function for comprehensive nitrogen balance calculation
* `calc_crop_N_demand()`: Calculate crop nitrogen requirements
* `soil_fertility()`: Estimate soil nitrogen availability
* `leaching_loss()`: Calculate nitrogen leaching losses
* `calc_N_immobilization_loss()`: Estimate nitrogen immobilization
* `organic_fertilization()`: Calculate nitrogen from organic fertilizers
* `natural_contribution()`: Estimate natural nitrogen deposition
* `nitrogen_from_previous_crop_residues()`: Estimate nitrogen from previous crops
* `tri2()` and `tri3()`: Soil texture classification
* `calc_soil_group_and_id_rag()`: Determine soil group and drainage index
* `estimate_N_rate_from_calibration_curve()`: NDVI-based rate estimation (calibration method)
* `estimate_N_rate_from_holland_schepers()`: NDVI-based rate estimation (H&S method)
* `calculate_N_fertilization()`: Calculate final nitrogen fertilization requirement from balance
* `get_MAS()`: Return maximum allowed N (MAS) for a crop; optional `edition = "2025"` (ZVN table from Guida) or `"2026"` (FertDPI style)
* `check_MAS()`: Check planned N dose against MAS (supports same `edition`)

### Improvements
* Comprehensive input validation in `N_balance()` function
* Optimized code removing duplicate function calls
* Improved error messages (translated to English)
* Added namespace prefixes for all external package functions
* Comprehensive test suite with testthat
* Improved documentation with data.R for all datasets
* Added README.md with usage examples
* Added LICENSE file (MIT)
* Added CITATION file for package citation

### Bug Fixes
* Fixed bug in `N_balance()` where `calc_crop_N_demand()` result (list) was incorrectly handled
* Fixed incorrect data.frame structure in `N_balance()` output
* Removed unused variables (silt, TRI2, TRI3) from `N_balance()`
* Fixed missing namespace prefixes in `estimate_N_rate_from_holland_schepers()`

### Data
* 13 internal datasets for lookup tables and coefficients
* All datasets documented in R/data.R

### Documentation
* Comprehensive function documentation with examples
* Vignette included with detailed usage examples
* README.md with quick start guide
