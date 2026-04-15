# NFert 0.3.0 — PK balance + distribution plan (Phases D, E, F)

Date: 2026-04-15
Scope: Phases D, E, F of the update plan in `UPDATE_PLAN_Fert_Office_v1.26.md`.
Complements 0.2.0 (Phases A, B, C).

## Phase D — P2O5 and K2O balance

### New data (from foglio Standard + Concimi of Fert_Office v1.26)

| File | Shape | Content |
|---|---:|---|
| `standard_pk_doses.table.rda` | 239 × 14 | PK base doses per crop with 4 soil dotation classes |
| `standard_decrements.table.rda` | 239 × 21 | DPI 2026 catalogue of N/P/K reduction factors |
| `standard_increments.table.rda` | 239 × 29 | DPI 2026 catalogue of N/P/K increment factors (with Inc_max caps) |
| `standard_multicycle.table.rda` | 239 × 24 | Allevamento / multi-taglio / inizio produzione |
| `concimi.table.rda` | 146 × 5 | Mineral & organo-mineral fertilisers (N, P2O5, K2O titres) |

### New functions (13)

| File | Function | Role |
|---|---|---|
| `R/calc_crop_P_K_demand.R` | `calc_crop_P_demand()`, `calc_crop_K_demand()` | Crop PK requirement (× 10 unit fix) |
| `R/soil_P_availability.R` | `classify_P_olsen()`, `soil_P_availability()` | Olsen P class + Arricchimento/Mantenimento/Riduzione strategy |
| `R/soil_K_availability.R` | `classify_K()`, `K_leaching_by_clay()`, `soil_K_availability()` | K class by texture + clay-driven leaching |
| `R/P_balance.R` | `P_balance()` | P2O5 bilancio: A + B1 + A2 − B2 |
| `R/K_balance.R` | `K_balance()` | K2O bilancio: A + H + B1 + A2 − B2 |
| `R/scheda_PK.R` | `scheda_PK()` | Dose standard per P e K |
| `R/soil_chemistry.R` | `classify_pH()`, `classify_carbonate_tot()`, `classify_carbonate_att()`, `classify_CEC()`, `ratio_Mg_K()`, `ratio_K_CEC()` | Classificazione agronomica suolo |
| `R/max_SO_input.R` | `max_SO_input()`, `classify_SOM()` | Apporto max SO annuo e classificazione SO |

## Phase E — Distribution plan

| File | Function | Role |
|---|---|---|
| `R/fert_plan.R` | `plan_distribution()` | Combina organici + minerali con efficienza N, alert Eccesso/Deficit vs A/P/K, ZVN 170 kg N/ha |
| `R/end_of_cycle_soil.R` | `estimate_soil_P_end_of_cycle()`, `estimate_soil_K_end_of_cycle()` | Dotazione teorica P2O5 e K2O a fine ciclo (per input anno successivo) |

## Phase F — Package-level finalisation

- `DESCRIPTION`: version → `0.3.0`, Title → "N, P and K fertilization", Description estesa.
- `NAMESPACE`: +15 export (scheda_PK, P_balance, K_balance, classify_*, plan_distribution, estimate_soil_*, etc.).
- `NEWS.md`: sezione 0.3.0 completa.
- `R/data.R`: documentazione aggiunta per i 5 nuovi dataset e le 4 nuove sezioni funzionali.
- `tests/testthat/test-PK-balance.R`: 9 nuovi test che verificano:
  - classificazione Olsen P (classi bassa/normale/riduzione)
  - `P_balance` grano duro = 112.9 kg P2O5/ha (match Bilancio Excel)
  - classificazione K per raggruppamento tessiturale
  - `K_leaching_by_clay` step function (60/30/20/10)
  - `K_balance` grano duro = 139.4 kg K2O/ha (match Bilancio Excel)
  - `plan_distribution` con organico + urea
  - end-of-cycle soil P/K
  - `scheda_PK` base doses
  - soil chemistry classifiers (pH, calcare, CEC, SO input)
- `vignettes/NFert-PK-and-distribution.Rmd`: vignetta con scenario grano duro completo (N + P + K + scheda + distribuzione + dotazione a fine ciclo + classificazione).

## Validated against Fert_Office v1.26 benchmark

Scenario: grano duro pianta intera, 6 t/ha, suolo FL medio impasto
(Sabbia 15.5%, Argilla 18.5%), Olsen P 15 ppm P2O5, K 150 ppm K2O:

| Component | Fert_Office | NFert 0.3.0 | Match |
|---|---:|---:|:---:|
| A (N) | 186.6 kg/ha | 186.6 | ✓ |
| A (P2O5) | 63.6 | 63.6 | ✓ |
| A (K2O) | 119.4 | 119.4 | ✓ |
| B (N) | 73.84 | 73.84 | ✓ |
| D (N, include −E) | 39.536 | 39.536 | ✓ |
| G (N) | 10.05 | 10.05 | ✓ |
| **Dose N ammessa** | **142.246** | **142.246** | **✓** |
| B1 (P arricchimento) | 49.296 | 49.296 | ✓ |
| **Dose P2O5** | **112.9** | **112.9** | **✓** |
| H (K lisciviazione) | 20 | 20 | ✓ |
| **Dose K2O** | **139.4** | **139.4** | **✓** |
| Scheda N (ristoppio + compattato) | 200 | 200 | ✓ |

## Next (v0.4.0 idea)

- Export CSV import helpers (`import_fert_office_xlsm()`).
- GIS: map soil P/K dotation rasters.
- PDF / HTML output of plan_distribution as "Registra_Piano".
- CRAN submission.
