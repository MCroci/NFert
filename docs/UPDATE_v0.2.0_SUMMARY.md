# NFert 0.2.0 – Summary of changes (Fert_Office v1.26 sync)

Date: 2026-04-15
Scope: Phases A, B, C of the update plan in `UPDATE_PLAN_Fert_Office_v1.26.md`.

## Phase A — data sync (DPI 2026)

All reference tables were re-generated from `Fert_Office_v1._26.xlsm` (Regione
Emilia-Romagna, Febbraio 2026). Files under `data/`:

### Updated
- `uptake_table.rda` — expanded to 236 crops with N, P2O5 and K2O coefficients, yield references and `parte_asportata`.
- `coef_time.rda` — now carries DPI 2026 percentages for N, P2O5 and K2O allowed per phase, plus `anticipazioni` and `allevamento` flags; keeps English `ccp` column for backward compatibility and adds `ccp_it` alias.
- `e.table.rda`, `f.table.rda`, `g.table.rda` — primary English columns preserved, Italian aliases added (`*_it`).

### Added
| File | Purpose |
|---|---|
| `mas.table.rda` | 239 rows: MAS DPI 2026, RR 2/2024 |
| `crops.table.rda` | Crop master list (genus, species, N-fix %, detrazione sodo) |
| `gruppo.table.rda` | 6 crop macro-groups |
| `ragg_tes.table.rda` | Texture groupings with peso_specifico, P immobilization, weights |
| `so.table.rda`, `so_max_input.rda` | SOM classes & max SO input (t ss/ha) |
| `ph.table.rda` | 7 pH classes |
| `calcare_tot.table.rda`, `calcare_att.table.rda` | Carbonate classes |
| `gri_p.table.rda`, `gri_p_meta.rda` | Olsen P availability & P↔P2O5 conversion |
| `gri_k.table.rda` | Exchangeable K availability by texture |
| `tipo_fert.table.rda` | 11 fertilizer type classes |
| `fert_org.table.rda` | 21 organic matrices with ss, N, P2O5, K2O |
| `efficienza.table.rda` | 220 rows: N efficiency (soil × level × sector × dose) |
| `mod_distribuz.table.rda` | 22 distribution modalities |
| `ciclo_modalita.table.rda` | 32 crop-cycle × distribution combos |
| `cicli.table.rda`, `cicli_fase.table.rda` | Crop cycles and phase-duration |
| `cd.table.rda` | Combined C / fc_D lookup (foglio C&D) |

### Untouched
`ca.table.rda`, `cb.table.rda`, `coefN_readily.rda`, `coefN_mineralised.rda`, `soil.table.rda`, `tri2.table.rda`, `tri3.table.rda`, `s2.rast.rda` (already correct).

## Phase B — N function fixes

- `soil_fertility()` gains `soil_seeding = c("traditional","no-till")`. In no-till a `-3 kg/ha` detrazione is applied on `b1` (foglio B cella L5).
- `calc_N_immobilization_loss()` gains `greenhouse = FALSE` (+2 kg/ha on D, foglio C&D D23) and `E_residual = 0` (if negative, `|E|` added to D per foglio C&D I21).
- `leaching_loss()` now returns `surplus_pluviometrico` logical (TRUE if winter_rain + start_spring_rain ≥ 300 mm).
- `N_balance()` gains:
  - `soil_seeding`, `greenhouse` (piped through)
  - `E_to_D = TRUE` (default) – folds negative precessione E into D so the output matches the Bilancio sheet row-by-row
  - Column `surplus_pluviometrico` added to the returned data frame.
- Backward compatibility: all existing callers that use positional args or the pre-0.2.0 keyword set continue to work because new parameters have defaults.

## Phase C — new function `scheda_N()`

DPI 2026 "Scheda a dose Standard" for N: starts from `mas.table$N_standard`, applies
decrement and increment flags/values, caps at `dose_max_N` (MAS + incremento_max).

Signature:
```r
scheda_N(crop, phase = NULL,
         decrements = numeric(),  # yield_low, prev_amendant, high_SOM,
                                  # after_alfalfa_meadow, after_legume
         increments = numeric(),  # yield_high, low_SOM, straw_burial,
                                  # rain_surplus, compacted_no_till
         mas.table = NFert::mas.table,
         apply_mas_cap = TRUE)
```

Returns a list with `dose_base`, `total_decrement`, `total_increment`,
`dose_recalculated`, `dose_max_N`, `dose_final`, `mas_exceeded`.

## Package-level updates

- `DESCRIPTION`: version bumped to `0.2.0`.
- `NAMESPACE`: adds `export(scheda_N)`.
- `NEWS.md`: new 0.2.0 section documenting this release.
- `R/data.R`: docs updated with all new datasets.
- `tests/testthat/test-dpi2026-benchmark.R`: 6 new tests validating the
  grano-duro Fert_Office benchmark (Bilancio = 142.25 kg/ha, Scheda = 200 kg/ha),
  `soil_seeding`, `greenhouse`, `E_to_D` and `surplus_pluviometrico`.

## Next steps (not covered in this release)

Phases D, E, F of the roadmap remain open:
- Phase D: full P2O5 and K2O balance (`P_balance`, `K_balance`, PK schede, soil P/K availability).
- Phase E: fertilizer distribution plan (`plan_distribution`) and end-of-cycle soil P/K.
- Phase F: vignette for PK & distribution, CRAN submission 0.3.0.

See `docs/UPDATE_PLAN_Fert_Office_v1.26.md` for the full gap analysis.
