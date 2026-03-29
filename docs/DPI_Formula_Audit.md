# DPI formula audit vs NFert package

This document compares the **Guida alla Fertilizzazione DPI 2026** (FertDPI Fert_Office_v1_26) with the NFert implementation and lists corrections and improvements.

## 1. General N balance

**DPI formula:** `N = A − B + C + D − E − F − G`

| Term | DPI meaning | NFert | Status |
|------|-------------|--------|--------|
| A | Crop N requirement | `calc_crop_N_demand()` | ✅ |
| B | Soil fertility (B1+B2) | `soil_fertility()` → B_total | ✅ (see B1/B2 below) |
| C | Leaching (Ca + Cb) | `leaching_loss()` → C1, C2 | ⚠ Formula detail differs |
| D | Immobilization + dispersion | `calc_N_immobilization_loss(B, …)` | ✅ (fc from cb.table) |
| E | N from previous crop residues | `nitrogen_from_previous_crop_residues()` | ✅ |
| F | N from **previous years’ organic** | Not implemented | ❌ **Missing** |
| G | Natural deposition | `natural_contribution()` | ✅ |
| (current-year organic) | Efficient N from organic this year | `organic_fertilization()` → Forg | ✅ |

**Action:** Add term **F** (residual N from organic applied in previous years) to the balance. Formula: `F = N_applied_previous_year × Recovery_coeff` with recovery by organic type and frequency (see Guida §3.1.6).

---

## 2. B1 vs B2 naming (DPI vs package)

**DPI (Guida):**
- **B1** = N from **mineralization of SOM** (SOM t/ha × coeff × C_tempo)
- **B2** = **Readily available N** from soil total N (N_tot t/ha × coeff × C_tempo)

**NFert (`soil_fertility`):**
- **b1** = Ntot × coef.N.readily → **readily available** (matches **DPI B2**)
- **b2** = SOM × coef.N.mineralised × C_tempo → **mineralized from SOM** (matches **DPI B1**)

So: **package b1 = DPI B2**, **package b2 = DPI B1**. The total B = b1 + b2 is correct; only the component names are reversed with respect to DPI. Downstream use (e.g. leaching uses “readily” N) is consistent.

**Action:** Document this mapping in `?soil_fertility` and in the vignette (e.g. “b1 = readily available N, DPI B2; b2 = mineralized from SOM, DPI B1”) so users are not confused.

---

## 3. Soil weight (20 cm) for B1 and B2

**DPI:** B1 uses SOM (t/ha) = SOM(%)/100 × **soil_weight_20cm** with 2,800 (sandy), 2,600 (loam), 2,400 (clay) t/ha. B2 uses N_tot (t/ha) = N_tot (g/kg)/1000 × same weight.

**NFert:** Uses Ntot and SOM with coefficients that may already embed a reference weight or different units (e.g. Ntot in ‰ or g/kg, SOM in %). No explicit 20 cm soil weight in code.

**Action:** Verify that internal coefficients in `coefN_readily` and `coefN_mineralised` are consistent with DPI (and, if needed, add conversion via soil weight by texture). Optional: add a note in docs that DPI uses 2,800 / 2,600 / 2,400 t/ha for 20 cm.

---

## 4. Leaching C = Ca + Cb

**DPI:**
- **Ca** = N_readily × (autumn_winter_rain − 150) / 100, with N_readily from a **table** (30–50 kg/ha sandy, 20–40 loam, 10–30 clay by drainage). Threshold 150 mm; 100 mm interval for full loss.
- **Cb** = (rain_OctJan + rain_Feb − 250) / 10 **if** rain_OctJan > 150 **and** (rain_OctJan + rain_Feb) > 250; else 0.

**NFert:** `leaching_loss()` uses **b1** (readily available N) for the proportion of loss and combines with a lookup **C** from `cb.table` (ID_Rag, ID_Dre). C1 and C2 are computed with a different structure (winter vs late rain and February).

**Action:** Align leaching logic with DPI where possible: (1) use a **max leachable N** table by texture and drainage (N_readily) for Ca; (2) implement Cb exactly as (rain_OctJan + rain_Feb − 250) / 10 with the same activation condition. Keep backward compatibility or document the current behaviour as “FertDPI‑compatible” if tables match.

---

## 5. D – Immobilization factor fc

**DPI:** D = (B1+B2)×fc or B1×fc (perennials). fc by texture and drainage: sandy 0.35/0.20/0.15, loam 0.40/0.25/0.20, clay 0.45/0.30/0.25 (poor / moderate / good drainage).

**NFert:** D = B × fc_D from `cb.table` (ID_Rag, ID_Dre → fc_D).

**Action:** Ensure `cb.table` fc_D values match the Guida fc table (by ID_Rag and drainage/oxygen). Document that D is applied to total B (B1+B2) for annual crops.

---

## 6. F – Previous years’ organic fertilization

**DPI:** F = N_applied_previous_year (kg/ha) × Recovery_coeff. Recovery by type: Ammendante 0.50/0.30/0.20/0.20 (every year / 2 years / 3 years / occasional), Cattle slurry 0.30/0.15/0.10/0.00, Pig/poultry 0.15/0.10/0.05/0.00.

**NFert:** F is not implemented. Only E (crop residues) and Forg (current-year organic) are in the balance.

**Action:** Add optional parameters to `N_balance()` (e.g. `organic_previous_year_N`, keep `fertorg_frequency` and organic `source` or type) and a recovery table; compute F and subtract it in the balance. If not provided, F = 0.

---

## 7. G – Natural deposition

**DPI:** G = annual_deposition × C_tempo. 20 / 15 / 10 kg/ha/year (plain urban / plain isolated / hill).

**NFert:** `natural_contribution(location, ccp)` returns `annual_deposition * C_tempo`. ✅

---

## 8. Organic N efficiency

**DPI:** N_available = N_total_applied × Efficiency% / 100. Efficiency by effluent type, texture and distribution (efficient / medium / low). Ammendanti 40% fixed.

**NFert:** `organic_N_efficiency()` and `organic_fertilization(..., soil_group, distribution_efficiency)` implement this. ✅

**Action:** Allow `N_balance()` to pass through `soil_group` and `distribution_efficiency` to `organic_fertilization()` so that Forg uses DPI efficiency when the user provides soil and distribution.

---

## 9. P and K

**DPI:** Full P and K balance equations (A, immobilization C/G, enrichment/reduction, 30 cm soil weight). **NFert:** P and K are not implemented; only N balance. Documented as out of scope. No change required unless P/K implementation is planned.

---

## 10. Summary of code changes

| Priority | Item | Action |
|----------|------|--------|
| High | Add F (previous years’ organic) | New recovery table; optional args in `N_balance()`; subtract F in formula. |
| High | Document B1/B2 mapping | In `soil_fertility` and vignette: b1 = DPI B2 (readily), b2 = DPI B1 (mineralized). |
| Medium | DPI efficiency in balance | Add optional `soil_group`, `distribution_efficiency` to `N_balance()` and pass to `organic_fertilization()`. |
| Medium | Leaching Ca/Cb | Align with DPI N_readily table and Cb condition; document or adjust `leaching_loss()`. |
| Low | Soil weight 20 cm | Document or add conversion for B1/B2 if coefficients assume different units. |
| Low | cb.table fc_D | Verify values match Guida fc table. |

Reference: `Guida_Fertilizzazione_DPI_v2.md` and `Guida_Fertilizzazione_DPI_English.md`.
