---
title: "Guide to Integrated Fertilization – N, P, K"
subtitle: "Herbaceous and tree crops – DPI Emilia-Romagna 2026"
author: "FertDPI / Fert_Office_v1_26"
date: "2026"
description: "Mineral and organic fertilization and all balance method equations"
---

# Guide to Integrated Fertilization

> **Nitrogen – Phosphorus – Potassium** for herbaceous and tree crops
>
> Mineral and organic fertilization and all equations of the balance method
>
> *Reference: DPI Emilia-Romagna 2026 | Tool: FertDPI / Fert_Office_v1_26*

## 1. Introduction and regulatory framework

The fertilization plan is a mandatory document for farms adhering to the Integrated Production Guidelines (DPI) of the Emilia-Romagna Region. It rationalizes N, P and K applications according to actual crop needs, soil characteristics and fertilizers used. The FertDPI application (Fert_Office_v1_26, 2026 edition) automatically implements the calculations of Annex 2 to the General Rules.

### 1.1 Fertilization plan deadlines

|                              |                      |                            |
|------------------------------|----------------------|----------------------------|
| **Crop type**                | **Draft**            | **Final version**          |
| Herbaceous and forage crops  | by 28 February       | 45 days before harvest     |
| Vegetable crops              | by 15 April         | 15 days before harvest     |
| Tree crops and seed crops    | by 15 April         | by 15 September            |

> **⚠ Note:** Each fertilization intervention must be recorded within 7 days in the DPI 2025 field sheets (herbaceous or tree crops), also updating the fertilizer warehouse register (in/out).

## 2. Soil analysis

Soil analysis (valid 5 years) is the basis of the plan. As an alternative to laboratory analyses, geostatistical analyses from the ER Soil Catalogue may be used, with permanent validity. Soil data determine: texture class (and thus effluent efficiency coefficients and P and K immobilization factors), SOM and total N level (for B1 and B2), and P and K level (for B1 and enrichment/reduction calculations).

### 2.1 Texture groupings and derived parameters

|                           |        |                   |                       |                          |
|---------------------------|--------|-------------------|-----------------------|--------------------------|
| **Grouping**              | **ID** | **USDA classes**  | **Bulk density (t/m³)** | **P immobilization factor (a)** |
| Tendentially sandy        | 1      | S, SF, FS         | 1.4                   | 1.4                      |
| Loam / Medium texture     | 2      | L, F, FL, FSA, FA | 1.3                   | 1.3                      |
| Tendentially clayey       | 3      | FLA, AS, AL, A    | 1.2                   | 1.2                      |

> The phosphorus immobilization factor 'a' (1.4 / 1.3 / 1.2) is used in the formula C = a + 0.02 × total limestone (%) to calculate the phosphatic fertilization factor C.

## 3. All equations of the Balance method (DPI Annex 2)

This section reports in explicit form all equations implemented in FertDPI for the fertilization plan with the balance method. The application’s hidden sheets (B, C&D, E, F, G, Gri_P, Gri_K, C_tempo, Efficienza) contain the reference numerical parameters.

### 3.1 General Nitrogen equation

**Main N formula (kg/ha):**

> **N = A − B + C + D − E − F − G**
>
> *where A = crop requirement; B = soil fertility; C = leaching losses; D = immobilization; E = N from previous crop; F = N from previous years’ organic; G = natural inputs*

#### 3.1.1 A – Crop requirement

**Herbaceous crops:**

> **A_N = N_uptake_coeff (kg/t) × average_yield (t/ha)**
>
> *Example: durum wheat, N coeff. = 31.1 kg/t, yield 6 t/ha → A_N = 186.6 kg/ha*

**Tree crops – general A formula (fruit + pruned wood + leaves):**

> **A_tree = N_uptake_coeff (kg/t) × average_production (t/ha)**
>
> *Production is the part removed annually from the field (fruit + wood + leaves). Coefficients from FertDPI sheet 'A' | Production = 3-year farm average or ISTAT data*

Table of unit uptake coefficients for main tree crops (FertDPI sheet 'A'):

|                                        |                     |                        |                       |                       |                          |
|----------------------------------------|---------------------|------------------------|-----------------------|-----------------------|--------------------------|
| **Crop (removed part)**                | **N coeff. (kg/t)** | **P₂O₅ coeff. (kg/t)** | **K₂O coeff. (kg/t)** | **Ref. prod. (t/ha)** | **Standard A_N (kg/ha)** |
| Actinidia green pulp (fr+wood+leaves)  | 0.59                | 0.16                   | 0.59                  | 25                    | 147                      |
| Apricot medium prod. (fr+wood+leaves)  | 0.55                | 0.13                   | 0.53                  | 13                    | 71                       |
| Cherry (fr+wood+leaves)                | 0.67                | 0.22                   | 0.59                  | 9                     | 60                       |
| Apple (fr+wood+leaves)                 | 0.29                | 0.08                   | 0.31                  | 40                    | 116                      |
| Blueberry                              | 0.14                | 0.07                   | 0.10                  | 17.5                  | 24                       |
| Nectarine (fr+wood+leaves)             | 0.64                | 0.14                   | 0.53                  | 25                    | 160                      |
| Hazelnut (fr+wood+leaves)              | 3.15                | 1.35                   | 2.90                  | —                     | —                        |
| Walnut (fr+wood+leaves)                | 3.20                | 1.00                   | 1.30                  | 4                     | 128                      |
| Pear medium prod. (fr+wood+leaves)     | 0.33                | 0.08                   | 0.33                  | 30                    | 99                       |
| Peach (fr+wood+leaves)                 | 0.58                | 0.17                   | 0.58                  | 25                    | 145                      |
| Plum (fr+wood+leaves)                  | 0.49                | 0.10                   | 0.49                  | 25                    | 122                      |
| Table grape (bunches+canes+leaves)     | 0.51                | 0.06                   | 0.48                  | 25                    | 127                      |
| Wine grape plain (bunches+wood+leaves)  | 0.62                | 0.28                   | 0.74                  | 20                    | 124                      |
| Wine grape hill (bunches+canes+leaves)  | 0.57                | 0.26                   | 0.67                  | 10                    | 57                       |

> **⚠ Note:** Unit coefficients (kg/t) represent the element (N, P₂O₅ or K₂O) removed per tonne of total product removed from the field. For perennial crops in the establishment phase, requirement A is calculated on the expected average yield at full production.

#### 3.1.2 B – Soil fertility (mineralization)

Term B has two components: B1 (N from SOM mineralization) and B2 (readily available N from soil total N). Both are multiplied by the C_tempo coefficient, which accounts for crop cycle length and phase.

**Component B1 – SOM mineralization:**

> **B1 = SOM (t/ha) × Mineralization_coeff × C_tempo**
>
> *SOM (t/ha) = SOM(%) / 100 × soil_weight_20cm (t/ha). Soil weight 20 cm: sandy = 2,800 t/ha; loam = 2,600 t/ha; clay = 2,400 t/ha. Mineralization_coeff depends on texture grouping and C/N ratio.*

**Component B2 – Readily available mineral N:**

> **B2 = N_tot (t/ha) × Readily_N_coeff × C_tempo**
>
> *N_tot (t/ha) = N_tot (g/kg) / 1000 × soil_weight_20cm (t/ha). B2 is calculated only for annual crops; for perennials only B1 is used.*

**Total B available to the crop:**

> **B_total = B1 + B2**
>
> *For perennial crops: B = B1 (B2 is not counted for system stability).*

C_tempo multiplier table by crop phase/cycle (FertDPI C_tempo sheet):

|                               |             |           |                                               |
|-------------------------------|-------------|-----------|-----------------------------------------------|
| **Crop phase / cycle**        | **C_tempo** | **Type**  | **Meaning**                                   |
| Pre-planting                  | 0.20        | Tree      | Only 20% of annual B1 is counted              |
| 1st year establishment       | 1.00        | Tree      | 100% – but N applications limited to 40%     |
| 2nd year establishment       | 1.00        | Tree      | 100% – N applications limited to 50%         |
| Full production              | 1.00        | Tree      | 100% with no phase limits                     |
| Spring–summer &lt; 70 days   | 0.30        | Herbaceous| 30% of annual mineralization                  |
| Spring–summer 70–100 days    | 0.50        | Herbaceous| 50% of annual mineralization                  |
| Spring–summer 100–130 days   | 0.67        | Herbaceous| 67% of annual mineralization                  |
| Spring–summer &gt; 130 days  | 0.75        | Herbaceous| 75% of annual mineralization                  |
| Autumn–winter &lt; 150 days  | 0.50        | Herbaceous| 50% of annual mineralization                  |
| Autumn–winter &gt; 150 days   | 0.60        | Herbaceous| 60% of annual mineralization                  |
| 1st cut (forage)              | 0.11        | Forage    | 11% – very short cycle                        |
| 2nd and subsequent cuts       | 0.10        | Forage    | 10% per cut                                   |

**B1_effective = B1_table × C_tempo (explicit equation):**

> **B1_effective = [SOM (t/ha) × Min_coeff] × C_tempo**
>
> *Example: SOM 2%, loam soil → SOM (t/ha) = 52 t/ha; Nm = 52 × 2.0 (mean coeff.) = 104 → B1_eff = 104 × 0.67 ≈ 70 kg/ha (100–130 day cycle).*

#### 3.1.3 C – Leaching losses (Ca and Cb)

Leaching is split into Ca (autumn–winter losses) and Cb (late winter, February). Calculated only for autumn–winter crops on soils at risk of leaching.

**Ca – Readily available N loss in autumn–winter:**

> **Ca = N_readily × (autumn_winter_rain − 150) / 100**
>
> *where N_readily = maximum leachable N (kg/ha) from table; threshold = 150 mm; 100 mm = rain range for complete loss of N_readily. Maximum leachable N by texture and drainage (table):*

|                        |                          |                        |                     |
|------------------------|--------------------------|------------------------|---------------------|
| **Texture**            | **Poor drainage**        | **Moderate drainage**  | **Good drainage**  |
| Sandy                  | 30 kg/ha                 | 40 kg/ha               | 50 kg/ha            |
| Loam – Medium         | 20 kg/ha                 | 30 kg/ha               | 40 kg/ha            |
| Clay and silty         | 10 kg/ha                 | 20 kg/ha               | 30 kg/ha            |

**Cb – N loss at end of winter (February):**

> **Cb = (rain_OctJan + rain_Feb − 250) / 10**
>
> *Activation condition: rain_OctJan &gt; 150 mm AND (rain_OctJan + rain_Feb) &gt; 250 mm. If not satisfied → Cb = 0. Result (kg/ha) is added to Ca to get total C.*

**Explicit conditional logic for Cb:**

> **IF (rain_OctJan + rain_Feb &gt; 250 mm) THEN: loss_Cb = (rain_OctJan + rain_Feb − 250) / 10 ELSE: Cb = 0**
>
> *rain_OctJan = precipitation October–January; rain_Feb = February precipitation. Overall threshold: 250 mm | About 1 kg/ha N lost per 10 mm above threshold.*

**Total C:**

> **C = Ca + Cb**
>
> *Both are zero if rainfall is below thresholds or for summer crops.*

#### 3.1.4 D – Immobilization and dispersion

Term D represents N losses from microbial immobilization and volatilization/denitrification. The factor fc (dispersion factor) depends on texture and oxygen availability (drainage).

**Annual crops (B2 present):**

> **D = (B1 + B2) × fc**
>
> *fc includes both immobilization and dispersion over the cycle.*

**Perennial crops (B1 only):**

> **D = B1 × fc**
>
> *B2 is not counted, so D is based only on the SOM mineralization share.*

fc values (immobilization and dispersion factor) from FertDPI sheet C&D:

|                                |                          |                        |                     |
|--------------------------------|--------------------------|------------------------|---------------------|
| **Texture (grouping)**         | **Poor drainage**        | **Moderate drainage**  | **Good drainage**   |
| Sandy (ID 1)                  | **0.35**                 | **0.20**               | **0.15**            |
| Loam – Medium (ID 2)          | **0.40**                 | **0.25**               | **0.20**            |
| Clay and silty (ID 3)         | **0.45**                 | **0.30**               | **0.25**            |

#### 3.1.5 E – N from previous crop

E is the residual N released (or immobilized if negative) by the previous crop. Values are tabular from FertDPI sheet E.

**E calculation:**

> **E = table_value (kg/ha) for the previous crop**
>
> *Positive value → N release (benefit); negative value → N immobilization (extra demand).*

E values by previous crop (FertDPI sheet E): see original table (Barbabietola +30, Cereale paglia asportata −10, Cereale paglia interrata −30, Colza +20, Girasole 0, Mais stocchi asportati −10, Mais stocchi interrati −40, Medica +80, Soia +10, Leguminosa da granella +40, etc.).

#### 3.1.6 F – N from previous years’ organic fertilization

**F calculation:**

> **F = N_applied_previous_year (kg/ha) × Recovery_coeff**
>
> *Recovery coefficient depends on organic type and application frequency.*

|                                     |                    |                 |                 |               |
|-------------------------------------|--------------------|-----------------|-----------------|---------------|
| **Organic type**                    | **Every year**     | **Every 2 years** | **Every 3 years** | **Occasional** |
| Ammendante (compost, mature manure) | 0.50               | 0.30            | 0.20            | 0.20          |
| Cattle slurry / digestate          | 0.30               | 0.15            | 0.10            | 0.00          |
| Pig slurry and/or poultry manure   | 0.15               | 0.10            | 0.05            | 0.00          |
| None                               | 0.00               | 0.00            | 0.00            | 0.00          |

#### 3.1.7 G – Natural inputs (atmospheric deposition)

**G calculation:**

> **G = annual_deposition × C_tempo**
>
> *Plain near urban areas: 20 kg/ha/year | Isolated plain: 15 kg/ha/year | Hill/mountain: 10 kg/ha/year. Value is modulated by crop C_tempo.*

### 3.2 Phosphorus (P₂O₅) equations

**General P₂O₅ formula (kg/ha):**

> **P = A ± (B × C) + B1_arr + A2**
>
> *A = crop requirement; B = P deviation from normal level; C = immobilization factor; B1_arr = enrichment if low; A2 = future years’ advance (perennial crops).*

**P crop requirement (A):** A_P = P₂O₅_uptake_coeff (kg/t) × yield (t/ha). **P immobilization factor C:** C = a + (0.02 × total_limestone_%). **Enrichment/Reduction P:** 3 × Da × Q_P (Da = soil weight 30 cm in t/ha; Q_P from normality range). Normality range P₂O₅: 22.9–68.7 ppm.

### 3.3 Potassium (K₂O) equations

**General K₂O formula (kg/ha):**

> **K = E_K ± (F_K × G_K) + H + B1_arr_K + A2_K**
>
> *E_K = K crop requirement; F_K = deviation from normal; G_K = K immobilization factor; H = K leaching; B1_arr_K = K enrichment; A2_K = advance.*

**K crop requirement:** E_K = K₂O_uptake_coeff (kg/t) × yield (t/ha). **K immobilization factor:** G_K = 1 + (0.018 × clay_%). **Enrichment/Reduction K:** 3 × Da × Q_K. Normality range K varies by texture: sandy 96–144 ppm, loam 120–180 ppm, clay 144–216 ppm K₂O.

## 4. Fertilization criteria (herbaceous and tree crops)

- **Herbaceous N:** Spring–summer: apply near sowing or split top-dressing. Autumn–winter: max 30 kg/ha N pre-sowing; main share in top-dressing from February. Splitting mandatory above 100 kg/ha N from quick-release synthesis (min 2 interventions, ≥ 7 days). Slow-release fertilizers: exempt from splitting; application allowed from mid-January.
- **Tree N:** Splitting mandatory above 60 kg/ha N from quick-release (≥ 7 days). Establishment 1st and 2nd year: only localized applications. NEW 2026 – Post-harvest: autumn applications &lt; 40 kg/ha N allowed by 15 October.
- **P and K:** Non-row crops: P and K at tillage. Fertigation: no restriction. No-till: P and K need not be incorporated. Vegetables: P and K also in top-dressing.

## 5. Organic fertilization – types and composition

(Table: Liquame suino, bovino, ovaiole, letame, digestato, ammendante compostato, fanghi – with dry matter %, N, P₂O₅, K₂O, MgO, release type, category.)

**Rules:** ZVN limit 170 kg/ha/year N from livestock effluents (farm average). Composted amendments with sewage sludge: NOT allowed in DPI. Incorporation mandatory within 24 h on bare soil. Max annual amendment: 15 t DM/ha (low SOM), 13 (normal), 9 (high SOM).

## 6. Organic nitrogen efficiency – equations and tables

**Available N from a single organic fertilizer:**

> **N_available = N_total_applied (kg/ha) × Efficiency% / 100**
>
> *N_total_applied (kg/ha) = N_tot (kg/t) × quantity_applied (t/ha).*

**Weighted average farm efficiency (multiple effluents):**

> **Eff_avg = Σ(Ni × ei) / Σ(Ni)**
>
> *Ni = total N from i-th fertilizer; ei = efficiency % of i-th fertilizer.*

**Efficiency levels:** 3 – HIGH (efficient): injection, fertigation, incorporation &lt; 4 h. 2 – MEDIUM: band spread, incorporation within 24 h. 1 – LOW: surface spread without incorporation.

**Ammendanti (compost, mature manure, palatable digestate):** fixed 40% available N regardless of technique.

Efficiency tables by texture (sandy / loam / clay) for liquame avicolo, bovino, suino, digestato t.q. bovini/suini/avicoli, digestato chiarificata, fanghi, ammendante 40% — see original sections 6.4.1–6.4.3.

## 7. Standard dose sheet method (Annex 3)

Simplified method from a base dose per crop, adjusted by factors (yield, SOM, previous crop, surplus rain, etc.). FertDPI always shows comparison with the balance method.

## 8. Distribution planning – sheet 'Distribuz'

Organic section: select up to 3 organic fertilizers with N/P/K/DM%, quantity, timing and method. Efficient N is calculated automatically. Mineral section: up to 8 fertilizers. Totals and comparison with calculated doses; 'Excess' alert (does not check MAS or ZVN limits). Integrate organic and mineral: compute useful N/P/K from organics first, then add minerals for the residual share within MAS.

## 9. Operational checklist

(Checklist: soil analysis validity, farm data, crop and yield, soil data, precession and organics, autumn–winter rain, balance or standard method, check MAS, C_tempo, P/K factors, organic selection and efficiency, plan minerals, check excess and MAS, weighted N_available, save Registra_Piano, final P and K for next year, record interventions within 7 days, update fertilizer register.)

---

*Document based on: DPI ER 2026 – General Rules (Annex 2) | FertDPI Fert_Office_v1_26 | Reg. ER 3/2017 | Nitrates Directive 91/676/EEC*
