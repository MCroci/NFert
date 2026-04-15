# Piano di aggiornamento di NFert sulla base di Fert_Office v1.26 (Febbraio 2026)

Questo documento confronta il file Excel regionale **Fert_Office_v1._26.xlsm** (DPI Emilia-Romagna 2026, aggiornamento febbraio 2026, curato da C. Ferronato – Regione ER) con lo stato corrente del pacchetto **NFert 0.1.0** e indica in modo puntuale le tabelle, le formule e le funzioni da aggiungere o rivedere.

## 1. Mappa dei fogli del file Excel

Il file contiene 42 fogli; di seguito la loro funzione e il loro stato di copertura in NFert.

| Foglio | Stato | Funzione | Corrispondenza NFert |
|---|---|---|---|
| Istruzioni | visibile | Note d'uso, versione DPI 2026 | – |
| Inserimento | visibile | UI di input (azienda, coltura, suolo, meteo, precessione) | parametri di `N_balance()` |
| Coltura | nascosto | Elenco colture (241 righe) con ID, gruppo, genere/specie, N-fissazione, limite N predefinito, detrazione sodo | parzialmente in `uptake_table` |
| Gruppo | nascosto | 6 gruppi: arboree, erbacee, foraggere, orticole, da seme, baby life | mancante come tabella esplicita |
| A | nascosto | Fabbisogni colturali: coefficienti unitari N/P₂O₅/K₂O (ass./asp.), rese standard, parte asportata | solo N coperto in `uptake_table` |
| MAS | nascosto | Massimali DPI 2026 (258 righe): N max, resa di riferimento, resa standard, incremento max, dose max, riferimento normativo (RR 2/2024) | coperto da `get_MAS()` ma **lista colture e valori da ri-sincronizzare** |
| Standard | nascosto | Parametri "dose standard" (82 colonne!) per il metodo semplificato (scheda N e PK) | **non implementato** |
| Bilancio | visibile | Output metodo del bilancio N+P₂O₅+K₂O | N coperto; P e K **non implementati** |
| Scheda_N | visibile | Metodo dose standard N (incrementi/decrementi) | **non implementato** |
| Scheda_PK | visibile | Metodo dose standard P₂O₅ e K₂O | **non implementato** |
| Distribuz | visibile | Piano di distribuzione (organici + minerali, epoche, efficienze, titoli) | **non implementato** |
| Registra_Piano | visibile | Versione stampabile del piano di distribuzione + dotazione PK teorica a fine ciclo | **non implementato** |
| Apporti al terreno | visibile | Calcolo dotazione P₂O₅/K₂O a fine ciclo | **non implementato** |
| Valori da riportare | nascosto | Celle di appoggio | – |
| Codici | visibile | Pivot ID → stringhe | – |
| Cicli | nascosto | 6 cicli colturali (autunno-vernino, intercalare, pluriennale arboree/erbacee, primaverile-estivo, indefinito) | parzialmente in `coef_time` |
| Cicli_Fase | nascosto | Fasi x durata x stagione (17 combinazioni) | parzialmente in `coef_time` |
| Ciclo_Modalità | nascosto | Efficienza organici × fase ciclo × modalità distribuzione (48 righe) | **non implementato** |
| Mod_distribuz | nascosto | 22 modalità di distribuzione (epoca+tecnica) con soglie di dose | **non implementato** |
| Metodo | nascosto | Selettore Bilancio/Schede | – |
| C_tempo | nascosto | Coefficienti C_tempo × fase, % apporti consentiti N/P/K, stagione | presente in `coef_time` ma da **aggiornare** (colonne P₂O₅/K₂O mancanti) |
| Concimi | nascosto | Database concimi minerali e organo-minerali (**1569 righe**) con titoli | **non implementato** |
| Fert_org | nascosto | 21 matrici organiche con ss, N, P₂O₅, K₂O (min/max/media), incremento SO, flag zootecnico | parzialmente in `f.table` (solo N, frequenze) |
| Mod_distribuz | nascosto | (vedi sopra) | |
| Efficienza | nascosto | Tabella efficienza organici (227 righe) con combinazioni suolo × livello × settore × dose N | coperto parzialmente da `organic_N_efficiency()` – **matrice da ampliare** |
| Tipo_Fert | nascosto | 11 classi di concimi (letame, compost, lettiera polli, liquami, digestato, pollina preess., ammendanti, organo-min., min.) | **non implementato** |
| Suolo | nascosto | 12 classi USDA → 3 raggruppamenti DPI (Sabbiosi/Medio impasto/Argillosi e limosi) + classe tessiturale | presente in `soil.table` |
| Ragg_Tes | nascosto | Raggruppamenti con peso specifico (1.2/1.3/1.4 t/m³), fattore immobilizzazione P, peso a 20–50 cm | **non implementato** (usato per dotazioni finali) |
| CN | nascosto | 3 classi C/N (basso <9, equilibrato 9–12, alto >12) | presente in `coefN_mineralised` |
| B | nascosto | Fertilità del suolo: classi SO × raggruppamento, coeff. mineralizzazione, b1, b2, B, detrazione sodo | coperto; la **detrazione per semina su sodo (-3)** è esposta ma va integrata meglio |
| C&D | nascosto | Lisciviazione, immobilizzazione, effetto precessione (serra = 2 kg/ha detrazione, surplus pluviometrico >300 mm) | coperto ma **controllare flag serra / surplus** |
| E | nascosto | 21 precessioni con N kg/ha e flag leguminosa | presente in `e.table` |
| F | nascosto | N da fertilizzazioni organiche anni precedenti (4 matrici × 4 frequenze) | presente in `f.table` |
| G | nascosto | 3 ubicazioni × deposizione atmosferica annua (20/15/10 kg/ha) | presente in `g.table` |
| TRI1, TRI2, TRI3 | nascosto | Triangoli tessiturali | presenti in `tri2.table` e `tri3.table` (manca **TRI1**) |
| SO | nascosto | Classi SO (12) × apporto max annuo (9/11/13 t ss/ha) | **non implementato** |
| pH | nascosto | 7 classi di pH (da fortemente acido a fortemente alcalino) | **non implementato** |
| Calcare | nascosto | Calcare totale (5 classi) e calcare attivo (4 classi) | **non implementato** |
| Gri_P | nascosto | Dotazione P Olsen (ppm): 5 classi, conversione P↔P₂O₅ (×0,4367), fattore immobilizzazione 1.6, B1/B2/A (arricchimento/mantenimento/riduzione) | **non implementato** |
| Gri_K | nascosto | Dotazione K (ppm) × raggruppamento, classi Mg e CSC, rapporti Mg/K e K/CSC, lisciviazione argilla, correzione K | **non implementato** |

## 2. Allineamenti normativi 2026

Novità dichiarate rispetto alla v1.25 e ai testi DPI 2025:

- **Denominazione del tool**: Fert_Office_26, riferimento DPI 2026 ("Norme di riferimento DPI 2026" nel foglio Inserimento).
- **Alert ZVN**: il tool segnala superamento del MAS ma **non** del limite ZVN 170 kg N/ha medio aziendale né del vincolo ZVN in distribuzione.
- **Riferimento MAS**: "RR n. 2/2024" citato in colonna R del foglio MAS (va mantenuto nel campo `note` di `get_MAS()`).
- **Concimi minerali**: database esteso a **1569 record** con titoli (N, P₂O₅, K₂O), nuovi organo-minerali.
- **Termine B ridefinito**: B = b1 (N pronto) + b2 (N da SO), con b2 = SO × Peso_20cm × Coeff_min(classe SO × ID_Rag × ID_CN) × C_tempo – la formula è **già allineata** in NFert, ma la detrazione Sodo (−3 kg/ha) e il surplus pluviometrico (pioggia 1/10–28/2 ≥ 300 mm) vanno verificati.
- **D**: D = B × f_D(ID_Rag, ID_Dre). Coefficienti f_D (0.15–0.45) in `C&D!I4:I12` – allineati.
- **E negativo**: le precessioni con residui che immobilizzano (es. mais stocchi interrati E=−40, cereali paglia interrata E=−30, sorgo −40) vanno conteggiate a D e non a E – questo è ben esplicitato nel foglio C&D (cella I21 "da conteggiare"). **Controllare che NFert lo faccia.**
- **Fase ciclo fattore tempo**: colonne `anticipaz.` e `allevamento` (sì/no) per abilitare anticipazioni PK – non presenti in `coef_time`.

## 3. Gap principali (mancanti in NFert)

### 3.1 Modelli P₂O₅ e K₂O (bilancio + scheda)

Il pacchetto attuale copre solo **N**. Il file Excel implementa un bilancio completo per PK con:

- **A** (fabbisogno): coefficienti `asp.` per P₂O₅ e K₂O in foglio `A` per tutte le 247 colture.
- **B** (fertilità suolo): per P usa dotazione Olsen (Gri_P) e per K la dotazione scambiabile (Gri_K) per raggruppamento tessiturale.
- **B1/B2/A** (strategia):
  - **Arricchimento (B1)**: se dotazione < soglia normale → si aggiunge la differenza × peso a 30 cm (kg/ha).
  - **Mantenimento (A)**: se dotazione normale → si apporta l'asportazione.
  - **Riduzione (B2)**: se dotazione molto alta → apporto ridotto o nullo.
- **A2** (anticipazioni): per colture pluriennali in fase di impianto, si possono anticipare asportazioni future.
- **Lisciviazione K** (solo K, per argilla <30%): `Gri_K!N20..O26`.

### 3.2 Metodo "Scheda a dose standard"

Sistema alternativo al bilancio con:

- **Dose base** standard per coltura (es. grano duro: 160 kg N/ha, 80 kg P₂O₅, 120 kg K₂O).
- **Decrementi** attivabili (flag TRUE/FALSE): resa bassa, ammendante precedente, tenore SO elevato, dopo medica/prato, dopo leguminosa.
- **Incrementi**: resa alta, tenore SO basso, ristoppio/interramento paglie, lisciviazione surplus pluvio, terreno compattato/semina sodo.
- **Tetto MAS**: dopo incrementi si applica `max` dose MAS.
- Dati in foglio **Standard** (82 colonne × 247 colture) e parametri visibili in **Scheda_N / Scheda_PK**.

### 3.3 Piano di distribuzione dei fertilizzanti (`Distribuz`)

- Fino a 2 matrici organiche + N concimi minerali.
- Per ogni riga: titoli (N/P/K/ss), quantità (t/ha o q/ha), epoca-modalità (22 opzioni × ciclo colturale), efficienza calcolata da `Efficienza` (suolo × livello × settore × dose).
- Alert "Eccesso/OK" per N, P₂O₅, K₂O.
- Attualmente NFert restituisce solo la dose N totale da bilancio; **manca la ripartizione nel tempo e la verifica di eccessi**.

### 3.4 Dotazioni a fine ciclo (`Apporti al terreno`, `Registra_Piano`)

- Calcolano la dotazione teorica P e K in ppm a fine ciclo (per input dell'anno successivo), considerando apporti – asportazioni – lisciviazione K – fattori di immobilizzazione (P = 1.6, tessitura-dipendente).
- Richiede funzioni `estimate_soil_P_end_of_cycle()` / `estimate_soil_K_end_of_cycle()`.

### 3.5 Altri gap minori

- **Triangolo tessiturale TRI1** (in `TRI1`): attualmente NFert ha solo TRI2 e TRI3. TRI1 è la classe FAO/ISSS (7 classi).
- **Classe pH, classe calcare, classe CSC, classe Mg, rapporti Mg/K e K/CSC**: valutazioni agronomiche collegate al PK.
- **Apporto SO annuo massimo**: 9/11/13 t ss/ha su base classe SO (per ammendanti).
- **Flag "in serra" (C&D!D23)**: aggiunge 2 kg/ha di detrazione a D.

## 4. Roadmap di aggiornamento proposta

### Fase A – Aggiornamento dati DPI 2026 (1–2 giornate)

Ri-import delle tabelle esistenti con i valori v1.26 per essere sicuri che siano identici ai nuovi:

1. Esportare da `Fert_Office_v1._26.xlsm` in CSV:
   - `Coltura` → `crops.csv` (241 righe: ID_Coltura, coltura, Gruppo, Genere, specie, N-fissazione, ID_Cic, limite N, detrazione sodo).
   - `A` → `a.table.csv` (coefficienti ass./asp. N, P₂O₅, K₂O; rese standard).
   - `MAS` → `mas.table.csv` (N max, P₂O₅ max, dose max con incremento, nota normativa).
   - `E`, `F`, `G`, `Suolo`, `CN`, `Cicli`, `Cicli_Fase`, `C_tempo`, `C&D`, `B` (coeff_min), `Efficienza`, `Fert_org`.
2. Rigenerare i `.rda` con `usethis::use_data(..., internal = FALSE, overwrite = TRUE)`.
3. Aggiornare `data/uptake_table.rda`: oggi ha solo N; aggiungere colonne `P2O5_asp`, `K2O_asp`, `yield_ref`, `parte_asportata`, `group`, `N_fix_pct`, `id_cic`.
4. Aggiungere i nuovi dataset mancanti: `a.table`, `mas.table`, `crops.table`, `gruppo.table`, `ragg_tes.table`, `so.table`, `ph.table`, `calcare.table`, `gri_p.table`, `gri_k.table`, `tri1.table`, `mod_distribuz.table`, `ciclo_modalita.table`, `tipo_fert.table`, `concimi.table` (database minerali), `fert_org.table` (caratteristiche organici).

### Fase B – Revisione funzioni N esistenti (mezza giornata)

1. `N_balance()`: aggiungere parametri `soil_seeding = c("traditional","no-till")` (Sodo → −3 kg/ha su b1) e `greenhouse = FALSE` (serra → +2 a D), allineati al foglio B/C&D.
2. `calc_N_leaching_loss()`: implementare il flag "surplus pluviometrico" (pioggia 1/10–28/2 ≥ 300 mm) usato nelle schede.
3. `calc_N_from_crop_residues.R`: se E < 0 non sottrarre da disponibilità, sommare a D (il file Excel fa esattamente questo in `C&D!I21`).
4. `get_MAS()`: allineare i valori alle 258 righe del foglio MAS (edition `"2026"`).
5. `organic_N_efficiency()`: ampliare la matrice usando i 227 record del foglio `Efficienza` (suolo × livello × settore × dose); includere settori `dig_tq`, `dig_sui`, `dig_avi`, `dig_chi`, `fanghi`, oltre ai già presenti.

### Fase C – Nuove funzioni N "scheda standard" (1 giornata)

```r
scheda_N(crop, fase, rese_obj, factors)
```
Input = lista di flag booleani per ognuno dei 10 fattori (5 decrementi + 5 incrementi) più valori kg/ha; output = dose N con controllo vs bilancio e vs MAS.

### Fase D – Estensione P₂O₅ e K₂O (3–5 giornate)

Nuovi file R proposti:

| File | Funzione principale | Descrizione |
|---|---|---|
| `R/calc_crop_P_K_demand.R` | `calc_crop_P_K_demand()` | Fabbisogno PK per resa e coltura dal foglio `A` |
| `R/soil_P_availability.R` | `classify_P_olsen()`, `soil_P_B1_B2_A()` | Classi Gri_P e regola B1/mantenimento/B2 |
| `R/soil_K_availability.R` | `classify_K()`, `soil_K_B1_B2_A()`, `K_leaching()` | Classi Gri_K per raggruppamento + lisciviazione argilla |
| `R/P_balance.R` | `P_balance()` | Bilancio P₂O₅ (A + B1 + D_imm − A_mantenimento − B2) |
| `R/K_balance.R` | `K_balance()` | Bilancio K₂O (A + B1 + lisciviazione − A − B2) |
| `R/scheda_N.R` | `scheda_N()` | Scheda dose standard N |
| `R/scheda_PK.R` | `scheda_PK()` | Scheda dose standard PK |
| `R/soil_pH.R` | `classify_pH()` | Classe pH |
| `R/soil_carbonate.R` | `classify_carbonate()` | Calcare totale + attivo |
| `R/soil_CEC.R` | `classify_CEC()`, `ratio_Mg_K()`, `ratio_K_CEC()` | CSC, Mg, rapporti |
| `R/max_SO_input.R` | `max_SO_input()` | Apporto max annuo SO |

### Fase E – Piano di distribuzione e dotazioni a fine ciclo (2 giornate)

| File | Funzione | Descrizione |
|---|---|---|
| `R/fert_plan.R` | `plan_distribution(fert_list, crop, phase, soil_group)` | Matrice organici+minerali con titoli, efficienza, alert eccessi vs bilancio e vs MAS |
| `R/end_of_cycle_soil.R` | `estimate_soil_P_end_of_cycle()`, `estimate_soil_K_end_of_cycle()` | Dotazione teorica a fine ciclo in ppm, usabile come input l'anno successivo |

### Fase F – Documentazione, test, vignette, versioning (1 giornata)

1. Bump versione → `0.2.0` in `DESCRIPTION` (nuove funzionalità PK e schede).
2. Aggiornare `NEWS.md` con sezione "Version 0.2.0 – Full N+P+K balance, distribution plan, DPI 2026 resync (Feb 2026)".
3. Nuova vignetta `NFert-PK-and-distribution.Rmd` con esempio completo di grano duro + melo in piena produzione.
4. Test: 1 test per ogni nuova funzione in `tests/testthat/`; aggiungere cross-check numerico rispetto ai valori esatti del foglio Bilancio (es. grano duro 6 t/ha, limo franco → A=186.6; B=73.84; D=39.536; G=10.05; dose ammessa = 142.246 kg N/ha).

## 5. Quick win immediati

Anche senza l'estensione completa PK, questi 4 aggiornamenti mirati chiudono disallineamenti con v1.26:

1. **Ri-sincronizzare `uptake_table`** con l'intero foglio `A` (asp. N) e aggiungere `coeff_asp_P2O5`, `coeff_asp_K2O`.
2. **Ri-sincronizzare `mas.table`** con tutte le 258 righe del foglio MAS (attualmente `get_MAS()` potrebbe avere valori parziali).
3. **Correggere la gestione E < 0** in `N_balance()`: precessioni che immobilizzano vanno a D, non a disponibilità negativa su E.
4. **Aggiungere flag `soil_seeding = "no-till"`** (−3 su b1) e **`greenhouse = TRUE`** (+2 su D) a `N_balance()`, `calc_N_immobilization_loss()` e `soil_fertility()`.

## 6. Script R di importazione proposto

Esempio di come importare le tabelle:

```r
library(readxl)

xlsm_path <- "Fert_Office_v1._26.xlsm"

# Fabbisogni colturali (A)
a_raw <- read_excel(xlsm_path, sheet = "A", skip = 2)
names(a_raw)[1:11] <- c("ordinato","ID_Coltura","coltura",
                        "coeff_ass_N","coeff_asp_P2O5","coeff_asp_K2O",
                        "bilancio","resa_rif","fabb_ass_N","fabb_asp_P2O5","fabb_asp_K2O")
uptake_table <- as.data.frame(a_raw[, 1:11])
usethis::use_data(uptake_table, overwrite = TRUE)

# MAS 2026
mas_raw <- read_excel(xlsm_path, sheet = "MAS", skip = 2)
mas.table <- as.data.frame(mas_raw)
usethis::use_data(mas.table, overwrite = TRUE)

# Nuove tabelle
gri_p.table  <- as.data.frame(read_excel(xlsm_path, sheet = "Gri_P", skip = 1))
gri_k.table  <- as.data.frame(read_excel(xlsm_path, sheet = "Gri_K", skip = 1))
so.table     <- as.data.frame(read_excel(xlsm_path, sheet = "SO",     skip = 2))
ph.table     <- as.data.frame(read_excel(xlsm_path, sheet = "pH",     skip = 1))
calcare.tot  <- as.data.frame(read_excel(xlsm_path, sheet = "Calcare",range = "A3:F8"))
calcare.att  <- as.data.frame(read_excel(xlsm_path, sheet = "Calcare",range = "A18:F22"))
ragg_tes.tab <- as.data.frame(read_excel(xlsm_path, sheet = "Ragg_Tes"))
fert_org.tab <- as.data.frame(read_excel(xlsm_path, sheet = "Fert_org", skip = 2))
concimi.tab  <- as.data.frame(read_excel(xlsm_path, sheet = "Concimi"))
efficienza.tab <- as.data.frame(read_excel(xlsm_path, sheet = "Efficienza"))

usethis::use_data(gri_p.table, gri_k.table, so.table, ph.table,
                  calcare.tot, calcare.att, ragg_tes.tab,
                  fert_org.tab, concimi.tab, efficienza.tab,
                  overwrite = TRUE)
```

## 7. Priorità raccomandata

1. Quick win §5 (1 giorno, porta NFert perfettamente al passo con DPI 2026 sul lato N).
2. Fase A aggiornamento dati (porta dentro tutte le tabelle di riferimento 2026).
3. Fase D completa PK (vero salto di scope: da "solo N" a "N+P+K").
4. Fase E piano distribuzione (trasforma NFert da calcolatore di dose a strumento di pianificazione stagionale).
5. Fase F rilascio 0.2.0 e submission a CRAN.

## 8. Riferimenti

- **Fert_Office_v1._26.xlsm** – Regione Emilia-Romagna, Febbraio 2026 (contatto: Chiara Ferronato, chiara.ferronato@regione.emilia-romagna.it).
- **DPI Emilia-Romagna 2026** – Norme Generali, Allegato 2 (bilancio N), Allegato 9 (MAS), Guida alla Fertilizzazione.
- **RR n. 2/2024** – limiti MAS.
- **RR n. 3/2017** – utilizzazione agronomica effluenti e digestato.
- **Direttiva Nitrati 91/676/CEE** – 170 kg N/ha/anno in ZVN.
