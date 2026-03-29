---
title: "Guida alla Fertilizzazione Integrata – N, P, K"
subtitle: "Colture erbacee e arboree – DPI Emilia-Romagna 2026"
author: "FertDPI / Fert_Office_v1_26"
date: "2026"
description: "Fertilizzazione minerale, organica e tutte le equazioni del metodo del bilancio"
---

# Guida alla Fertilizzazione Integrata

> **Azoto – Fosforo – Potassio** per colture erbacee e arboree
>
> Fertilizzazione minerale, organica e tutte le equazioni del metodo del bilancio
>
> *Riferimento: DPI Emilia-Romagna 2026 | Strumento: FertDPI / Fert_Office_v1_26*

## 1. Introduzione e quadro normativo

Il piano di fertilizzazione è un documento obbligatorio per le aziende aderenti ai Disciplinari di Produzione Integrata (DPI) della Regione Emilia-Romagna. Razionalizza gli apporti di N, P e K in funzione delle reali esigenze delle colture, delle caratteristiche del suolo e dei fertilizzanti impiegati. L'applicativo FertDPI (Fert_Office_v1_26, edizione 2026) implementa automaticamente i calcoli dell'Allegato 2 alle Norme Generali.

### 1.1 Scadenze del piano di fertilizzazione

|                              |                      |                            |
|------------------------------|----------------------|----------------------------|
| **Tipo di coltura**          | **Predisposizione**  | **Versione definitiva**    |
| Colture erbacee e foraggere  | entro il 28 febbraio | 45 gg prima della raccolta |
| Colture orticole             | entro il 15 aprile   | 15 gg prima della raccolta |
| Colture arboree e sementiere | entro il 15 aprile   | entro il 15 settembre      |

> **⚠ Nota:** Ogni intervento di fertilizzazione deve essere registrato entro 7 giorni nelle schede di campo DPI 2025 (colture erbacee o arboree), aggiornando anche il registro di magazzino fertilizzanti (carico/scarico).

## 2. Analisi del suolo

L'analisi del suolo (validità 5 anni) è la base del piano. In alternativa alle analisi di laboratorio sono utilizzabili le analisi geostatistiche del Catalogo dei Suoli ER, con validità permanente. I dati del suolo determinano: la classe di tessitura (e quindi i coefficienti di efficienza degli effluenti e i fattori di immobilizzazione di P e K), il livello di S.O. e N totale (per calcolare B1 e B2), e il livello di P e K (per calcolare B1 e arricchimenti/riduzioni).

### 2.1 Raggruppamenti di tessitura e parametri derivati

|                           |        |                   |                       |                          |
|---------------------------|--------|-------------------|-----------------------|--------------------------|
| **Raggruppamento**        | **ID** | **Classi USDA**   | **Peso spec. (t/m³)** | **Fattore immob. P (a)** |
| Tendenzialmente sabbioso  | 1      | S, SF, FS         | 1.4                   | 1.4                      |
| Franco / Medio impasto    | 2      | L, F, FL, FSA, FA | 1.3                   | 1.3                      |
| Tendenzialmente argilloso | 3      | FLA, AS, AL, A    | 1.2                   | 1.2                      |

> Il fattore 'a' di immobilizzazione del fosforo (1.4 / 1.3 / 1.2) è quello usato nella formula C = a + 0,02 × calcare totale (%) per calcolare il fattore C della concimazione fosfatica.

## 3. Tutte le equazioni del metodo del Bilancio (Allegato 2 DPI)

Questa sezione riporta in forma esplicita tutte le equazioni implementate in FertDPI per il calcolo del piano di fertilizzazione con il metodo del bilancio. I fogli nascosti dell'applicativo (B, C&D, E, F, G, Gri_P, Gri_K, C_tempo, Efficienza) contengono i parametri numerici di riferimento.

### 3.1 Equazione generale dell'Azoto

**Formula principale N (kg/ha):**

> **N = A − B + C + D − E − F − G**
>
> *dove A = fabbisogno colturale; B = fertilità suolo; C = perdite lisciviazione; D = immobilizzazione; E = N da precessione; F = N da organici anni prec.; G = apporti naturali*

#### 3.1.1 A – Fabbisogno colturale

**Colture erbacee:**

> **A_N = Coeff_asportazione_N (kg/t) × resa_media (t/ha)**
>
> *Esempio: grano duro, coeff. N = 31,1 kg/t, resa 6 t/ha → A_N = 186,6 kg/ha*

**Colture arboree – formula generale A (frutti + legno potato + foglie):**

> **A_arboree = Coeff_asportazione_N (kg/t) × produzione_media (t/ha)**
>
> *La produzione è la parte asportata annualmente dal campo (frutti + legno + foglie). Coefficienti estratti dal foglio 'A' di FertDPI \| Produzione = media aziendale 3 anni o dato ISTAT*

Tabella dei coefficienti di asportazione unitari per le principali colture arboree (foglio 'A' di FertDPI):

|                                        |                     |                        |                       |                       |                          |
|----------------------------------------|---------------------|------------------------|-----------------------|-----------------------|--------------------------|
| **Coltura (parte asportata)**          | **Coeff. N (kg/t)** | **Coeff. P₂O₅ (kg/t)** | **Coeff. K₂O (kg/t)** | **Prod. rif. (t/ha)** | **A_N standard (kg/ha)** |
| Actinidia polpa verde (fr+leg+fg)      | 0.59                | 0.16                   | 0.59                  | 25                    | 147                      |
| Albicocco media prod. (fr+leg+fg)      | 0.55                | 0.13                   | 0.53                  | 13                    | 71                       |
| Ciliegio (fr+leg+fg)                   | 0.67                | 0.22                   | 0.59                  | 9                     | 60                       |
| Melo (fr+leg+fg)                       | 0.29                | 0.08                   | 0.31                  | 40                    | 116                      |
| Mirtillo                               | 0.14                | 0.07                   | 0.10                  | 17.5                  | 24                       |
| Nettarine (fr+leg+fg)                  | 0.64                | 0.14                   | 0.53                  | 25                    | 160                      |
| Nocciolo (fr+leg+fg)                   | 3.15                | 1.35                   | 2.90                  | —                     | —                        |
| Noce da frutto (fr+leg+fg)             | 3.20                | 1.00                   | 1.30                  | 4                     | 128                      |
| Pero media prod. (fr+leg+fg)           | 0.33                | 0.08                   | 0.33                  | 30                    | 99                       |
| Pesco (fr+leg+fg)                      | 0.58                | 0.17                   | 0.58                  | 25                    | 145                      |
| Susino (fr+leg+fg)                     | 0.49                | 0.10                   | 0.49                  | 25                    | 122                      |
| Uva da tavola (grappoli+tralci+fg)     | 0.51                | 0.06                   | 0.48                  | 25                    | 127                      |
| Vite da vino pianura (grappoli+leg+fg) | 0.62                | 0.28                   | 0.74                  | 20                    | 124                      |
| Vite da vino collina (grappoli+tr+fg)  | 0.57                | 0.26                   | 0.67                  | 10                    | 57                       |

> **⚠ Nota:** I coefficienti unitari (kg/t) rappresentano l'elemento (N, P₂O₅ o K₂O) asportato per tonnellata di prodotto totale allontanato dal campo. Per colture pluriennali in fase di impianto, il fabbisogno A si calcola sulla resa media della fase di piena produzione prevista.

#### 3.1.2 B – Fertilità del suolo (mineralizzazione)

La voce B comprende due componenti: B1 (N da mineralizzazione della S.O.) e B2 (N prontamente disponibile dall'azoto totale del suolo). Entrambe vengono moltiplicate per il coefficiente C_tempo, che tiene conto della durata e della fase del ciclo colturale.

**Componente B1 – Mineralizzazione della S.O.:**

> **B1 = S.O. (t/ha) × Coeff_mineralizzazione × C_tempo**
>
> *S.O. (t/ha) = S.O.(%) / 100 × peso_suolo_20cm (t/ha) Peso suolo 20 cm: sabbiosi = 2.800 t/ha; franchi = 2.600 t/ha; argillosi = 2.400 t/ha Coeff_mineralizzazione dipende da raggruppamento tessitura e rapporto C/N*

**Componente B2 – N minerale prontamente disponibile:**

> **B2 = N_tot (t/ha) × Coeff_N_pronto × C_tempo**
>
> *N_tot (t/ha) = N_tot(g/kg) / 1000 × peso_suolo_20cm (t/ha) B2 viene calcolata solo per colture annuali; per pluriennali si usa solo B1*

**B totale disponibile alla coltura:**

> **B_totale = B1 + B2**
>
> *Per colture pluriennali: B = B1 (B2 non viene conteggiata per la stabilità del sistema)*

Tabella dei coefficienti moltiplicatori C_tempo per fase/ciclo colturale (foglio C_tempo di FertDPI):

|                               |             |           |                                               |
|-------------------------------|-------------|-----------|-----------------------------------------------|
| **Fase colturale / ciclo**    | **C_tempo** | **Tipo**  | **Significato**                               |
| Pre-impianto                  | 0.20        | Arboree   | Solo 20% della B1 dell'anno viene conteggiata |
| I anno allevamento            | 1.00        | Arboree   | 100% – ma apporti N limitati al 40%           |
| II anno allevamento           | 1.00        | Arboree   | 100% – apporti N limitati al 50%              |
| Piena produzione              | 1.00        | Arboree   | 100% senza limitazioni di fase                |
| Primaverile-estiva \< 70 gg   | 0.30        | Erbacee   | 30% della mineralizzazione annua              |
| Primaverile-estiva 70–100 gg  | 0.50        | Erbacee   | 50% della mineralizzazione annua              |
| Primaverile-estiva 100–130 gg | 0.67        | Erbacee   | 67% della mineralizzazione annua              |
| Primaverile-estiva \> 130 gg  | 0.75        | Erbacee   | 75% della mineralizzazione annua              |
| Autunno-vernina \< 150 gg     | 0.50        | Erbacee   | 50% della mineralizzazione annua              |
| Autunno-vernina \> 150 gg     | 0.60        | Erbacee   | 60% della mineralizzazione annua              |
| 1° taglio (foraggere)         | 0.11        | Foraggere | 11% – ciclo molto breve                       |
| 2° taglio e successivi        | 0.10        | Foraggere | 10% per taglio                                |

**B1_effettivo = B1_tabella × C_tempo (equazione esplicita):**

> **B1_effettivo = \[S.O.(t/ha) × Coeff_min\] × C_tempo**
>
> *Esempio: S.O. 2%, terreno franco → S.O.(t/ha) = 52 t/ha; Nm = 52 × 2,0 (coeff. medio) = 104 → B1_eff = 104 × 0,67 = ~70 kg/ha (ciclo 100–130 gg)*

#### 3.1.3 C – Perdite per lisciviazione (Ca e Cb)

Le perdite per lisciviazione sono suddivise in Ca (perdite nel periodo autunno-invernale) e Cb (perdite alla fine dell'inverno, a febbraio). Si calcolano solo per colture autunno-vernine su suoli con rischio di dilavamento.

**Ca – Perdite N pronto nel periodo autunno-invernale:**

> **Ca = N_pronto × (piogge_periodo_autinv − 150) / 100**
>
> *dove N_pronto = N ammoniacale disponibile (kg/ha); soglia = 150 mm; 100 mm = quota piogge per perdita completa N_pronto massimo lisciviabile per texture e drenaggio (tabella):*

|                        |                          |                        |                     |
|------------------------|--------------------------|------------------------|---------------------|
| **Tessitura**          | **Drenaggio imperfetto** | **Drenaggio moderato** | **Drenaggio buono** |
| Sabbiosi               | 30 kg/ha                 | 40 kg/ha               | 50 kg/ha            |
| Franco – Medio impasto | 20 kg/ha                 | 30 kg/ha               | 40 kg/ha            |
| Argillosi e limosi     | 10 kg/ha                 | 20 kg/ha               | 30 kg/ha            |

**Cb – Perdite N all'uscita dall'inverno (febbraio):**

> **Cb = (piogge_ottgen + piogge_feb − 250) / 10**
>
> *Condizione attivante: piogge_ottgen \> 150 mm E (piogge_ottgen + piogge_feb) \> 250 mm Se la condizione non è soddisfatta → Cb = 0 Il risultato (kg/ha) si aggiunge a Ca per ottenere il C totale*

**Logica condizionale esplicita per Cb:**

> **SE (piogge_ottgen + piogge_feb \> 250 mm) ALLORA: perdita_Cb = (piogge_ottgen + piogge_feb − 250) / 10 ALTRIMENTI: Cb = 0**
>
> *piogge_ottgen = precipitazioni da ottobre a gennaio; piogge_feb = precipitazioni di febbraio Soglia complessiva: 250 mm \| Per ogni 10 mm oltre la soglia si perde ~1 kg/ha di N*

**C totale:**

> **C = Ca + Cb**
>
> *Entrambe sono zero se le precipitazioni sono inferiori alle soglie o per colture estive*

#### 3.1.4 D – Immobilizzazione e dispersione

Il fattore D rappresenta le perdite di N per immobilizzazione microbica e per volatilizzazione/denitrificazione. Il coefficiente fc (fattore di dispersione) dipende da tessitura e disponibilità di ossigeno (drenaggio).

**Colture annuali (B2 presente):**

> **D = (B1 + B2) × fc**
>
> *fc include sia l'immobilizzazione che la dispersione nel corso del ciclo*

**Colture pluriennali (solo B1):**

> **D = B1 × fc**
>
> *B2 non è conteggiata, quindi anche D si basa solo sulla quota da mineralizzazione della S.O.*

Tabella dei valori di fc (fattore di immobilizzazione e dispersione) estratti dal foglio C&D di FertDPI:

|                                |                          |                        |                     |
|--------------------------------|--------------------------|------------------------|---------------------|
| **Tessitura (raggruppamento)** | **Drenaggio imperfetto** | **Drenaggio moderato** | **Drenaggio buono** |
| Sabbiosi (ID 1)                | **0.35**                 | **0.20**               | **0.15**            |
| Franco – Medio impasto (ID 2)  | **0.40**                 | **0.25**               | **0.20**            |
| Argillosi e limosi (ID 3)      | **0.45**                 | **0.30**               | **0.25**            |

#### 3.1.5 E – N da precessione colturale

Il valore E rappresenta l'N residuo rilasciato (o immobilizzato, se negativo) dalla coltura precedente. I valori sono tabellari e forniti dal foglio E di FertDPI.

**Calcolo E:**

> **E = valore_tabellare (kg/ha) per la coltura in precessione**
>
> *Valore positivo → rilascio N (beneficio); valore negativo → immobilizzazione N (onere aggiuntivo)*

Tabella dei valori di E per coltura in precessione (foglio E di FertDPI):

|                                              |                       |                |                                    |
|----------------------------------------------|-----------------------|----------------|------------------------------------|
| **Coltura in precessione**                   | **N residuo (kg/ha)** | **Leguminosa** | **Note**                           |
| Barbabietola                                 | **+30**               | No             | Radici ricche in N                 |
| Cereale autunnale – paglia asportata         | **−10**               | No             | N immobilizzato dalla paglia       |
| Cereale autunnale – paglia interrata         | **−30**               | No             | Forte immobilizzazione N           |
| Colza                                        | **+20**               | No             | Residui foglie e stocchi           |
| Girasole                                     | **0**                 | No             | Effetto neutro                     |
| Mais – stocchi asportati                     | **−10**               | No             | Leggera immobilizzazione           |
| Mais – stocchi interrati                     | **−40**               | No             | Forte immobilizzazione N           |
| Medica in buone condizioni                   | **+80**               | Sì             | Leguminosa perenne prolifica       |
| Medica diradato                              | **+60**               | Sì             | Leguminosa parzialmente produttiva |
| Prato polifita leg. \> 15%                   | **+60**               | Sì             | —                                  |
| Prato polifita leg. 5–15%                    | **+40**               | Sì             | —                                  |
| Prato polifita leg. \< 5%                    | **+15**               | No             | —                                  |
| Prato di breve durata / trifoglio            | **+30**               | Sì             | —                                  |
| Soia                                         | **+10**               | Sì             | Fissazione limitata nel residuo    |
| Sovescio di leguminose                       | **+50**               | Sì             | —                                  |
| Leguminosa da granella (pisello, fagiolo...) | **+40**               | Sì             | —                                  |
| Orticole a foglia                            | **+25**               | No             | Residui fogliari                   |
| Pomodoro e altre orticole                    | **+30**               | No             | —                                  |
| Sorgo                                        | **−40**               | No             | Forte immobilizzazione N           |
| Non definita                                 | **0**                 | —              | —                                  |

#### 3.1.6 F – N da fertilizzazioni organiche degli anni precedenti

**Calcolo F:**

> **F = N_apportato_anno_prec (kg/ha) × Coeff_recupero**
>
> *Il coefficiente di recupero dipende dal tipo di organico e dalla frequenza di distribuzione*

|                                     |                    |                 |                 |               |
|-------------------------------------|--------------------|-----------------|-----------------|---------------|
| **Tipo di organico**                | **Tutti gli anni** | **Ogni 2 anni** | **Ogni 3 anni** | **Saltuaria** |
| Ammendante (compost, letame maturo) | 0.50               | 0.30            | 0.20            | 0.20          |
| Liquame bovino / digestato          | 0.30               | 0.15            | 0.10            | 0.00          |
| Liquame suino e/o pollina           | 0.15               | 0.10            | 0.05            | 0.00          |
| Nessuno                             | 0.00               | 0.00            | 0.00            | 0.00          |

#### 3.1.7 G – Apporti naturali (deposizione atmosferica)

**Calcolo G:**

> **G = deposizione_annua × C_tempo**
>
> *Pianura vicino zone urbane: 20 kg/ha/anno \| Pianura isolata: 15 kg/ha/anno \| Collina/montagna: 10 kg/ha/anno Il valore viene modulato dal C_tempo della coltura*

### 3.2 Equazioni per il Fosforo (P₂O₅)

**Formula generale P₂O₅ (kg/ha):**

> **P = A ± (B × C) + B1_arr + A2**
>
> *A = fabbisogno colturale; B = scarto di P dalla dotazione normale; C = fattore immobilizzazione; B1_arr = arricchimento se dotazione scarsa; A2 = anticipazioni anni futuri (colture pluriennali)*

**Fabbisogno colturale P (A):**

> **A_P = Coeff_asportazione_P2O5 (kg/t) × resa (t/ha)**
>
> *Esempio: grano duro, coeff. P₂O₅ = 10,6 kg/t, resa 6 t/ha → A_P = 63,6 kg/ha*

**Fattore di immobilizzazione C per il fosforo:**

> **C = a + (0,02 × calcare_totale\_%)**
>
> *dove a = 1,4 (sabbiosi) \| 1,3 (franchi) \| 1,2 (argillosi) Esempio: terreno franco con calcare 15% → C = 1,3 + (0,02 × 15) = 1,60*

**Arricchimento/Riduzione P (B1_arr o B2_rid):**

> **Arricchimento o Riduzione = 3 × Da × Q_P**
>
> *Da = peso suolo 30 cm (t/ha); Da = 4.200 (sabb.) \| 3.900 (franco) \| 3.600 (arg.) Q_P = scarto in ppm di P₂O₅ dai limiti dell'intervallo di normalità × conversione Fattore 3 = anni di validità dell'arricchimento/riduzione*
>
> L'intervallo di normalità del fosforo (come P₂O₅) è 22,9–68,7 ppm per tutti i terreni. Se la dotazione è scarsa (\<22,9 ppm) si aggiunge un arricchimento; se molto alta (\>68,7 ppm) si applica una riduzione. FertDPI calcola automaticamente Q_P nel foglio 'Gri_P'.

### 3.3 Equazioni per il Potassio (K₂O)

**Formula generale K₂O (kg/ha):**

> **K = E_K ± (F_K × G_K) + H + B1_arr_K + A2_K**
>
> *E_K = fabbisogno colturale K; F_K = scarto dalla dotazione normale; G_K = fattore immobilizzazione K; H = lisciviazione K; B1_arr_K = arricchimento K; A2_K = anticipazioni*

**Fabbisogno colturale K (E_K):**

> **E_K = Coeff_asportazione_K2O (kg/t) × resa (t/ha)**
>
> *Esempio: grano duro, coeff. K₂O = 19,9 kg/t, resa 6 t/ha → E_K = 119,4 kg/ha*

**Fattore di immobilizzazione G per il potassio:**

> **G_K = 1 + (0,018 × argilla\_%)**
>
> *Esempio: terreno con 24% argilla → G_K = 1 + (0,018 × 24) = 1,43 Il G dipende dal contenuto di argilla in quanto il K è trattenuto dai minerali argillosi*

**Arricchimento/Riduzione K:**

> **Arricchimento o Riduzione K = 3 × Da × Q_K**
>
> *Da (peso suolo 30 cm): 4.200 (sabb.) \| 3.900 (franco) \| 3.600 (arg.) in t/ha Q_K = scarto in ppm di K₂O dai limiti dell'intervallo di normalità × fattore conversione FertDPI calcola automaticamente Q_K nel foglio 'Gri_K'*
>
> L'intervallo di normalità del K varia per tessitura: sabbiosi 96–144 ppm K₂O; franchi 120–180 ppm; argillosi 144–216 ppm. Se la dotazione è inferiore al minimo si aggiunge arricchimento; se superiore al massimo si applica riduzione.

## 4. Criteri di fertilizzazione per colture erbacee e arboree

### 4.1 Colture erbacee

#### 4.1.1 Azoto

- Colture primaverili-estive: apporto in prossimità della semina o in copertura frazionata.

- Colture autunno-vernine: max 30 kg/ha N in presemina; quota principale in copertura da febbraio in poi.

- Frazionamento obbligatorio oltre 100 kg/ha N da sintesi a pronto effetto (min. 2 interventi, ≥ 7 giorni).

- Concimi a lenta cessione: esenti dal frazionamento; anticipazione consentita a metà gennaio.

#### 4.1.2 Fosforo e Potassio

- Colture non sarchiate (cereali): P e K durante la lavorazione del terreno. Eccezione: P posticipabile alla semina se localizzato.

- Fertirrigazione: nessuna limitazione sull'epoca di distribuzione di P e K.

- Semina su sodo: P e K non devono necessariamente essere interrati.

- Colture orticole: P e K anche in copertura; fortemente consigliato durante la preparazione del terreno.

### 4.2 Colture arboree

#### 4.2.1 Azoto

- Frazionamento obbligatorio oltre 60 kg/ha N da sintesi a pronto effetto (≥ 7 giorni tra interventi).

- Fase di allevamento 1° e 2° anno: solo apporti localizzati (quantitativi massimi nelle schede tecniche).

- NEW 2026 – Post raccolta: consentite distribuzioni autunnali \< 40 kg/ha N entro il 15 ottobre.

#### 4.2.2 Fosforo e Potassio

- Pre-impianto: solo ammendanti organici; non ammessi concimi minerali.

- Concimazione di arricchimento in pre-impianto: max 250 kg/ha P₂O₅ e 300 kg/ha K₂O.

- Lavorazioni lungo la fila per interrare fertilizzanti ammesse anche con inerbimento dell'interfila.

### 4.3 Massimali (MAS)

|                                |                   |                      |                      |          |
|--------------------------------|-------------------|----------------------|----------------------|----------|
| **Coltura (esempi)**           | **MAS N (kg/ha)** | **MAS P₂O₅ (kg/ha)** | **Resa rif. (t/ha)** | **Tipo** |
| Frumento tenero (granella)     | 200               | 100                  | 6–8                  | Erbacee  |
| Grano duro (granella)          | 200               | 100                  | 5–6                  | Erbacee  |
| Mais da granella               | 260               | 150                  | 10–13                | Erbacee  |
| Mais da insilato               | 340               | 150                  | 40–50                | Erbacee  |
| Orzo                           | 180               | 90                   | 5–7                  | Erbacee  |
| Girasole                       | 160               | 90                   | 2–3                  | Erbacee  |
| Soia                           | — \*              | 100                  | 3–4                  | Erbacee  |
| Pomodoro da industria          | 200               | 130                  | 70–100               | Orticole |
| Melo (frutti + legno + foglie) | 340               | 120                  | 35                   | Arboree  |
| Pero media produzione          | 340               | 120                  | 30                   | Arboree  |
| Pesco/Nettarine                | 340               | 175                  | 25                   | Arboree  |
| Vite (uva da vino)             | 120               | 60                   | 8–12                 | Arboree  |
| Actinidia polpa verde          | 340               | 150                  | 25                   | Arboree  |
| Ciliegio                       | 340               | 120                  | 9                    | Arboree  |

> **⚠ Nota:** (\*) Soia: N da sintesi in genere nullo grazie alla fissazione simbiotica. Nelle ZVN: limite 170 kg/ha/anno N da effluenti zootecnici come media aziendale (non per singola coltura).

## 5. Fertilizzazione organica – tipologie e composizione

|                              |          |                 |               |              |              |                   |                   |
|------------------------------|----------|-----------------|---------------|--------------|--------------|-------------------|-------------------|
| **Fertilizzante organico**   | **ss %** | **N tot. kg/t** | **P₂O₅ kg/t** | **K₂O kg/t** | **MgO kg/t** | **Tipo cessione** | **Categoria**     |
| Liquame suino                | 1.5–6    | 1.5–5           | 1.1–3         | 1.5–4        | —            | Rapida            | Refluo zootecnico |
| Liquame bovino da carne      | 7–10     | 3.2–4.5         | 2.3–3         | 4.5–7        | —            | Rapida            | Refluo zootecnico |
| Liquame bovino da latte      | 10–16    | 3.9–6.3         | 2.3–3         | 5–8          | —            | Rapida            | Refluo zootecnico |
| Liquame ovaiole              | 19–25    | 10–15           | 9.2–13        | 8–12         | —            | Rapida            | Refluo zootecnico |
| Letame bovino                | 20–30    | 3–7             | 0.9–2         | 4–7          | —            | Graduale          | Refluo zootecnico |
| Letame suino                 | ~25      | ~4.7            | ~4.1          | ~3.5         | —            | Graduale          | Refluo zootecnico |
| Lettiera polli da carne      | 60–80    | 30–47           | 29–45         | 20–35        | —            | Graduale          | Refluo avicolo    |
| Pollina preessiccata         | 50–85    | 23–43           | 21–35         | 15–30        | —            | Rapida/Graduale   | Refluo avicolo    |
| Digestato t.q. (bovini)      | 20–24    | 3–9             | 4–6           | 3–6          | —            | Rapida            | Digestato         |
| Digestato fraz. chiarificata | 2–6      | 6–10            | 3–5           | 4–7          | —            | Molto rapida      | Digestato         |
| Digestato fraz. palabile     | 30–40    | 2–8             | 5–8           | 3–6          | —            | Graduale          | Digestato         |
| Ammendante compostato misto  | 60–70    | 16–20           | 10–15         | 10–14        | —            | Molto graduale    | Ammendante        |
| Fanghi agroalimentari        | 20–30    | 1.4–4.2         | 1.25–3        | 0.5–2        | —            | Graduale          | Fanghi            |

### 5.1 Regole generali

- Letame, compost, reflui, fanghi e digestato: nessun vincolo specifico di epoca o frazionamento DPI (rispettare Reg. 3/2017).

- ZVN: limite 170 kg/ha/anno N da effluenti zootecnici come media aziendale.

- Ammendanti compostati con fanghi civili: NON ammessi nei DPI.

- Incorporazione nel terreno: obbligatoria entro 24 ore su terreno nudo; esclusi gli appezzamenti con copertura vegetale.

- Apporti massimi annui di ammendanti: 15 t ss/ha (S.O. scarsa), 13 t ss/ha (S.O. normale), 9 t ss/ha (S.O. elevata).

## 6. Efficienza dell'azoto organico – equazioni e tabelle

### 6.1 Formula per il calcolo dell'N disponibile

**N utile alla coltura da un singolo fertilizzante organico:**

> **N_disponibile = N_tot_apportato (kg/ha) × Efficienza% / 100**
>
> *N_tot_apportato (kg/ha) = N_tot (kg/t) × quantità_distribuita (t/ha) Esempio: 100 t/ha liquame bovino, N_tot = 3,85 kg/t → N_tot = 385 kg/ha; con eff. 60,4% (franco/alta) → N_disp = 233 kg/ha*

### 6.2 Efficienza aziendale media ponderata (più effluenti)

Quando in azienda si utilizzano più tipologie di effluente con efficienze diverse, FertDPI calcola automaticamente l'N totale disponibile sommando i contributi individuali. In generale la formula della media ponderata è:

**Efficienza media aziendale ponderata:**

> **Eff_media = Σ(Ni × ei) / Σ(Ni)**
>
> *dove Ni = kg N totale apportati dall'i-esimo fertilizzante; ei = efficienza% dell'i-esimo fertilizzante Esempio: 200 kg N da liquame bovino (eff. 60%) + 100 kg N da pollina (eff. 81%) = Eff_media = (200×60 + 100×81) / (200+100) = (12000+8100)/300 = 67%*

### 6.3 Livelli di efficienza e tecniche di distribuzione

|             |                           |                                                                                        |                            |
|-------------|---------------------------|----------------------------------------------------------------------------------------|----------------------------|
| **Livello** | **Qualità distribuzione** | **Esempi di tecniche**                                                                 | **Perdite NH₃ indicative** |
| 3 – ALTA    | Efficiente                | Iniezione nel suolo (6–30 cm), fertirrigazione, interramento \< 4 ore dal'applicazione | \< 10–15%                  |
| 2 – MEDIA   | Media                     | Spandimento rasoterra a bande, solco aperto (6–8 cm), interramento nelle 24 ore        | 15–40%                     |
| 1 – BASSA   | Poco efficiente           | Spandimento superficiale con piatto deviatore/getto senza incorporazione               | fino a 80%                 |

> **⚠ Nota:** Per gli ammendanti organici (compost, letame maturo, digestato palabile) il calcolo dell'N utile è fisso al 40% indipendentemente dalla tecnica, per la prevalenza di N organico a rilascio lento.

### 6.4 Tabelle di efficienza per tessitura

#### 6.4.1 Terreno sabbioso

|                                                |                                        |                                           |                                       |                   |
|------------------------------------------------|----------------------------------------|-------------------------------------------|---------------------------------------|-------------------|
| **Tipo di effluente / fertilizzante organico** | **Tecnica efficiente**                 | **Tecnica media**                         | **Tecnica poco efficiente**           | **Ammendanti**    |
| (iniezione, fertirrig., interramento \< 4 ore) | rasoterra + interramento non immediato | spandimento in bande senza incorporazione | spandimento sup. senza incorporazione | quota utile fissa |
| Liquame avicolo                                | **90.9%**                              | 65.6%                                     | 40.3%                                 | —                 |
| Liquame bovino                                 | **67.1%**                              | 48.4%                                     | 29.8%                                 | —                 |
| Liquame suino                                  | **79.0%**                              | 57.0%                                     | 35.0%                                 | —                 |
| Digestato t.q. (bovini)                        | **67.1%**                              | 48.4%                                     | 29.8%                                 | —                 |
| Digestato t.q. (suini)                         | **79.0%**                              | 57.0%                                     | 35.0%                                 | —                 |
| Digestato t.q. (avicoli)                       | **90.9%**                              | 65.6%                                     | 40.3%                                 | —                 |
| Digestato fraz. chiarificata                   | **90.9%**                              | 65.6%                                     | 40.3%                                 | —                 |
| Fanghi agroalimentari                          | **67.1%**                              | 48.4%                                     | 29.8%                                 | —                 |
| Ammendante / compost                           | **—**                                  | —                                         | —                                     | 40%               |

#### 6.4.2 Terreno franco / medio impasto

|                                                |                                        |                                           |                                       |                   |
|------------------------------------------------|----------------------------------------|-------------------------------------------|---------------------------------------|-------------------|
| **Tipo di effluente / fertilizzante organico** | **Tecnica efficiente**                 | **Tecnica media**                         | **Tecnica poco efficiente**           | **Ammendanti**    |
| (iniezione, fertirrig., interramento \< 4 ore) | rasoterra + interramento non immediato | spandimento in bande senza incorporazione | spandimento sup. senza incorporazione | quota utile fissa |
| Liquame avicolo                                | **81.6%**                              | 59.8%                                     | 37.9%                                 | —                 |
| Liquame bovino                                 | **60.4%**                              | 44.2%                                     | 28.1%                                 | —                 |
| Liquame suino                                  | **71.0%**                              | 52.0%                                     | 33.0%                                 | —                 |
| Digestato t.q. (bovini)                        | **60.4%**                              | 44.2%                                     | 28.1%                                 | —                 |
| Digestato t.q. (suini)                         | **71.0%**                              | 52.0%                                     | 33.0%                                 | —                 |
| Digestato t.q. (avicoli)                       | **81.6%**                              | 59.8%                                     | 37.9%                                 | —                 |
| Digestato fraz. chiarificata                   | **81.6%**                              | 59.8%                                     | 37.9%                                 | —                 |
| Fanghi agroalimentari                          | **60.4%**                              | 44.2%                                     | 28.1%                                 | —                 |
| Ammendante / compost                           | **—**                                  | —                                         | —                                     | 40%               |

#### 6.4.3 Terreno argilloso

|                                                |                                        |                                           |                                       |                   |
|------------------------------------------------|----------------------------------------|-------------------------------------------|---------------------------------------|-------------------|
| **Tipo di effluente / fertilizzante organico** | **Tecnica efficiente**                 | **Tecnica media**                         | **Tecnica poco efficiente**           | **Ammendanti**    |
| (iniezione, fertirrig., interramento \< 4 ore) | rasoterra + interramento non immediato | spandimento in bande senza incorporazione | spandimento sup. senza incorporazione | quota utile fissa |
| Liquame avicolo                                | **72.4%**                              | 54.0%                                     | 35.6%                                 | —                 |
| Liquame bovino                                 | **53.6%**                              | 40.1%                                     | 26.5%                                 | —                 |
| Liquame suino                                  | **63.0%**                              | 47.0%                                     | 31.0%                                 | —                 |
| Digestato t.q. (bovini)                        | **53.6%**                              | 40.1%                                     | 26.5%                                 | —                 |
| Digestato t.q. (suini)                         | **63.0%**                              | 47.0%                                     | 31.0%                                 | —                 |
| Digestato t.q. (avicoli)                       | **72.4%**                              | 54.0%                                     | 35.6%                                 | —                 |
| Digestato fraz. chiarificata                   | **72.4%**                              | 54.0%                                     | 35.6%                                 | —                 |
| Fanghi agroalimentari                          | **53.6%**                              | 40.1%                                     | 26.5%                                 | —                 |
| Ammendante / compost                           | **—**                                  | —                                         | —                                     | 40%               |

## 7. Metodo della Scheda a Dose Standard (Allegato 3)

Il metodo semplificato parte da una dose base di N, P₂O₅ e K₂O per coltura e la corregge con incrementi/decrementi in base a fattori aziendali. FertDPI mostra sempre il confronto con il metodo bilancio.

|                                               |                  |                                                          |
|-----------------------------------------------|------------------|----------------------------------------------------------|
| **Fattore di aggiustamento**                  | **Effetto su N** | **Note**                                                 |
| Resa bassa (\< soglia di riferimento)         | **Decremento**   | Riduce la dose base                                      |
| Resa alta (\> soglia di riferimento)          | **Incremento**   | Aumenta la dose base                                     |
| Tenore di S.O. nel suolo elevato              | **Decremento**   | Maggiore mineralizzazione                                |
| Tenore di S.O. nel suolo basso                | **Incremento**   | Minore disponibilità dal suolo                           |
| Ammendante distribuito nell'anno precedente   | **Decremento**   | N rilasciato per mineralizzazione residua                |
| Precessione di leguminose                     | **Decremento**   | Azoto fissato biologicamente                             |
| Precessione dopo medica/prato \> 5 anni       | **Decremento**   | Elevato rilascio di azoto dal suolo                      |
| Ristoppio o interramento paglie/stocchi       | **Incremento**   | N immobilizzato dalla flora batterica                    |
| Surplus pluviometrico / rischio lisciviazione | **Incremento**   | Perdite attese per dilavamento (piogge ott–feb ≥ 300 mm) |
| Terreno compattato / semina su sodo           | **Incremento**   | Minore efficienza di assorbimento radicale               |

> **⚠ Nota:** Per alcune colture da seme è previsto esclusivamente il metodo a scheda standard. Verificare le norme tecniche di coltura specifiche. I risultati delle schede standard per N sono confrontati con il bilancio nel foglio Scheda_N.

## 8. Pianificazione della distribuzione – foglio 'Distribuz'

- Sezione organici: selezionare fino a 3 fertilizzanti organici con titoli N/P/K/ss%, quantità, epoca e modalità. L'N efficiente viene calcolato automaticamente.

- Sezione minerali/sintesi: titoli, quantità (q/ha) ed epoca per fino a 8 concimi.

- Totali automatici: confronto con dosi calcolate; alert 'Eccesso' se superamento (non segnala MAS né limiti ZVN).

- Integrazione organico–minerale: calcolare prima l'N/P/K utile dagli organici, poi integrare con minerali solo la quota residua rispettando i MAS.

> Il foglio 'Registra_Piano' genera la scheda ufficiale del piano da conservare in azienda. Riporta i massimali, gli apporti pianificati, la verifica OK/Eccesso e le dotazioni finali di P e K da riportare nel piano dell'anno successivo.

## 9. Checklist operativa per la predisposizione del piano

|       |                                                                                                  |                              |
|-------|--------------------------------------------------------------------------------------------------|------------------------------|
| **✓** | **Operazione**                                                                                   | **Foglio FertDPI**           |
| ☐     | Verificare la validità dell'analisi del suolo (max 5 anni)                                       | *Inserimento*                |
| ☐     | Inserire dati aziendali: nome, area omogenea, appezzamento, anno, ZVN                            | *Inserimento*                |
| ☐     | Selezionare coltura, fase/ciclo e resa media attesa                                              | *Inserimento*                |
| ☐     | Inserire dati del suolo: tessitura, pH, S.O., calcare, P, K (e CSC, Mg se disponibili)           | *Inserimento*                |
| ☐     | Indicare pratiche agronomiche: precessione, irriguo, semina su sodo, apporti organici anni prec. | *Inserimento*                |
| ☐     | Inserire precipitazioni autunno-invernali (per calcolo Ca e Cb)                                  | *Inserimento*                |
| ☐     | Scegliere il metodo di calcolo: Bilancio o Scheda Standard                                       | *Bilancio / Scheda_N/PK*     |
| ☐     | Verificare fabbisogni N, P₂O₅, K₂O e confrontarli con i MAS                                      | *Bilancio o Schede*          |
| ☐     | Verificare il coefficiente C_tempo applicato (coerente con fase e ciclo)                         | *C_tempo (nascosto)*         |
| ☐     | Verificare fattore Da immobilizzazione P e G immobilizzazione K                                  | *Gri_P / Gri_K (nascosti)*   |
| ☐     | Selezionare fertilizzanti organici con titoli e modalità di distribuzione                        | *Distribuz*                  |
| ☐     | Controllare efficienza N per tipo di effluente, tessitura e tecnica                              | *Efficienza / Distribuz*     |
| ☐     | Pianificare integrazione con concimi minerali per quota residua N, P, K                          | *Distribuz*                  |
| ☐     | Verificare che nessun elemento sia in Eccesso rispetto al calcolato e al MAS                     | *Distribuz / Registra_Piano* |
| ☐     | Calcolare l'N_disponibile aziendale ponderato se si usano più effluenti                          | *Distribuz*                  |
| ☐     | Salvare/stampare la scheda Registra_Piano come versione ufficiale                                | *Registra_Piano*             |
| ☐     | Annotare dotazioni finali di P e K per il piano dell'anno successivo                             | *Registra_Piano*             |
| ☐     | Registrare ogni intervento di fertilizzazione entro 7 giorni nelle schede di campo               | *Schede DPI 2025*            |
| ☐     | Aggiornare il registro di magazzino dei fertilizzanti (carico/scarico)                           | *Scheda magazzino DPI 2025*  |

*Documento redatto sulla base di: DPI ER 2026 – Norme Generali (Allegato 2) \| FertDPI Fert_Office_v1_26 \| Reg. reg. ER 3/2017 \| Direttiva Nitrati 91/676/CEE*

---

*Documento generato sulla base di: DPI ER 2026 – Norme Generali (Allegato 2) | FertDPI Fert_Office_v1_26 | Reg. reg. ER 3/2017 | Direttiva Nitrati 91/676/CEE*
