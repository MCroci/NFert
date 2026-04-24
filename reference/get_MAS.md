# Maximum Allowed Doses (MAS) per DPI Emilia-Romagna

Returns or checks the Maximum Allowed Doses (Massimali Ammessi dal DPI,
MAS) of nitrogen for a crop, as defined in the Disciplinari di
Produzione Integrata (DPI) Emilia-Romagna (Allegato 9, Reg. reg.
2/2024). In ZVN the values are binding; they can be exceeded if
production over 3 years exceeds the reference yield.

## Usage

``` r
get_MAS(crop = NULL, mas_table = NULL, edition = c("2026", "2025"))
```

## Arguments

- crop:

  Character. Crop name (as in DPI nomenclature). Use `get_MAS(NULL)` or
  `get_MAS()` to list all crops.

- mas_table:

  Data frame with columns `crop`, `mas_N`, `reference_yield`, `type`. If
  NULL, uses the built-in table for the chosen `edition`.

- edition:

  Character. `"2026"` (default) or `"2025"`. 2025 = Guida DPI 2025 (ZVN
  table); 2026 = FertDPI / Allegato 9 style.

## Value

For `get_MAS(crop)`: a one-row data frame with columns `crop`, `mas_N`,
`reference_yield` (and if present `mas_P2O5`, `yield_ref_min`,
`yield_ref_max`), `type`, or all rows if `crop` is NULL. For
`check_MAS`: a list with `ok` (logical), `mas_N`, `N_planned`,
`message`.

## References

DPI Emilia-Romagna - Norme Generali 2025, Allegato 9; Guida alla
Fertilizzazione Minerale e Organica 2025 (N, P, K). Reg. reg. 2/2024.
DPI 2026, FertDPI / Fert_Office_v1_26.

## Examples

``` r
get_MAS("Frumento tenero (granella)")
#>                         crop            crop_en mas_N mas_P2O5 yield_ref_min
#> 1 Frumento tenero (granella) Soft wheat (grain)   200      100             6
#>   yield_ref_max    type
#> 1             8 Erbacee
get_MAS("Frumento tenero (granella)", edition = "2025")  # ZVN 2025
#>                         crop mas_N reference_yield    type
#> 1 Frumento tenero (granella)   180             6.5 Erbacee
get_MAS()  # list all
#>                               crop
#> 1       Frumento tenero (granella)
#> 2       Grano tenero FF (granella)
#> 3  Frumento tenero (pianta intera)
#> 4            Grano duro (granella)
#> 5                 Mais da granella
#> 6                 Mais da insilato
#> 7          Shredded corn class 700
#> 8                             Orzo
#> 9                         Girasole
#> 10                            Soia
#> 11           Pomodoro da industria
#> 12                            Melo
#> 13                            Pero
#> 14               Pesco e Nettarine
#> 15              Vite (uva da vino)
#> 16                       Actinidia
#> 17                        Ciliegio
#>                                             crop_en mas_N mas_P2O5
#> 1                                Soft wheat (grain)   200      100
#> 2                    Soft wheat FF - strong (grain)   200      100
#> 3                          Soft wheat (whole plant)   200      100
#> 4                               Durum wheat (grain)   200      100
#> 5                       Grain maize 500-700 (grain)   260      150
#> 6                          Silage maize (class 700)   340      150
#> 7                          Silage maize (class 700)   340      150
#> 8                                    Barley (grain)   180       90
#> 9                               Sunflower (achenes)   160       90
#> 10                                  Soybean (grain)    NA      100
#> 11                 Processing tomato (medium yield)   200      130
#> 12                                            Apple   340      120
#> 13                                             Pear   340      120
#> 14                              Peach and Nectarine   340      175
#> 15       Vineyard (plain) - grapes, wood and leaves   120       60
#> 16 Kiwifruit (green flesh) - fruit, wood and leaves   340      150
#> 17                                           Cherry   340      120
#>    yield_ref_min yield_ref_max     type
#> 1              6             8  Erbacee
#> 2              6             8  Erbacee
#> 3              6             8  Erbacee
#> 4              5             6  Erbacee
#> 5             10            13  Erbacee
#> 6             40            50  Erbacee
#> 7             40            50  Erbacee
#> 8              5             7  Erbacee
#> 9              2             3  Erbacee
#> 10             3             4  Erbacee
#> 11            70           100 Orticole
#> 12            35            35  Arboree
#> 13            30            30  Arboree
#> 14            25            25  Arboree
#> 15             8            12  Arboree
#> 16            25            25  Arboree
#> 17             9             9  Arboree
check_MAS("Mais da granella", 250)
#> $ok
#> [1] TRUE
#> 
#> $mas_N
#> [1] 260
#> 
#> $N_planned
#> [1] 250
#> 
#> $message
#> [1] "Planned N is within MAS."
#> 
check_MAS("Frumento tenero (granella)", 210)  # over MAS
#> $ok
#> [1] FALSE
#> 
#> $mas_N
#> [1] 200
#> 
#> $N_planned
#> [1] 210
#> 
#> $message
#> [1] "Planned N (210 kg/ha) exceeds MAS (200 kg/ha)."
#> 
```
