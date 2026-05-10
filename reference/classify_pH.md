# Classify soil pH

DPI 2026 7-class scheme from Fert_Office v1.26 foglio `pH`:

- fortemente acido: pH \< 5.4

- acido: 5.4 - 6.0

- leggermente acido: 6.1 - 6.7

- neutro: 6.8 - 7.3

- leggermente alcalino: 7.4 - 8.1

- alcalino: 8.2 - 8.6

- fortemente alcalino: \> 8.6

## Usage

``` r
classify_pH(pH, ph.table = nfert_data_get("ph.table"))
```

## Arguments

- pH:

  Numeric.

- ph.table:

  Lookup (default
  [`NFert::ph.table`](https://mcroci.github.io/NFert/reference/NFert-data.md)).

## Value

List with `ID_pH` and `class`.

## Examples

``` r
classify_pH(8.3)
#> $ID_pH
#> [1] 6
#> 
#> $class
#> [1] "alkaline"
#> 
#> $pH
#> [1] 8.3
#> 
```
