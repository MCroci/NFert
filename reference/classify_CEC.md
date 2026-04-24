# Classify cation exchange capacity (CSC, meq/100 g) From Fert_Office v1.26 foglio Gri_K: \< 10 = bassa, 10-20 = media, \> 20 = alta.

Classify cation exchange capacity (CSC, meq/100 g) From Fert_Office
v1.26 foglio Gri_K: \< 10 = bassa, 10-20 = media, \> 20 = alta.

## Usage

``` r
classify_CEC(csc_meq)
```

## Arguments

- csc_meq:

  Numeric meq/100 g.

## Value

List with `class`.

## Examples

``` r
classify_CEC(18)
#> $class
#> [1] "media"
#> 
#> $csc
#> [1] 18
#> 
```
