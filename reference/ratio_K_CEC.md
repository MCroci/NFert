# K/CSC ratio assessment (Fert_Office foglio Gri_K) \<0.1% basso; 0.1-2.1 ottimale; 2.1-5.1 alto; ... (percentage of CSC)

K/CSC ratio assessment (Fert_Office foglio Gri_K) \<0.1% basso; 0.1-2.1
ottimale; 2.1-5.1 alto; ... (percentage of CSC)

## Usage

``` r
ratio_K_CEC(k_meq_per_100g, csc_meq)
```

## Arguments

- k_meq_per_100g:

  K in meq/100g.

- csc_meq:

  CSC in meq/100g.

## Value

List with `pct` and `class`.

## Examples

``` r
ratio_K_CEC(k_meq_per_100g = 0.38, csc_meq = 18)
#> $pct
#> [1] 2.111111
#> 
#> $class
#> [1] "alto"
#> 
```
