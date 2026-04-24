# Mg/K ratio assessment (Fert_Office foglio Gri_K) 0-0.1 basso; 0.1-2.1 equilibrato; 2.1-5.1 alto; \>5.1 molto alto.

Mg/K ratio assessment (Fert_Office foglio Gri_K) 0-0.1 basso; 0.1-2.1
equilibrato; 2.1-5.1 alto; \>5.1 molto alto.

## Usage

``` r
ratio_Mg_K(mg_ppm, k_ppm)
```

## Arguments

- mg_ppm, k_ppm:

  Magnesium and potassium ppm.

## Value

List with `ratio` and `class`.

## Examples

``` r
ratio_Mg_K(mg_ppm = 121.5, k_ppm = 150)
#> $ratio
#> [1] 2.606667
#> 
#> $class
#> [1] "alto"
#> 
```
