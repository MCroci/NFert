# N from previous years' organic fertilization (term F in DPI balance)

Residual N available to the crop from organic fertilizer applied in
previous years. DPI formula: F = N_applied_previous_year (kg/ha) x
Recovery_coeff. Recovery coefficient depends on organic type and
application frequency (Guida DPI 2026, sec. 3.1.6).

## Usage

``` r
organic_previous_years_N(N_applied_previous_year, source, frequency)
```

## Arguments

- N_applied_previous_year:

  Numeric. Total N (kg/ha) applied in the previous year from organic
  fertilization (same type and frequency as current practice).

- source:

  Character. Organic fertilizer source (e.g. "Cattle slurry", "Composted
  manure").

- frequency:

  Character. Application frequency: "every year", "every two years",
  "every three years", or "occasional".

## Value

Numeric. F in kg/ha (residual N from previous years' organic). Returns 0
if N_applied_previous_year is 0, NA, or NULL.

## References

DPI Emilia-Romagna, Guida alla fertilizzazione 2026, sec. 3.1.6 - N da
fertilizzazioni organiche degli anni precedenti.

## Examples

``` r
organic_previous_years_N(100, "Cattle slurry", "every year")   # 30 kg/ha
#> [1] 30
organic_previous_years_N(80, "Composted manure", "every two years")  # 24 kg/ha
#> [1] 24
```
