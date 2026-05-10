# Retrieve package objects and lazy-loaded datasets from `data/`.

Lazy datasets are not always bound in `asNamespace("NFert")` when only
the namespace is loaded (e.g. `NFert::fun()` without
[`library(NFert)`](https://github.com/mcroci/NFert)). Fall back to
[`utils::data()`](https://rdrr.io/r/utils/data.html) so vignettes and
`::` calls work.

## Usage

``` r
nfert_data_get(name)
```

## Details

Kept in `00-nfert_data.R` so collation loads this before other sources.
