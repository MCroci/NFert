#' Retrieve package objects and lazy-loaded datasets from `data/`.
#'
#' Lazy datasets are not always bound in `asNamespace("NFert")` when only the
#' namespace is loaded (e.g. `NFert::fun()` without `library(NFert)`). Fall back
#' to [utils::data()] so vignettes and `::` calls work.
#'
#' Kept in `00-nfert_data.R` so collation loads this before other sources.
#'
#' @keywords internal
nfert_data_get <- function(name) {
  ns <- asNamespace("NFert")
  if (exists(name, envir = ns, inherits = FALSE)) {
    return(get(name, envir = ns, inherits = FALSE))
  }
  if ("package:NFert" %in% search()) {
    pe <- as.environment("package:NFert")
    if (exists(name, envir = pe, inherits = FALSE)) {
      return(get(name, envir = pe, inherits = FALSE))
    }
  }
  e <- new.env(parent = emptyenv())
  utils::data(list = name, package = "NFert", envir = e)
  if (!exists(name, envir = e, inherits = FALSE)) {
    stop(
      sprintf("NFert object `%s` not found (code, LazyData, or data/).", name),
      call. = FALSE
    )
  }
  get(name, envir = e, inherits = FALSE)
}

#' Map legacy oxygen / drainage labels to `ca.table$availability` (Slow / Normal / Fast).
#'
#' @keywords internal
nfert_normalize_oxygen <- function(x) {
  if (length(x) != 1L) {
    stop("`oxygen_availability` must be length 1.", call. = FALSE)
  }
  o <- trimws(as.character(x))
  if (!nzchar(o)) {
    stop("`oxygen_availability` cannot be empty.", call. = FALSE)
  }
  switch(
    o,
    "Low" = "Slow",
    "High" = "Fast",
    "Reduced" = "Slow",
    "Poor" = "Slow",
    "Good" = "Fast",
    o
  )
}
