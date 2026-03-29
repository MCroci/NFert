#' N from previous years' organic fertilization (term F in DPI balance)
#'
#' Residual N available to the crop from organic fertilizer applied in previous
#' years. DPI formula: F = N_applied_previous_year (kg/ha) x Recovery_coeff.
#' Recovery coefficient depends on organic type and application frequency
#' (Guida DPI 2026, §3.1.6).
#'
#' @param N_applied_previous_year Numeric. Total N (kg/ha) applied in the previous year
#'   from organic fertilization (same type and frequency as current practice).
#' @param source Character. Organic fertilizer source (e.g. "Cattle slurry", "Composted manure").
#' @param frequency Character. Application frequency: "every year", "every two years",
#'   "every three years", or "occasional".
#'
#' @return Numeric. F in kg/ha (residual N from previous years' organic).
#'   Returns 0 if N_applied_previous_year is 0, NA, or NULL.
#'
#' @references
#' DPI Emilia-Romagna, Guida alla fertilizzazione 2026, §3.1.6 – N da fertilizzazioni
#' organiche degli anni precedenti.
#'
#' @export
#' @examples
#' organic_previous_years_N(100, "Cattle slurry", "every year")   # 30 kg/ha
#' organic_previous_years_N(80, "Composted manure", "every two years")  # 24 kg/ha
organic_previous_years_N <- function(N_applied_previous_year, source, frequency) {
  if (is.null(N_applied_previous_year) || is.na(N_applied_previous_year) || N_applied_previous_year <= 0)
    return(0)
  freq <- .normalize_organic_frequency(frequency)
  cat <- .organic_source_to_F_category(source)
  tab <- .organic_recovery_table()
  coeff <- tab[tab$organic_type == cat & tab$frequency == freq, "recovery"]
  if (length(coeff) == 0 || is.na(coeff[1L])) return(0)
  N_applied_previous_year * coeff[1L]
}

#' Map source to DPI F category (ammendante, cattle_slurry, pig_poultry, none)
#' @noRd
.organic_source_to_F_category <- function(source) {
  s <- tolower(as.character(source))
  if (grepl("compost|letame maturo|ammendante|palabile", s)) return("ammendante")
  if (grepl("cattle|bovino|liquame bovino|digestato.*bovin", s)) return("cattle_slurry")
  if (grepl("pig|suino|suini|pollina|avicolo|poultry", s)) return("pig_poultry")
  "cattle_slurry"  # default for unknown
}

#' Normalize frequency to table levels
#' @noRd
.normalize_organic_frequency <- function(frequency) {
  f <- tolower(as.character(frequency))
  if (f %in% c("every year", "ogni anno", "annual")) return("every year")
  if (f %in% c("every two years", "every 2 years", "ogni due anni")) return("every two years")
  if (f %in% c("every three years", "every 3 years", "ogni tre anni")) return("every three years")
  if (f %in% c("occasional", "saltuaria", "saltuario")) return("occasional")
  "every year"
}

#' DPI recovery coefficients: organic_type x frequency -> recovery (0-1)
#' Guida §3.1.6 – Ammendante 50/30/20/20; Liquame bovino 30/15/10/0; Suino/pollina 15/10/5/0
#' @noRd
.organic_recovery_table <- function() {
  data.frame(
    organic_type = rep(c("ammendante", "cattle_slurry", "pig_poultry", "none"), each = 4L),
    frequency = rep(c("every year", "every two years", "every three years", "occasional"), 4L),
    recovery = c(
      0.50, 0.30, 0.20, 0.20,
      0.30, 0.15, 0.10, 0.00,
      0.15, 0.10, 0.05, 0.00,
      0.00, 0.00, 0.00, 0.00
    ),
    stringsAsFactors = FALSE
  )
}
