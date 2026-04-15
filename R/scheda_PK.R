#' Scheda a dose Standard - Phosphorus and Potassium (DPI 2026)
#'
#' Implements the DPI 2026 simplified "Scheda a dose Standard" method for P2O5
#' and K2O, matching Fert_Office v1.26 foglio `Scheda_PK`. The base dose for
#' phosphorus is selected from `standard_pk_doses.table` according to soil P
#' class; potassium base dose depends on soil K class. Then user-selected
#' decrements and increments are applied. No MAS cap exists for P/K in DPI
#' 2026 (the MAS applies to N only).
#'
#' @param crop Crop name (matches `standard_pk_doses.table$crop`).
#' @param phase Optional phase name.
#' @param soil_P_class One of `"Molto_Bassa"`, `"Bassa"`, `"Normale"` (default),
#'   `"Elevata"`. Selects the P2O5 base dose column.
#' @param soil_K_class Same levels for potassium.
#' @param P_decrements,P_increments,K_decrements,K_increments Named numeric
#'   vectors of adjustments (kg/ha). See `scheda_N()` for the same pattern.
#' @param standard_pk_doses.table Lookup (default `NFert::standard_pk_doses.table`).
#'
#' @return A named list with `dose_base_P2O5`, `dose_base_K2O`,
#'   `dose_final_P2O5`, `dose_final_K2O`, and subtotals.
#' @examples
#' scheda_PK(crop = "Grano duro (pianta intera)",
#'           soil_P_class = "Normale",
#'           soil_K_class = "Normale")
#' @export
scheda_PK <- function(crop,
                      phase = NULL,
                      soil_P_class = c("Normale","Bassa","Elevata","Molto_Bassa"),
                      soil_K_class = c("Normale","Bassa","Elevata","Molto_Bassa"),
                      P_decrements = numeric(), P_increments = numeric(),
                      K_decrements = numeric(), K_increments = numeric(),
                      standard_pk_doses.table = NFert::standard_pk_doses.table) {

  soil_P_class <- match.arg(soil_P_class)
  soil_K_class <- match.arg(soil_K_class)

  if (missing(crop) || !is.character(crop) || length(crop) != 1) {
    stop("`crop` must be a single crop name string.")
  }

  # Locate crop row
  t <- standard_pk_doses.table
  idx <- which(t$crop == crop)
  if (length(idx) == 0) {
    stop(sprintf("Crop '%s' not found in standard_pk_doses.table.", crop))
  }
  if (!is.null(phase)) {
    idx_p <- which(t$crop == crop & t$phase == phase)
    if (length(idx_p) > 0) idx <- idx_p
  }
  if (length(idx) > 1) idx <- idx[1]
  row <- t[idx, , drop = FALSE]

  P_col <- paste0("P2O5_dot_", soil_P_class)
  K_col <- paste0("K2O_dot_",  soil_K_class)
  dose_base_P <- suppressWarnings(as.numeric(row[[P_col]]))
  dose_base_K <- suppressWarnings(as.numeric(row[[K_col]]))

  sum_adjust <- function(v) {
    tot <- 0
    for (nm in names(v)) {
      x <- v[[nm]]
      if (is.null(x)) next
      if (is.logical(x)) { if (isTRUE(x)) tot <- tot + 0 }  # numeric values expected
      else if (is.numeric(x)) tot <- tot + abs(x)
    }
    tot
  }

  P_dec <- sum_adjust(P_decrements); P_inc <- sum_adjust(P_increments)
  K_dec <- sum_adjust(K_decrements); K_inc <- sum_adjust(K_increments)

  P_final <- max(0, (if (is.na(dose_base_P)) 0 else dose_base_P) + P_inc - P_dec)
  K_final <- max(0, (if (is.na(dose_base_K)) 0 else dose_base_K) + K_inc - K_dec)

  list(
    crop = crop,
    phase = row$phase[[1]],
    soil_P_class = soil_P_class, soil_K_class = soil_K_class,
    dose_base_P2O5 = dose_base_P,
    P_total_decrement = P_dec, P_total_increment = P_inc,
    dose_final_P2O5 = P_final,
    dose_base_K2O = dose_base_K,
    K_total_decrement = K_dec, K_total_increment = K_inc,
    dose_final_K2O = K_final,
    units = "kg/ha",
    reference = "DPI Emilia-Romagna 2026, Fert_Office v1.26 (Scheda_PK)"
  )
}
