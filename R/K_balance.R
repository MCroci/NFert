#' Potassium balance (DPI Emilia-Romagna 2026 metodo del bilancio)
#'
#' Implements the K2O balance of Fert_Office v1.26 foglio `Bilancio` / `Gri_K`.
#'
#' Formula:
#' \preformatted{
#'   K2O da apportare = A (asportazione) + H (lisciviazione clay-based) + B1 (arricchimento)
#'                      + A2 (advance_allowed) - B2 (riduzione)
#' }
#' with A set to 0 in "Riduzione" strategy.
#'
#' @param expected_yield_tons_ha Expected yield (t/ha).
#' @param crop Crop name (matches `uptake_table$crop`).
#' @param k_value Analytical K (or K2O) ppm.
#' @param k_unit `"K"` or `"K2O"`, default `"K"`.
#' @param soil_group DPI texture group; if `NULL` and `clay`/`sand` given, derived.
#' @param clay,sand Soil clay/sand percentages (%).
#' @param advance_K2O Advance anni futuri (default 0).
#' @param include_leaching Logical, use clay-based H leaching (default TRUE).
#'
#' @return A data frame with A, H, B1, A2, B2, strategy, ID_Gri_K, K2O_required.
#' @examples
#' K_balance(expected_yield_tons_ha = 6,
#'           crop = "Grano duro (pianta intera)",
#'           k_value = 150, k_unit = "K2O",
#'           clay = 18.5, sand = 15.5)
#' @export
K_balance <- function(expected_yield_tons_ha,
                      crop,
                      k_value,
                      k_unit = c("K", "K2O"),
                      soil_group = NULL,
                      clay = NULL, sand = NULL,
                      advance_K2O = 0,
                      include_leaching = TRUE) {
  k_unit <- match.arg(k_unit)

  if (is.null(soil_group)) {
    if (is.null(clay) || is.null(sand)) {
      stop("Provide either `soil_group` or both `clay` and `sand`.")
    }
    sp <- calc_soil_group_and_id_rag(clay = clay, sand = sand)
    soil_group <- normalise_soil_group(sp$soil.group)$en
  } else {
    soil_group <- normalise_soil_group(soil_group)$en
  }
  if (is.null(clay)) clay <- 0  # needed for leaching default

  A <- calc_crop_K_demand(expected_yield_tons_ha, crop)$K2O_requirement
  if (is.na(A)) {
    return(data.frame(A = NA_real_, H = NA_real_, B1 = NA_real_,
                      A2 = advance_K2O, B2 = NA_real_,
                      strategy = NA_character_, ID_Gri_K = NA_integer_,
                      K2O_required = NA_real_))
  }

  avail <- soil_K_availability(k_value = k_value, unit = k_unit,
                               soil_group = soil_group,
                               A_demand_K2O = A,
                               clay_pct = clay,
                               include_leaching = include_leaching)

  # A_mantenimento already includes H leaching
  K2O_required <- max(0,
                      avail$A_mantenimento + avail$B1 + advance_K2O - avail$B2)

  data.frame(
    A  = if (!is.na(avail$strategy) && avail$strategy == "Riduzione") 0 else A,
    A_fabbisogno = A,
    H  = avail$H,
    B1 = avail$B1,
    A2 = advance_K2O,
    B2 = avail$B2,
    strategy = avail$strategy,
    ID_Gri_K = avail$ID_Gri_K,
    K2O_required = K2O_required
  )
}
