#' Phosphorus balance (DPI Emilia-Romagna 2026 metodo del bilancio)
#'
#' Implements the P2O5 balance of Fert_Office v1.26 foglio `Bilancio`.
#'
#' Formula:
#' \preformatted{
#'   P2O5 da apportare = A (asportazione) + B1 (arricchimento) + A2 (anticipazioni)
#'                        - B2 (riduzione)
#' }
#' with A set to 0 in "Riduzione" strategy and B2 used only as placeholder.
#'
#' @param expected_yield_tons_ha Expected yield (t/ha).
#' @param crop Crop name (matches `uptake_table$crop`).
#' @param olsen_value Analytical Olsen P (or P2O5) in ppm.
#' @param olsen_unit `"P"` or `"P2O5"`, default `"P"`.
#' @param soil_group DPI texture group; if `NULL` and `clay`/`sand` given, derived
#'   via `calc_soil_group_and_id_rag()` and mapped to Italian Ragg.
#' @param clay,sand Soil clay/sand percentages (%).
#' @param anticipazioni_P2O5 Anticipazioni anni futuri (default 0 kg/ha).
#' @param depth_cm Soil depth for enrichment calculation (default 30 cm).
#'
#' @return A data frame with A, B1, A2, B2, `strategy`, `ID_Gri_P`, `P2O5_required`.
#' @examples
#' P_balance(expected_yield_tons_ha = 6,
#'           crop = "Grano duro (pianta intera)",
#'           olsen_value = 15, olsen_unit = "P2O5",
#'           clay = 18.5, sand = 15.5)
#' @export
P_balance <- function(expected_yield_tons_ha,
                      crop,
                      olsen_value,
                      olsen_unit = c("P", "P2O5"),
                      soil_group = NULL,
                      clay = NULL, sand = NULL,
                      anticipazioni_P2O5 = 0,
                      depth_cm = 30) {
  olsen_unit <- match.arg(olsen_unit)

  if (is.null(soil_group)) {
    if (is.null(clay) || is.null(sand)) {
      stop("Provide either `soil_group` or both `clay` and `sand`.")
    }
    sp <- calc_soil_group_and_id_rag(clay = clay, sand = sand)
    soil_group <- normalise_soil_group(sp$soil.group)$it_plural
  } else {
    # Accept any naming convention; canonicalise
    soil_group <- normalise_soil_group(soil_group)$it_plural
  }

  A <- calc_crop_P_demand(expected_yield_tons_ha, crop)$P2O5_requirement
  if (is.na(A)) {
    return(data.frame(A = NA_real_, B1 = NA_real_, A2 = anticipazioni_P2O5,
                      B2 = NA_real_, strategy = NA_character_,
                      ID_Gri_P = NA_integer_, P2O5_required = NA_real_))
  }

  avail <- soil_P_availability(olsen_value = olsen_value, unit = olsen_unit,
                               soil_group = soil_group,
                               A_demand_P2O5 = A,
                               depth_cm = depth_cm)

  P2O5_required <- max(0,
                       avail$A_mantenimento + avail$B1 + anticipazioni_P2O5 - avail$B2)

  data.frame(
    A  = avail$A_mantenimento,   # asportazione effettiva (0 se Riduzione)
    A_fabbisogno = A,
    B1 = avail$B1,
    A2 = anticipazioni_P2O5,
    B2 = avail$B2,
    strategy = avail$strategy,
    ID_Gri_P = avail$ID_Gri_P,
    soil_weight_t_ha = avail$soil_weight_t_ha,
    P2O5_required = P2O5_required
  )
}
