#' Calculate soil fertility parameters (B = B1 + B2 in DPI).
#'
#' Estimates readily available and mineralized nitrogen in the soil. DPI naming:
#' b1 = readily available N from soil total N (DPI B2); b2 = mineralized N from
#' SOM (DPI B1). Total B = b1 + b2.
#'
#' @param Ntot Total nitrogen content in the soil (e.g. g/kg or ‰).
#' @param SOM Soil organic matter content (%).
#' @param soil.group Soil group (e.g., "Sandy textures").
#' @param CN Carbon-to-nitrogen ratio.
#' @param ccp Crop category and planting period (e.g., "Autumn-winter crop <150 days").
#' @param soil_seeding Seeding mode, one of `"traditional"` (default) or `"no-till"`.
#'   DPI 2026 applies a detrazione of 3 kg N/ha on b1 (readily available N, "N pronto")
#'   for semina su sodo, because tillage-driven mineralisation is absent. See
#'   foglio B, cella `Detrazione per semina su sodo = 3` in Fert_Office v1.26.
#' @param coefN_readily,coefN_mineralised,coef_time Lookup tables.
#'
#' @return A named list containing:
#'   - b1: Readily available nitrogen (kg/ha), DPI B2.
#'   - b2: Mineralized nitrogen from SOM (kg/ha), DPI B1.
#'   - units: The units of the output values ("kg/ha").
#'
#' @export
soil_fertility <- function(Ntot = 1.2, SOM = 2, soil.group = "Sandy textures",
                                   CN = 8, ccp = "Autumn-winter crop <150 days",
                                   soil_seeding = c("traditional", "no-till"),
                                   coefN_readily = NFert::coefN_readily,
                                   coefN_mineralised = NFert::coefN_mineralised,
                                   coef_time = NFert::coef_time
                                   ) {
  soil_seeding <- match.arg(soil_seeding)
  CN_class <- ifelse(CN<9, "<9", ifelse(CN>=9 & CN<12, "9-12", ">12") )

  coef.N.readily <- as.numeric( coefN_readily[coefN_readily[,"group"]==soil.group, "coef.N.readily"] )
  coef.N.mineralised <- as.numeric( coefN_mineralised[coefN_mineralised[,"group"]==soil.group & coefN_mineralised[,"CN_ratio"]==CN_class , "coef.N.mineralised"] )
  ctempo <- as.numeric( coef_time[coef_time[,"ccp"]==ccp, "C_tempo"] )

  # Nitrogen readily available from soil total nitrogen
  b1 <- Ntot * coef.N.readily

  # Mineralised nitrogen available (kg/ha) from total nitrogen
  b2 <- SOM * coef.N.mineralised * ctempo

  # DPI 2026: detrazione per semina su sodo (no-till) = -3 kg/ha su N pronto
  if (identical(soil_seeding, "no-till")) {
    b1 <- b1 - 3
  }

  B <- list(b1=b1, b2= b2, units="kg/ha")

    return(B)
}
