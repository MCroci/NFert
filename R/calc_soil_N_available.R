#' Calculate soil fertility parameters.
#'
#' This function estimates the amount of readily available and mineralized nitrogen in the soil.
#'
#' @param Ntot Total nitrogen content in the soil (kg/ha).
#' @param SOM Soil organic matter content (%).
#' @param soil.group Soil group (e.g., "Sandy textures").
#' @param CN Carbon-to-nitrogen ratio.
#' @param ccp Crop category and planting period (e.g., "Autumn-winter crop <150 days").
#'
#' @return A named vector containing:
#'   - b1: N_readily_available: Readily available nitrogen (kg/ha).
#'   - b2: N_mineralized: Mineralized nitrogen (kg/ha).
#'   - units: The units of the output values ("kg/ha").
#'
#' @export
soil_fertility <- function(Ntot = 1.2, SOM = 2, soil.group = "Sandy textures",
                                   CN = 8, ccp = "Autumn-winter crop <150 days",
                                   coefN_readily = NFert::coefN_readily,
                                   coefN_mineralised = NFert::coefN_mineralised,
                                   coef_time = NFert::coef_time
                                   ) {
  CN_class <- ifelse(CN<9, "<9", ifelse(CN>=9 & CN<12, "9-12", ">12") )

  coef.N.readily <- as.numeric( coefN_readily[coefN_readily[,"group"]==soil.group, "coef.N.readily"] )
  coef.N.mineralised <- as.numeric( coefN_mineralised[coefN_mineralised[,"group"]==soil.group & coefN_mineralised[,"CN_ratio"]==CN_class , "coef.N.mineralised"] )
  ctempo <- as.numeric( coef_time[coef_time[,"ccp"]==ccp, "C_tempo"] )

  # Nitrogen readily available from soil total nitrogen
  b1 <- Ntot * coef.N.readily

  # Mineralised nitrogen available (kg/ha) from total nitrogen
  b2 <- SOM * coef.N.mineralised * ctempo

  B <- list(b1=b1, b2= b2, units="kg/ha")

    return(B)
}
