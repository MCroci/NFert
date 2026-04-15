#' Leaching Loss of Nitrogen
#'
#' Calculates the nitrogen leaching loss from soil based on two methods:
#'
#' 1. Precipitation-Based Method (C1):
#'    - Considers winter rainfall (October 1 to January 31).
#'    - No loss if rainfall < 150 mm.
#'    - Progressive loss of readily available nitrogen (b1) if rainfall is 150-250 mm.
#'    - Complete loss of readily available nitrogen if rainfall > 250 mm.
#'
#' 2. Ease of Drainage Method (C2):
#'    - Estimates leaching based on soil drainage capacity (ID_Rag) and oxygen availability.
#'    - Uses lookup tables (`ca.table` and `cb.table`) for specific values.
#'
#' @param winter_rain Winter rainfall (October 1 to January 31) in mm.
#' @param start_spring_rain Rainfall in February in mm.
#' @param oxygen_availability Oxygen availability level in the soil (e.g., "Normal").
#' @param id_rag Soil drainage index (ID_Rag).
#' @param b1 Readily available nitrogen in the soil (kg/ha).
#' @param method Leaching loss calculation method ("precipitation" or "drainage"). Default is "precipitation".
#'
#' @return A list containing:
#'         - C1: Nitrogen leaching loss in the autumn-winter season (kg/ha).
#'         - C2: Nitrogen leaching loss after leaving winter (kg/ha).
#'         - surplus_pluviometrico: Logical. TRUE when winter_rain + start_spring_rain >= 300 mm
#'           (DPI 2026 scheda a dose standard: attiva l'incremento "Lisciviazione x surplus pluviometrico").
#' @export
#'
#' @examples
#' leaching_loss(winter_rain = 160, start_spring_rain = 40,
#'               oxygen_availability = "Normal", id_rag = 3, b1 = 29.16)

leaching_loss <- function(winter_rain = 160, start_spring_rain=40, oxygen_availability = "Normal", id_rag = 3, b1 = 29.16) {

  ca.table = NFert::ca.table
  cb.table = NFert::cb.table

  # C (kg/ha): Leaching loss
  # Ca: Nitrogen losses in the autumn winter season
  #- based on easiness of drainage:
  id_dre <- as.numeric( ca.table[ca.table[,"availability"] == oxygen_availability, "ID_Dre"] ) # extraction of id_dre based on description
  C1 <-as.numeric( cb.table[cb.table[,"ID_Rag"] == id_rag & cb.table[,"ID_Dre"] == id_dre , "C"] ) #

  #- based on rainfall:
  # Missing rain for complete loss
  missing_rain_for_complete_loss <- 250-winter_rain
  # b1: Available N potentially leachable (kg ha-1)
  percentage_loss <- ifelse((-150 + winter_rain)<0, 0, ifelse((-150 + winter_rain) > 100, 100, -150 + winter_rain))
  nitrogen_loss_with_winter_rain <-b1*(percentage_loss/100)

  # Nitrogen loss from late rain (kg ha-1)
  # remaining rain for febraury loss
  remaining_rain_for_febraury_loss <- start_spring_rain-missing_rain_for_complete_loss
  nitrogen_loss_with_late_rain <- ifelse(remaining_rain_for_febraury_loss>0, b1 - nitrogen_loss_with_winter_rain, 0)

  # total nitrogen (Azoto pronto) loss
  total_nitrogen_loss <- (nitrogen_loss_with_winter_rain+nitrogen_loss_with_late_rain)

  # Cb: Nitrogen losses after leaving winter
  # start_spring_rain
  # quantità di N lisciviabile (kg/ha)
  nitrogen_leachable_amount <- remaining_rain_for_febraury_loss/10
  febraury_loss <- ifelse(winter_rain>150 & nitrogen_leachable_amount>0, nitrogen_leachable_amount, 0)

  C2 <- nitrogen_loss_with_winter_rain+ febraury_loss

  # DPI 2026 scheda standard: condizione surplus pluviometrico
  # (pioggia 1/10 - 28/2 >= 300 mm) → attiva incremento "Lisciviazione x surplus pluviometrico".
  # Foglio C&D, righe 34-35.
  surplus_pluviometrico <- isTRUE((winter_rain + start_spring_rain) >= 300)

  res <- list(C1 = C1, C2 = C2, surplus_pluviometrico = surplus_pluviometrico)
  return(res)
}
