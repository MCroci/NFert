#' Leaching loss nitrogen
#'
#' Precipitation-based method (c1)
#' Where rainfall is concentrated in the autumn-winter period, in general,that portion of nitrogen that enters the budget as "ready N" should be considered leachable. Whereas in situations with significant rainfall surplus even during the spring-summer period and with soils with low water retention, a fraction of the nitrogen from fertilization and that from S.O. mineralization should be considered leachable in addition to ready nitrogen. Leaching losses in the autumn-winter period are estimated by taking as a reference
#' the amount of precipitation in the interval from October 1 to January 31 as follows below:
#'  - with rainfall <150 mm: no losses:
#'  - with rainfall between 150 and 250 mm: progressively increasing loss of ready nitrogen;
#'  - with rainfall >250 mm: all ready nitrogen is lost.
#' To calculate the % of ready N that is considered to be leached as a function of rainfall, the following expression is used:
#'  x = (y - 150)
#' where: x>0 = percentage of ready nitrogen lost; y = rainfall in mm during October to January.
#'
#' Ease of drainage method (c2)
#' The calculation of nitrogen losses in soil by leaching based on drainage and texture can be estimated by adopting the following scheme.
#' @param winter_rain Rain in the winter time dal 1/10 al 31/01
#'
#' @param start_spring_rain Rain in febraury
#' @param oxygen_availability
#' @param id_rag
#' @param b1
#' @param method
#'
#' @return
#' @export
#'
#' @examples
leaching_loss <- function(winter_rain = 160, start_spring_rain=40, oxygen_availability = "Normal", id_rag = 3, b1 = 29.16) {

  ca.table = NFert::ca.table
  cb.table = NFert::cb.table

  # C (kg/ha): Leaching loss
  # Ca: Nitrogen losses in the autumn winter season
  #      - based on easiness of drainage:
  id_dre <- as.numeric( ca.table[ca.table[,"availability"] == oxygen_availability, "ID_Dre"] ) # extraction of id_dre based on description
  C1 <-  as.numeric( cb.table[cb.table[,"ID_Rag"] == id_rag & cb.table[,"ID_Dre"] == id_dre , "C"] ) #

  #      - based on rainfall:
  # Missing rain for complete loss
  missing_rain_for_complete_loss <- 250-winter_rain
  # b1: Available N potentially leachable (kg ha-1)
  percentage_loss <- ifelse((-150 + winter_rain)<0, 0, ifelse((-150 + winter_rain) > 100, 100, -150 + winter_rain))
  nitrogen_loss_with_winter_rain <-  b1*(percentage_loss/100)

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

  C2 <- nitrogen_loss_with_winter_rain  + febraury_loss

  res <- list(C1 = C1, C2 = C2 )
  return(res)
    }
