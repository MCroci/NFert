#' Nitrogen balance
#'
#' @param expected_yield_tons_ha
#' @param crop
#' @param ccp
#' @param sand
#' @param clay
#' @param Ntot
#' @param SOM
#' @param CN
#' @param oxygen_availability
#' @param winter_rain
#' @param start_spring_rain
#' @param prev_crop
#' @param source
#' @param fertorg_frequency
#' @param location
#' @param forg_quantity
#'
#' @return
#' @export
#'
#' @examples
N_balance <- function(expected_yield_tons_ha = 15, crop = "Silage maize FAO 500", ccp = "Spring-summer crop 100–130 days",
                      sand = 50, clay = 35, Ntot = 1.2, SOM = 1.2, CN = 9.5, oxygen_availability = "Normal",
                      winter_rain = 160, start_spring_rain = 40,
                      prev_crop = "Winter cereals straw removal", source = "Cattle slurry", fertorg_frequency = "every year",
                      location = "Plain adjacent to urbanized areas", forg_quantity = 100){

  silt <- 100-(sand+clay)
  TRI2 <- tri2(clay=clay, sand=sand)
  TRI3 <- tri3(clay=clay, sand=sand)

  id_rag <- calc_soil_group_and_id_rag(clay=clay, sand=sand)$id_rag
  soil.group <- calc_soil_group_and_id_rag(clay=clay, sand=sand)$soil.group

  A <- calc_crop_N_demand(expected_yield_tons_ha = expected_yield_tons_ha, crop = crop)
  B <- soil_fertility(Ntot = Ntot, SOM = SOM, soil.group = soil.group, CN = CN, ccp = ccp)
  B$B <- sum(B$b1, B$b2)
  C <- leaching_loss(winter_rain = winter_rain, start_spring_rain = start_spring_rain,
                     oxygen_availability = oxygen_availability, id_rag = id_rag, b1 = B[["b1"]])
  D <- calc_N_immobilization_loss(B=B[["B"]], oxygen_availability = oxygen_availability, id_rag = id_rag)
  E <- nitrogen_from_previous_crop_residues(previous_crop =prev_crop )
  Forg <- organic_fertilization(source = source, frequency = fertorg_frequency, quantity = forg_quantity)
  G <- natural_contribution(location = location, ccp = ccp)

  Nfert <- data.frame(A = A, B = B, b1 = B[["b1"]], b2 = B[["b2"]], C1 = C[["C1"]], C2 = C[["C2"]], D = D, E = E, Forg = Forg, G = G)
  return(Nfert)
}
