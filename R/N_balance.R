#' Nitrogen Balance Calculation
#'
#' Calculates the nitrogen balance of a crop considering various soil and environmental factors.
#'
#' @param expected_yield_tons_ha Expected crop yield in tons per hectare.
#' @param crop Name of the crop.
#' @param ccp Crop calendar period (e.g., "Spring-summer crop 100–130 days").
#' @param sand Percentage of sand in the soil.
#' @param clay Percentage of clay in the soil.
#' @param Ntot Total nitrogen content in the soil (%).
#' @param SOM Soil organic matter content (%).
#' @param CN Carbon to nitrogen ratio of the soil.
#' @param oxygen_availability Oxygen availability level in the soil (e.g., "Normal").
#' @param winter_rain Total winter rainfall (mm).
#' @param start_spring_rain Rainfall at the start of spring (mm).
#' @param prev_crop Previous crop grown in the field.
#' @param source Source of organic fertilizer (e.g., "Cattle slurry").
#' @param fertorg_frequency Frequency of organic fertilizer application (e.g., "every year").
#' @param location Location of the field relative to urban areas.
#' @param forg_quantity Quantity of organic fertilizer applied (kg/ha).
#'
#' @return A data frame containing the calculated nitrogen balance components:
#'         \itemize{
#'           \item A: Total nitrogen demand of the crop.
#'           \item B: Soil nitrogen supply.
#'           \item b1: Mineralizable nitrogen from soil organic matter.
#'           \item b2: Readily available mineral nitrogen in the soil.
#'           \item C1: Nitrogen leaching loss in winter.
#'           \item C2: Nitrogen leaching loss in spring.
#'           \item D: Nitrogen immobilization loss.
#'           \item E: Nitrogen from previous crop residues.
#'           \item Forg: Nitrogen from organic fertilizer.
#'           \item G: Natural nitrogen contribution (e.g., from rainfall).
#'         }
#' @export
#'
#' @examples
#' N_balance(expected_yield_tons_ha = 15, crop = "Mais trinciato (classe 700)",
#'           ccp = "Spring-summer crop 100–130 days", sand = 50, clay = 35,
#'           Ntot = 1.2, SOM = 1.2, CN = 9.5, oxygen_availability = "Normal",
#'           winter_rain = 160, start_spring_rain = 40,
#'           prev_crop = "Winter cereals straw removal", source = "Cattle slurry",
#'           fertorg_frequency = "every year", location = "Plain adjacent to urbanized areas",
#'           forg_quantity = 100)

N_balance <- function(expected_yield_tons_ha = 15, crop = "Grano tenero FF (granella)", ccp = "Spring-summer crop 100–130 days",
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
