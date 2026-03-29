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
#' @param organic_previous_year_N Optional. Total N (kg/ha) from organic fertilization
#'   applied in the previous year (same source/frequency). If provided, term F (residual
#'   N from previous years' organic, DPI §3.1.6) is included. Default 0.
#' @param soil_group Optional. Soil group (e.g. from \code{calc_soil_group_and_id_rag()})
#'   to use DPI efficiency for current-year organic. If set, \code{distribution_efficiency}
#'   should also be set.
#' @param distribution_efficiency Optional. "efficient", "medium", or "low" (DPI distribution).
#'   Used with \code{soil_group} for DPI organic N efficiency.
#'
#' @return A data frame containing the calculated nitrogen balance components (all in kg/ha):
#'         \itemize{
#'           \item A: Total nitrogen demand of the crop (kg/ha).
#'           \item B: Total soil nitrogen supply (sum of b1 and b2) (kg/ha).
#'           \item b1: Readily available mineral nitrogen (DPI B2) (kg/ha).
#'           \item b2: Mineralizable N from soil organic matter (DPI B1) (kg/ha).
#'           \item C1: Nitrogen leaching loss in winter (kg/ha).
#'           \item C2: Nitrogen leaching loss in spring (kg/ha).
#'           \item D: Nitrogen immobilization loss (kg/ha).
#'           \item E: Nitrogen from previous crop residues (kg/ha).
#'           \item F: Nitrogen from previous years' organic fertilization (kg/ha).
#'           \item Forg: Nitrogen from organic fertilizer (current year, efficient N) (kg/ha).
#'           \item G: Natural nitrogen contribution (e.g., from rainfall) (kg/ha).
#'         }
#'         
#' @details The nitrogen fertilization requirement (DPI formula) is:
#'          N_fert = A - B + C1 + C2 + D - E - F - Forg - G
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
                      location = "Plain adjacent to urbanized areas", forg_quantity = 100,
                      organic_previous_year_N = 0, soil_group = NULL, distribution_efficiency = NULL){

  # Comprehensive input validation
  if (!is.numeric(expected_yield_tons_ha) || expected_yield_tons_ha <= 0) {
    stop("Expected yield must be a positive numeric value.")
  }
  if (!is.numeric(sand) || sand < 0 || sand > 100) {
    stop("Sand percentage must be a numeric value between 0 and 100.")
  }
  if (!is.numeric(clay) || clay < 0 || clay > 100) {
    stop("Clay percentage must be a numeric value between 0 and 100.")
  }
  if (sand + clay > 100) {
    stop("The sum of sand and clay percentages cannot exceed 100%.")
  }
  if (!is.numeric(Ntot) || Ntot < 0) {
    stop("Total nitrogen (Ntot) must be a non-negative numeric value.")
  }
  if (!is.numeric(SOM) || SOM < 0) {
    stop("Soil organic matter (SOM) must be a non-negative numeric value.")
  }
  if (!is.numeric(CN) || CN <= 0) {
    stop("Carbon to nitrogen ratio (CN) must be a positive numeric value.")
  }
  if (!is.numeric(winter_rain) || winter_rain < 0) {
    stop("Winter rainfall must be a non-negative numeric value.")
  }
  if (!is.numeric(start_spring_rain) || start_spring_rain < 0) {
    stop("Spring rainfall must be a non-negative numeric value.")
  }
  if (!is.numeric(forg_quantity) || forg_quantity < 0) {
    stop("Organic fertilizer quantity must be a non-negative numeric value.")
  }
  if (!is.null(organic_previous_year_N) && (!is.numeric(organic_previous_year_N) || organic_previous_year_N < 0)) {
    stop("organic_previous_year_N must be NULL or a non-negative number.")
  }

  # Calculate soil properties (single call for efficiency)
  soil_props <- calc_soil_group_and_id_rag(clay = clay, sand = sand)
  id_rag <- soil_props$id_rag
  soil.group <- soil_props$soil.group

  # Calculate crop nitrogen demand (returns list, extract numeric value)
  A_result <- calc_crop_N_demand(expected_yield_tons_ha = expected_yield_tons_ha, crop = crop)
  A <- A_result$N_requirement

  # Calculate soil fertility (returns list). Note: b1 = readily available (DPI B2), b2 = mineralized from SOM (DPI B1)
  B <- soil_fertility(Ntot = Ntot, SOM = SOM, soil.group = soil.group, CN = CN, ccp = ccp)
  B_total <- sum(B$b1, B$b2)

  # Calculate leaching losses
  C <- leaching_loss(winter_rain = winter_rain, start_spring_rain = start_spring_rain,
                     oxygen_availability = oxygen_availability, id_rag = id_rag, b1 = B[["b1"]])

  # Calculate other components
  D <- calc_N_immobilization_loss(B = B_total, oxygen_availability = oxygen_availability, id_rag = id_rag)
  E <- nitrogen_from_previous_crop_residues(previous_crop = prev_crop)
  F_prev <- organic_previous_years_N(
    N_applied_previous_year = if (is.null(organic_previous_year_N)) 0 else organic_previous_year_N,
    source = source,
    frequency = fertorg_frequency
  )
  Forg <- organic_fertilization(
    source = source,
    frequency = fertorg_frequency,
    quantity = forg_quantity,
    soil_group = soil_group,
    distribution_efficiency = distribution_efficiency
  )
  G <- natural_contribution(location = location, ccp = ccp)

  # Check for NA values and warn user
  na_components <- character()
  if (is.na(A)) na_components <- c(na_components, "A (crop demand)")
  if (is.na(B_total)) na_components <- c(na_components, "B (soil supply)")
  if (is.na(C[["C1"]])) na_components <- c(na_components, "C1 (winter leaching)")
  if (is.na(C[["C2"]])) na_components <- c(na_components, "C2 (spring leaching)")
  if (is.na(D)) na_components <- c(na_components, "D (immobilization)")
  if (is.na(E)) na_components <- c(na_components, "E (previous crop residues)")
  if (is.na(Forg)) na_components <- c(na_components, "Forg (organic fertilizer)")
  if (is.na(G)) na_components <- c(na_components, "G (natural contribution)")
  
  if (length(na_components) > 0) {
    warning(paste("Some balance components returned NA:",
                  paste(na_components, collapse = ", "),
                  ". These may indicate missing data or invalid input values."))
  }

  # Build result data frame (DPI: N = A - B + C + D - E - F - Forg - G)
  Nfert <- data.frame(
    A = A,
    B = B_total,
    b1 = B[["b1"]],
    b2 = B[["b2"]],
    C1 = C[["C1"]],
    C2 = C[["C2"]],
    D = D,
    E = E,
    F = F_prev,
    Forg = Forg,
    G = G
  )

  return(Nfert)
}
