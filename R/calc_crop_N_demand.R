#' Crop requirement estimation
#'
#' Estimates the crop's nitrogen requirement based on unit crop absorption and expected yield.
#'
#' The calculation is done as follows:
#' A = unit crop absorption (kg N/t yield) * expected production (t/ha)
#'
#' @param expected_yield_tons_ha Expected crop yield in tons per hectare (t/ha).
#' @param uptake_table A data frame containing unit crop absorptions for different crops.
#'   It should have at least two columns: "crop" (character) and "N" (numeric).
#' @param crop The name of the crop for which to calculate the requirement. Must be a valid
#'   value in the `crop` column of the `uptake_table`.
#'
#' @return A list containing:
#'   - N_requirement: The calculated nitrogen requirement in kilograms per hectare (kg/ha).
#'   - units: The units of the output value ("kg/ha").
#'
#' @note The `uptake_table` should provide the unit crop absorption for the specified
#'   `crop`. The default table (`NFert::uptake_table`) may not be suitable for all regions
#'   or specific crop varieties.
#'
#' @export
calc_crop_N_demand <- function(expected_yield_tons_ha = 10,
                              uptake_table=NFert::uptake_table,
                              crop = "Mais trinciato (classe 700)") {

  # Error Handling and Input Validation
  if (!crop %in% uptake_table$crop) {
    stop(paste("Crop '", crop, "' not found in the uptake table."))
  }

  if (expected_yield_tons_ha <= 0) {
    stop("Expected yield must be a positive value.")
  }

  # Extraction and Calculation
  N_perc <- uptake_table$N[uptake_table$crop == crop]
  N_requirement_kg_ha <- expected_yield_tons_ha * N_perc * 10  # Directly calculate kg/ha

  # Return with Units
  return(list(
    N_requirement = N_requirement_kg_ha,
    units = "kg/ha"  # Add units for clarity
  ))
}

