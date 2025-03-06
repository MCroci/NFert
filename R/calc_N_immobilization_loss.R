#' Nitrogen Immobilization Loss Calculation
#'
#' Calculates the amount of nitrogen immobilized by soil microorganisms based on soil fertility, oxygen availability, and drainage.
#'
#' @param B Soil fertility factor (B) representing the potential nitrogen mineralization.
#' @param oxygen_availability Oxygen availability level in the soil (e.g., "Normal", "Reduced", "Poor").
#' @param id_rag Soil drainage index (ID_Rag), ranging from 1 (well-drained) to 5 (poorly drained).
#'
#' @return The estimated nitrogen immobilization loss (D) in kg/ha.
#' @export
#'
#' @details This function uses lookup tables (`ca.table` and `cb.table`) from the `NFert` package to determine the immobilization correction factor (`fc_D`) based on soil drainage and oxygen availability. The nitrogen immobilization loss is then calculated as:
#'
#'   D = B * fc_D
#'
#' @examples
#' calc_N_immobilization_loss(B = 50, oxygen_availability = "Normal", id_rag = 3)

calc_N_immobilization_loss <- function(B = 50, oxygen_availability = "Normal", id_rag = 1) {

  ca.table <- NFert::ca.table
  cb.table <- NFert::cb.table

  # Ottieni ID_Dre corrispondente all'oxygen_availability
  id_dre <- ca.table$ID_Dre[match(oxygen_availability, ca.table$availability)]

  # Controlla se id_dre è valido
  if (is.na(id_dre)) {
    stop("Valore non valido per oxygen_availability")
  }

  # Ottieni fc_D corrispondente a id_rag e id_dre
  fc_D <- cb.table$fc_D[cb.table$ID_Rag == id_rag & cb.table$ID_Dre == id_dre]

  # Controlla se fc_D è valido
  if (length(fc_D) == 0) {
    stop("Combinazione di id_rag e oxygen_availability non valida")
  }

  return(B * fc_D)
}

