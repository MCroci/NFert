#' Immobiization and dispersion
#'
#' @param B Soil fertility
#' @param oxygen_availability
#' @param id_rag
#'
#' @return
#' @export
#'
#' @examples
calc_N_immobilization_loss <- function(B=0,  oxygen_availability = "Normal", id_rag = 1) {

  ca.table <- NFert::ca.table; cb.table <- NFert::cb.table
  id_dre <- as.numeric( ca.table[ca.table[,"availability"]==oxygen_availability, "ID_Dre"] )
  fc_D <- as.numeric( cb.table[cb.table[,"ID_Rag"]==id_rag & cb.table[,"ID_Dre"]==id_dre , "fc_D"] )
  D <- B*fc_D
  return(D)
}
