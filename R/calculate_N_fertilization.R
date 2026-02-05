#' Calculate Nitrogen Fertilization Requirement
#'
#' Calculates the final nitrogen fertilization requirement (N_fert) from a nitrogen balance result.
#'
#' @param n_balance A data frame returned by `N_balance()` containing nitrogen balance components.
#' @param method The calculation method to use. Default is "standard" which uses the formula:
#'   N_fert = A - B + C1 + C2 + D - E - Forg - G
#'
#' @return A numeric value representing the nitrogen fertilization requirement in kg/ha.
#'
#' @details
#' This function calculates the final nitrogen fertilization requirement based on the nitrogen
#' balance components. If the result is negative, it returns 0 (no fertilization needed).
#'
#' The standard formula is:
#' ```
#' N_fert = A - B + C1 + C2 + D - E - Forg - G
#' ```
#'
#' Where:
#' - A: Crop nitrogen demand
#' - B: Soil nitrogen supply
#' - C1, C2: Leaching losses
#' - D: Immobilization losses
#' - E: Nitrogen from previous crop residues
#' - Forg: Nitrogen from organic fertilizer
#' - G: Natural nitrogen contribution
#'
#' @export
#'
#' @examples
#' # Calculate nitrogen balance
#' balance <- N_balance(
#'   expected_yield_tons_ha = 15,
#'   crop = "Mais trinciato (classe 700)",
#'   ccp = "Spring-summer crop 100–130 days",
#'   sand = 50, clay = 35,
#'   Ntot = 1.2, SOM = 1.2, CN = 9.5,
#'   oxygen_availability = "Normal",
#'   winter_rain = 160, start_spring_rain = 40,
#'   prev_crop = "Winter cereals straw removal",
#'   source = "Cattle slurry",
#'   fertorg_frequency = "every year",
#'   location = "Plain adjacent to urbanized areas",
#'   forg_quantity = 100
#' )
#'
#' # Calculate required nitrogen fertilization
#' n_fert <- calculate_N_fertilization(balance)
#' print(paste("Required N fertilization:", round(n_fert, 2), "kg/ha"))

calculate_N_fertilization <- function(n_balance, method = "standard") {
  # Input validation
  if (!is.data.frame(n_balance)) {
    stop("n_balance must be a data frame (typically returned by N_balance()).")
  }
  
  required_cols <- c("A", "B", "C1", "C2", "D", "E", "Forg", "G")
  missing_cols <- setdiff(required_cols, names(n_balance))
  if (length(missing_cols) > 0) {
    stop(paste("n_balance is missing required columns:",
               paste(missing_cols, collapse = ", ")))
  }
  
  if (nrow(n_balance) == 0) {
    stop("n_balance data frame is empty.")
  }
  
  # Extract values (handle first row if multiple rows exist)
  A <- n_balance$A[1]
  B <- n_balance$B[1]
  C1 <- n_balance$C1[1]
  C2 <- n_balance$C2[1]
  D <- n_balance$D[1]
  E <- n_balance$E[1]
  Forg <- n_balance$Forg[1]
  G <- n_balance$G[1]
  
  # Handle NA values (set to 0 for calculation purposes, but warn user)
  na_components <- character()
  if (is.na(A)) na_components <- c(na_components, "A")
  if (is.na(B)) { B <- 0; na_components <- c(na_components, "B") }
  if (is.na(C1)) { C1 <- 0; na_components <- c(na_components, "C1") }
  if (is.na(C2)) { C2 <- 0; na_components <- c(na_components, "C2") }
  if (is.na(D)) { D <- 0; na_components <- c(na_components, "D") }
  if (is.na(E)) { E <- 0; na_components <- c(na_components, "E") }
  if (is.na(Forg)) { Forg <- 0; na_components <- c(na_components, "Forg") }
  if (is.na(G)) { G <- 0; na_components <- c(na_components, "G") }
  
  if (length(na_components) > 0 && !("A" %in% na_components)) {
    warning(paste("Some balance components are NA and were set to 0:",
                  paste(na_components, collapse = ", ")))
  }
  
  if (is.na(A)) {
    stop("Crop nitrogen demand (A) is NA. Cannot calculate fertilization requirement.")
  }
  
  # Calculate nitrogen fertilization requirement
  if (method == "standard") {
    N_fert <- A - B + C1 + C2 + D - E - Forg - G
  } else {
    stop("Unknown method. Use 'standard'.")
  }
  
  # Return 0 if negative (no fertilization needed)
  N_fert <- max(0, N_fert)
  
  return(N_fert)
}
