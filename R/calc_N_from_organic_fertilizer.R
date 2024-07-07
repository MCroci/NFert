#' Nitrogen Provided by Organic Fertilization
#'
#' Calculates the amount of nitrogen (N) supplied to the soil through organic fertilization.
#'
#' @param source The source of organic fertilizer (e.g., "Cattle slurry", "Composted manure").
#' @param frequency The frequency of application (e.g., "every year", "every two years").
#' @param quantity The amount of organic fertilizer applied per application (in m3/ha or t/ha).
#' @param f.table A data frame containing information about nitrogen content and application factors
#'                for different organic fertilizers and frequencies. This should have at least
#'                three columns: `source` (character), `frequency` (character), and `value` (numeric).
#'                The default is `NFert::f.table`.
#'
#' @return The estimated amount of nitrogen (N) in kg/ha provided by the organic fertilization.
#'
#' @note The `f.table` should provide the relevant conversion factors (N content and application
#'       factors) for the specified `source` and `frequency`. The default table (`NFert::f.table`)
#'       may not be suitable for all regions or specific organic fertilizers.
#'
#' @export
#'
#' @examples
#' Nitrogen_from_organic_fertilization(source = "Cattle slurry", frequency = "every year", quantity = 50)
#'
organic_fertilization <- function(source = "Cattle slurry", frequency = "every year",
                                                quantity = 100, f.table = NFert::f.table) {

  # Input Validation
  if (!source %in% f.table$source) {
    stop(paste("Organic fertilizer source '", source, "' not found in the table."))
  }

  if (!frequency %in% f.table$frequency) {
    stop(paste("Frequency '", frequency, "' not found in the table."))
  }

  if (quantity <= 0) {
    stop("Quantity of organic fertilizer must be a positive value.")
  }

  # Retrieve Value and Calculate
  factor <- f.table$value[f.table$source == source & f.table$frequency == frequency]

  # Check if factor is missing (NA)
  if (is.na(factor)) {
    stop(paste("No factor found for source '", source, "' and frequency '", frequency, "'."))
  }

  Forg <- factor * quantity / 100

  return(Forg)
}
