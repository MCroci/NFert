#' Maximum annual organic matter input (t ss/ha)
#'
#' Returns the DPI 2026 maximum annual input of soil organic matter (SO) as
#' dry matter t/ha, based on the classification of initial SO in soil:
#' "Scarsa" = 13, "Normale" = 11, "Elevata" = 9 (Fert_Office v1.26 foglio SO).
#'
#' @param so_class One of `"Scarsa"`, `"Normale"`, `"Elevata"`.
#' @param so_max_input Lookup table (default `NFert::so_max_input`).
#' @return Numeric t ss/ha.
#' @examples max_SO_input("Normale")
#' @export
max_SO_input <- function(so_class = c("Scarsa", "Normale", "Elevata"),
                         so_max_input = NFert::so_max_input) {
  so_class <- match.arg(so_class)
  v <- so_max_input$max_t_ss_ha[so_max_input$`class` == so_class]
  as.numeric(v[1])
}

#' Classify SO (soil organic matter) initial content by texture group
#'
#' Returns DPI 2026 SO class (molto bassa / bassa / media / elevata) by
#' soil texture group (Sabbiosi / Medio impasto / Argillosi e limosi) and
#' maps to the 3-class scheme "Scarsa / Normale / Elevata" used for the
#' maximum annual SO input.
#'
#' @param SOM Soil organic matter (% mass).
#' @param soil_group Texture group (Italian).
#' @param so.table Lookup (default `NFert::so.table`).
#' @return List with `rating` (4 classes) and `class` (3-class for SO input).
#' @examples classify_SOM(SOM = 2, soil_group = "Medio impasto")
#' @export
classify_SOM <- function(SOM, soil_group, so.table = NFert::so.table) {
  if (!is.numeric(SOM) || length(SOM) != 1 || is.na(SOM) || SOM < 0) {
    stop("`SOM` must be a non-negative numeric.")
  }
  # Thresholds from foglio B (NFert 0.1.0 already handles fertility; here we
  # just classify SO for the organic-input limit)
  th <- list(
    "Sabbiosi"           = c(0.8, 1.4, 2.0),
    "Medio impasto"      = c(1.0, 1.8, 2.5),
    "Argillosi e limosi" = c(1.2, 2.2, 3.0)
  )
  sg <- normalise_soil_group(soil_group)$it_plural
  if (!(sg %in% names(th))) {
    stop(sprintf("Unknown soil_group '%s' (canonical: '%s').", soil_group, sg))
  }
  b <- th[[sg]]
  rating <- if (SOM < b[1]) "molto bassa" else
    if (SOM < b[2]) "bassa" else
      if (SOM < b[3]) "media" else "elevata"

  # Map to "Scarsa" / "Normale" / "Elevata"
  cl <- switch(rating,
    "molto bassa" = "Scarsa",
    "bassa"       = "Scarsa",
    "media"       = "Normale",
    "elevata"     = "Elevata")

  list(rating = rating, class = cl, SOM = SOM, soil_group = soil_group)
}
