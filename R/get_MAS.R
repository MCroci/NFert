#' Maximum Allowed Doses (MAS) per DPI Emilia-Romagna
#'
#' Returns or checks the Maximum Allowed Doses (Massimali Ammessi dal DPI, MAS)
#' of nitrogen for a crop, as defined in the Disciplinari di Produzione
#' Integrata (DPI) Emilia-Romagna (Allegato 9, Reg. reg. 2/2024). In ZVN the
#' values are binding; they can be exceeded if production over 3 years exceeds
#' the reference yield.
#'
#' @param crop Character. Crop name (as in DPI nomenclature). Use
#'   `get_MAS(NULL)` or `get_MAS()` to list all crops.
#' @param mas_table Data frame with columns `crop`, `mas_N`, `reference_yield`, `type`.
#'   If NULL, uses the built-in table for the chosen `edition`.
#' @param edition Character. `"2026"` (default) or `"2025"`. 2025 = Guida DPI
#'   2025 (ZVN table); 2026 = FertDPI / Allegato 9 style.
#'
#' @return For `get_MAS(crop)`: a one-row data frame with columns
#'   `crop`, `mas_N`, `reference_yield` (and if present `mas_P2O5`, `yield_ref_min`,
#'   `yield_ref_max`), `type`, or all rows if `crop` is NULL. For `check_MAS`: a list with
#'   `ok` (logical), `mas_N`, `N_planned`, `message`.
#'
#' @references
#' DPI Emilia-Romagna – Norme Generali 2025, Allegato 9; Guida alla
#' Fertilizzazione Minerale e Organica 2025 (N, P, K). Reg. reg. 2/2024.
#' DPI 2026, FertDPI / Fert_Office_v1_26.
#'
#' @export
#' @examples
#' get_MAS("Frumento tenero (granella)")
#' get_MAS("Frumento tenero (granella)", edition = "2025")  # ZVN 2025
#' get_MAS()  # list all
#' check_MAS("Mais da granella", 250)
#' check_MAS("Frumento tenero (granella)", 210)  # over MAS
get_MAS <- function(crop = NULL, mas_table = NULL, edition = c("2026", "2025")) {
  edition <- match.arg(edition)
  if (is.null(mas_table)) mas_table <- if (edition == "2025") .mas_table_dpi2025() else .mas_table_dpi2026()
  if (is.null(crop)) return(mas_table)
  crop <- as.character(crop)
  # Accept English crop name if mas_table has crop_en
  if ('crop_en' %in% names(mas_table) && !(crop %in% mas_table$crop)) {
    j <- which(tolower(mas_table$crop_en) == tolower(crop))
    if (length(j) > 0) crop <- as.character(mas_table$crop[j[1]])
  }
  i <- match(crop, mas_table$crop)
  if (is.na(i)) {
    warning("Crop '", crop, "' not found in MAS table. Return NULL. Use get_MAS() to see available crops.")
    return(NULL)
  }
  mas_table[i, , drop = FALSE]
}

#' Check planned N against MAS
#'
#' Checks whether a planned nitrogen dose (kg/ha) exceeds the Maximum Allowed
#' Dose (MAS) for the crop. In ZVN, also consider the 170 kg N/ha/year limit
#' from livestock effluents.
#'
#' @param crop Character. Crop name (DPI nomenclature).
#' @param N_planned Numeric. Planned nitrogen dose (kg/ha).
#' @param mas_table Optional. Same as in `get_MAS`. Default uses built-in table.
#'
#' @param edition Character. `"2026"` or `"2025"`; used only if `mas_table` is NULL.
#' @return List with `ok` (TRUE if N_planned <= MAS or crop has no MAS_N),
#'   `mas_N`, `N_planned`, `message`.
#' @export
check_MAS <- function(crop, N_planned, mas_table = NULL, edition = c("2026", "2025")) {
  edition <- match.arg(edition)
  if (is.null(mas_table)) mas_table <- if (edition == "2025") .mas_table_dpi2025() else .mas_table_dpi2026()
  row <- get_MAS(crop, mas_table)
  if (is.null(row) || nrow(row) == 0L) {
    return(list(ok = NA, mas_N = NA, N_planned = N_planned,
                message = "Crop not found in MAS table; check not performed."))
  }
  mas_N <- row$mas_N
  if (is.na(mas_N)) {
    return(list(ok = TRUE, mas_N = NA, N_planned = N_planned,
                message = "No MAS N defined for this crop (e.g. soya)."))
  }
  ok <- N_planned <= mas_N
  msg <- if (ok) "Planned N is within MAS." else
    paste0("Planned N (", N_planned, " kg/ha) exceeds MAS (", mas_N, " kg/ha).")
  list(ok = ok, mas_N = mas_N, N_planned = N_planned, message = msg)
}

#' Built-in MAS table – DPI 2025 Guida (ZVN, resa riferimento)
#' Reg. reg. 2/2024, Allegato 9.
#' @noRd
.mas_table_dpi2025 <- function() {
  data.frame(
    crop = c(
      "Frumento tenero (granella)",
      "Grano tenero FF (granella)",
      "Frumento duro (granella)",
      "Grano duro (granella)",
      "Orzo",
      "Mais da granella",
      "Mais da insilato",
      "Shredded corn class 700",
      "Soia",
      "Barbabietola da zucchero",
      "Pomodoro da industria",
      "Patata",
      "Melo",
      "Pero",
      "Pesco e Nettarine",
      "Vite (uva da vino)",
      "Asparago verde"
    ),
    mas_N = c(180L, 180L, 190L, 190L, 150L, 280L, 280L, 280L, 30L, 160L, 180L, 190L, 120L, 120L, 175L, 100L, 210L),
    reference_yield = c(6.5, 6.5, 6.0, 6.0, 6.0, 13.0, 23.0, 23.0, NA_real_, 60.0, 80.0, 48.0, 35.0, 30.0, 25.0, 18.0, 7.0),
    type = c(
      "Erbacee", "Erbacee", "Erbacee", "Erbacee", "Erbacee", "Erbacee", "Erbacee", "Erbacee",
      "Erbacee", "Erbacee", "Orticole", "Orticole", "Arboree", "Arboree", "Arboree", "Arboree", "Orticole"
    ),
    stringsAsFactors = FALSE
  )
}

#' Built-in MAS table (DPI 2026 / FertDPI style)
#' @noRd
.mas_table_dpi2026 <- function() {
  data.frame(
    crop = c(
      "Frumento tenero (granella)",
      "Grano tenero FF (granella)",   # alias used in NFert uptake_table
      "Frumento tenero (pianta intera)",
      "Grano duro (granella)",
      "Mais da granella",
      "Mais da insilato",
      "Shredded corn class 700",       # alias for maize silage
      "Orzo",
      "Girasole",
      "Soia",
      "Pomodoro da industria",
      "Melo",
      "Pero",
      "Pesco e Nettarine",
      "Vite (uva da vino)",
      "Actinidia",
      "Ciliegio"
    ),
    mas_N = c(200L, 200L, 200L, 200L, 260L, 340L, 340L, 180L, 160L, NA_integer_,
              200L, 340L, 340L, 340L, 120L, 340L, 340L),
    mas_P2O5 = c(100L, 100L, 100L, 100L, 150L, 150L, 150L, 90L, 90L, 100L,
                 130L, 120L, 120L, 175L, 60L, 150L, 120L),
    yield_ref_min = c(6, 6, 6, 5, 10, 40, 40, 5, 2, 3, 70, 35, 30, 25, 8, 25, 9),
    yield_ref_max = c(8, 8, 8, 6, 13, 50, 50, 7, 3, 4, 100, 35, 30, 25, 12, 25, 9),
    type = c(
      "Erbacee", "Erbacee", "Erbacee", "Erbacee", "Erbacee", "Erbacee", "Erbacee",
      "Erbacee", "Erbacee", "Erbacee", "Orticole",
      "Arboree", "Arboree", "Arboree", "Arboree", "Arboree", "Arboree"
    ),
    stringsAsFactors = FALSE
  )
}
