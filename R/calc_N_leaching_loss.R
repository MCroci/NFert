#' Leaching Loss of Nitrogen
#'
#' DPI 2026 (Allegato 2; foglio \verb{C&D} di Fert_Office v1.26): loss of readily
#' available mineral nitrogen (\eqn{b_1}) from leaching.
#'
#' \describe{
#'   \item{\strong{C1} (\eqn{C_a}, autumn--winter)}{
#'     Depends on winter rainfall (1 October -- 31 January), \code{winter_rain} (mm):
#'     \itemize{
#'       \item \eqn{\leq 150} mm \eqn{\rightarrow} no loss (0);
#'       \item \eqn{(150,\,250]} mm \eqn{\rightarrow} linear fraction of \eqn{b_1}:
#'             \eqn{b_1 \times (R_{\mathrm{win}} - 150) / 100};
#'       \item \eqn{> 250} mm \eqn{\rightarrow} loss of the entire \eqn{b_1} pool
#'             (cannot exceed \eqn{b_1}).
#'     }
#'   }
#'   \item{\strong{C2} (\eqn{C_b}, February)}{
#'     Additional loss from February rainfall (\code{start_spring_rain}, mm) applied
#'     only to the \emph{residual} readily available N after \eqn{C_1}, and only if
#'     \code{winter_rain > 150}. Formula: \eqn{\min(\code{start_spring_rain}/10,\, b_1 - C_1)}.
#'   }
#' }
#'
#' Invariant: \eqn{C_1 + C_2 \leq b_1}.
#'
#' \code{oxygen_availability} is validated against \code{\link{ca.table}} for
#' consistency with other balance terms; the rainfall formulas above do not use
#' the legacy tabular column \code{cb.table$C} (that column referred to the old,
#' incorrect assignment of a fixed loss independent of rainfall).
#'
#' @param winter_rain Winter rainfall (October 1 to January 31) in mm.
#' @param start_spring_rain Rainfall in February in mm.
#' @param oxygen_availability Oxygen availability level in the soil: one of
#'   \code{ca.table$availability} (e.g. \code{"Normal"}, \code{"Slow"}, \code{"Fast"}).
#' @param id_rag Soil drainage index (\code{ID_Rag}); retained for backward
#'   compatibility with earlier NFert signatures (not used in the \eqn{C_a}/\eqn{C_b}
#'   formulas above).
#' @param b1 Readily available nitrogen in the soil (kg N/ha).
#'
#' @return A list containing:
#'         \item{C1}{Autumn--winter leaching loss (kg N/ha).}
#'         \item{C2}{February leaching loss (kg N/ha).}
#'         \item{surplus_pluviometrico}{Logical. \code{TRUE} when
#'           \code{winter_rain + start_spring_rain >= 300} mm (DPI 2026 scheda a
#'           dose standard: surplus-pluviometric flag).}
#'
#' @export
#'
#' @examples
#' leaching_loss(winter_rain = 160, start_spring_rain = 40,
#'               oxygen_availability = "Normal", id_rag = 3, b1 = 29.16)

leaching_loss <- function(winter_rain = 160, start_spring_rain = 40,
                          oxygen_availability = "Normal", id_rag = 3, b1 = 29.16) {

  if (!is.numeric(id_rag) || length(id_rag) != 1L || !is.finite(id_rag)) {
    stop("`id_rag` must be a single finite numeric value.", call. = FALSE)
  }
  if (!is.numeric(b1) || length(b1) != 1L || !is.finite(b1) || b1 < 0) {
    stop("`b1` must be a non-negative finite numeric value.", call. = FALSE)
  }

  oxygen_availability <- nfert_normalize_oxygen(oxygen_availability)
  ca.table <- nfert_data_get("ca.table")
  avail <- unique(trimws(as.character(ca.table[["availability"]])))

  if (!oxygen_availability %in% avail) {
    stop(
      "Invalid value for oxygen_availability. Use one of: ",
      paste(sort(avail), collapse = ", "),
      call. = FALSE
    )
  }

  # Ca: autumn–winter — fraction of b1 for rainfall in (150, 250] mm; full b1 above 250 mm
  percentage_loss <- pmin(pmax(winter_rain - 150, 0), 100)
  C1 <- pmin(b1 * (percentage_loss / 100), b1)

  # Cb: February — only on residual b1, only after winter rain exceeds 150 mm
  b1_residual <- max(b1 - C1, 0)
  february_loss <- if (isTRUE(winter_rain > 150) && isTRUE(start_spring_rain > 0)) {
    pmin(start_spring_rain / 10, b1_residual)
  } else {
    0
  }
  C2 <- february_loss

  # DPI 2026 scheda standard: surplus pluviometrico (foglio C&D)
  surplus_pluviometrico <- isTRUE((winter_rain + start_spring_rain) >= 300)

  list(C1 = C1, C2 = C2, surplus_pluviometrico = surplus_pluviometrico)
}
