#' @noRd
.crop_alias_to_canonical <- function() {
  # Common English / legacy names -> canonical Italian `uptake_table$crop`
  stats::setNames(
    c(
      "Mais trinciato classe 700",
      "Mais trinciato classe 700",
      "Mais trinciato classe 700"
    ),
    c(
      "Mais trinciato (classe 700)",
      "Maize silage (class 700)",
      "Shredded corn class 700"
    )
  )
}

#' Resolve a crop name (Italian or English) to its canonical entry
#'
#' NFert reference tables (`uptake_table`, `mas.table`, `crops.table`,
#' `standard_pk_doses.table`) carry the official Italian DPI crop names in
#' the column `crop` and an English translation in `crop_en`. This helper
#' accepts either form (case-insensitive, exact match) and returns the
#' canonical Italian name used as join key.
#'
#' If the input matches no Italian or English crop name, the input is
#' returned unchanged together with a warning, so the caller can decide how
#' to handle the mismatch.
#'
#' @param x Character. Crop name in Italian or English.
#' @param table Lookup table; default `NFert::uptake_table`.
#' @return Character. The canonical Italian crop name.
#' @examples
#' resolve_crop("Durum wheat (whole plant)")
#' resolve_crop("Grano duro (pianta intera)")
#' @export
resolve_crop <- function(x, table = NFert::uptake_table) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`x` must be a single non-NA character.")
  }
  # Normalise: trim outer spaces and collapse internal multiple spaces
  norm <- function(s) gsub("\\s+", " ", trimws(s))
  xn <- norm(x)

  # Apply known aliases (English / alternate spelling) before table lookup
  amap <- .crop_alias_to_canonical()
  i_alias <- match(tolower(xn), tolower(names(amap)))
  if (!is.na(i_alias)) xn <- norm(amap[i_alias])

  # Direct match on Italian name (with normalised whitespace)
  it_norm <- norm(as.character(table$crop))
  i <- which(it_norm == xn)
  if (length(i) > 0) return(as.character(table$crop[i[1]]))

  # Match on English name (case-insensitive, normalised whitespace)
  if ('crop_en' %in% names(table)) {
    en_norm <- norm(as.character(table$crop_en))
    i <- which(tolower(en_norm) == tolower(xn))
    if (length(i) > 0) return(as.character(table$crop[i[1]]))
  }

  warning(sprintf("Crop '%s' not found in lookup; returning input as-is.", x))
  x
}

#' Resolve a `ccp` (crop calendar period) string with whitespace and dash
#' normalisation
#'
#' Handles common variations between user input and the canonical English
#' value stored in `coef_time$ccp`:
#' \itemize{
#'   \item Trims outer whitespace and collapses internal multiple spaces.
#'   \item Replaces en-dash (U+2013) and em-dash (U+2014) with hyphen.
#'   \item Case-insensitive match.
#' }
#'
#' @param x Character. The `ccp` string to resolve.
#' @param table Lookup with column `ccp`; default `NFert::coef_time`.
#' @return The canonical `ccp` string (as in `table$ccp`), or `x` unchanged
#'   with a warning if not found.
#' @examples
#' resolve_ccp("Spring-summer crop 100\u2013130 days")  # em-dash
#' resolve_ccp("autumn-winter crop <150 days")         # case
#' @export
resolve_ccp <- function(x, table = NFert::coef_time) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`x` must be a single non-NA character.")
  }
  norm <- function(s) {
    s <- gsub("[\u2013\u2014]", "-", s)   # en/em dash -> hyphen
    s <- gsub("\\bcrop\\b", "", s, ignore.case = TRUE)  # optional "crop" word
    s <- gsub("\\s+", " ", trimws(s))
    tolower(s)
  }
  xn <- norm(x)
  cands <- norm(as.character(table$ccp))
  i <- which(cands == xn)
  if (length(i) > 0) return(as.character(table$ccp[i[1]]))
  # Last-resort: also try Italian alias if present
  if ('ccp_it' %in% names(table)) {
    cands_it <- norm(as.character(table$ccp_it))
    i <- which(cands_it == xn)
    if (length(i) > 0) return(as.character(table$ccp[i[1]]))
  }
  warning(sprintf("ccp '%s' not found in coef_time$ccp; returning input as-is.", x))
  x
}

#' List all crops with both Italian and English names
#'
#' @return A data frame with columns `crop` (Italian, canonical) and
#'   `crop_en` (English).
#' @examples
#' head(list_crops())
#' @export
list_crops <- function() {
  ut <- NFert::uptake_table
  if (!('crop_en' %in% names(ut))) {
    return(data.frame(crop = ut$crop, crop_en = ut$crop, stringsAsFactors = FALSE))
  }
  data.frame(crop = ut$crop, crop_en = ut$crop_en, stringsAsFactors = FALSE)
}
