#' Crop-specific parameters for the Nitrogen Nutrition Index pipeline
#'
#' This function returns the default parameters used by the remote-sensing
#' NNI pipeline (\code{\link{compute_NNI_from_S2}}) for a given crop. Values
#' are taken from the peer-reviewed literature and represent first-guess
#' defaults — they almost always require local recalibration before
#' operational use (see the \emph{Calibration caveats} section below).
#'
#' @section Parameters:
#' \describe{
#'   \item{a, b}{Coefficients of the critical-N dilution curve
#'     \eqn{N_c(\%) = a \cdot W^{-b}}, with W in t DM ha\eqn{^{-1}}.
#'     Lemaire & Gastal (1997); Justes et al. (1994) for wheat;
#'     Plenet & Lemaire (2000) for maize; Sheehy et al. (1998) for rice;
#'     Colnenne et al. (1998) for rapeseed.}
#'   \item{w_min}{Lower bound of validity of the dilution curve
#'     (t DM ha\eqn{^{-1}}). NNI is masked below this biomass.}
#'   \item{alpha_leaf}{Leaf-to-total-aboveground biomass allocation
#'     coefficient during the vegetative stages the NNI is meant for.
#'     W_total = W_leaf / alpha_leaf, so that typical partitioning of
#'     0.30 (maize) / 0.35 (wheat) / 0.40 (rice) rescales the
#'     GPR-derived leaf biomass to aboveground biomass.}
#'   \item{k_NChl}{Canopy-level N : chlorophyll ratio used when converting
#'     \code{CNC_Cab} (g Chl m\eqn{^{-2}} ground) to canopy N
#'     (g N m\eqn{^{-2}}). Values in the 50-80 range; defaults are those
#'     reported by Houles et al. (2007) and Baret et al. (2007) as
#'     first-guess for Sentinel-2. Not used when the pipeline is fed with
#'     \code{CNC_Cprot} (protein-retrieval path, which already returns
#'     canopy N directly).}
#'   \item{fvc_min}{Minimum fractional vegetation cover required for the
#'     spectral signal to be dominated by canopy and not soil. Pixels
#'     below this threshold are masked out of the NNI retrieval.}
#' }
#'
#' @section Calibration caveats:
#' \itemize{
#'   \item The dilution-curve coefficients a and b are well established for
#'     the listed crops across European cropping systems; they rarely need
#'     recalibration.
#'   \item \code{alpha_leaf} and \code{k_NChl} are the two locally
#'     sensitive parameters. Under-calibrated values commonly introduce a
#'     15-30pct bias in the final NNI map. A minimal calibration dataset
#'     consists of 30-50 plot-level samples across the growing season,
#'     analysed for dry biomass (oven at 65 degC) and total plant N
#'     (Kjeldahl or Dumas), paired with the matching Sentinel-2
#'     acquisition date.
#'   \item NNI is only meaningful inside crop-specific phenological
#'     windows (e.g. GS31-GS55 for wheat, V6-V12 for maize). Outside
#'     these windows the critical-N curve is not valid.
#'   \item When the scene is dominated by partially covered pixels
#'     (FVC < 0.5), the GPR retrievals degrade because the soil signal
#'     enters the BRDF. Use \code{fvc_min} to prune them.
#' }
#'
#' @param crop Character. Canonical crop name (case-insensitive). Any of
#'   \code{"wheat"}, \code{"maize"}, \code{"rice"}, \code{"barley"},
#'   \code{"rapeseed"}, \code{"sorghum"}, \code{"sunflower"},
#'   \code{"soybean"}. A helpful error is raised otherwise.
#'
#' @return A named list of seven numeric parameters plus the resolved
#'   canonical \code{crop} name.
#' @references
#' Lemaire G, Gastal F. N uptake and distribution in plant canopies. In:
#' Diagnosis of the Nitrogen Status in Crops, Springer, 1997. p. 3-43.
#'
#' Justes E, Mary B, Meynard J-M, Machet J-M, Thelier-Huche L. Determination
#' of a critical nitrogen dilution curve for winter wheat crops. Ann Bot
#' 1994;74:397-407.
#'
#' Plenet D, Lemaire G. Relationships between dynamics of nitrogen uptake
#' and dry matter accumulation in maize crops. Plant Soil 2000;216:65-82.
#'
#' Sheehy JE, Dionora MJA, Mitchell PL, Peng S, Cassman KG, Lemaire G,
#' Williams RL. Critical nitrogen concentrations: implications for high-
#' yielding rice genotypes in the tropics. Field Crops Res 1998;59:31-41.
#'
#' Colnenne C, Meynard J-M, Reau R, Justes E, Merrien A. Determination of a
#' critical nitrogen dilution curve for winter oilseed rape. Ann Bot
#' 1998;81:311-317.
#'
#' Houles V, Guerif M, Mary B. Elaboration of a nitrogen nutrition
#' indicator for winter wheat based on leaf area index and chlorophyll
#' content. Eur J Agron 2007;27:1-11.
#'
#' Baret F, Houles V, Guerif M. Quantification of plant stress using
#' remote sensing observations and crop models. J Exp Bot 2007;58:869-880.
#' @export
crop_params_NNI <- function(crop) {
  key <- tolower(trimws(as.character(crop)))
  aliases <- c(
    "wheat" = "wheat", "frumento" = "wheat", "winter wheat" = "wheat",
    "durum wheat" = "wheat", "triticum" = "wheat",
    "maize" = "maize", "mais" = "maize", "corn" = "maize", "zea" = "maize",
    "rice" = "rice", "riso" = "rice", "oryza" = "rice",
    "barley" = "barley", "orzo" = "barley", "hordeum" = "barley",
    "rapeseed" = "rapeseed", "colza" = "rapeseed",
    "oilseed rape" = "rapeseed", "brassica" = "rapeseed",
    "sorghum" = "sorghum", "sorgo" = "sorghum",
    "sunflower" = "sunflower", "girasole" = "sunflower",
    "soybean" = "soybean", "soia" = "soybean", "soy" = "soybean"
  )
  if (!(key %in% names(aliases)))
    stop(sprintf(
      "Unknown crop '%s'. Supported: %s.",
      crop, paste(sort(unique(unname(aliases))), collapse = ", ")))
  canonical <- unname(aliases[[key]])

  tab <- list(
    wheat    = list(a = 5.35, b = 0.44, w_min = 1.0,
                    alpha_leaf = 0.35, k_NChl = 55, fvc_min = 0.50),
    maize    = list(a = 3.40, b = 0.37, w_min = 1.0,
                    alpha_leaf = 0.30, k_NChl = 60, fvc_min = 0.50),
    rice     = list(a = 5.18, b = 0.52, w_min = 1.0,
                    alpha_leaf = 0.40, k_NChl = 55, fvc_min = 0.50),
    barley   = list(a = 4.80, b = 0.32, w_min = 1.0,
                    alpha_leaf = 0.35, k_NChl = 55, fvc_min = 0.50),
    rapeseed = list(a = 4.48, b = 0.25, w_min = 1.0,
                    alpha_leaf = 0.45, k_NChl = 50, fvc_min = 0.50),
    sorghum  = list(a = 3.90, b = 0.39, w_min = 1.0,
                    alpha_leaf = 0.30, k_NChl = 60, fvc_min = 0.50),
    sunflower= list(a = 4.53, b = 0.42, w_min = 1.0,
                    alpha_leaf = 0.45, k_NChl = 55, fvc_min = 0.50),
    soybean  = list(a = 5.00, b = 0.32, w_min = 1.0,
                    alpha_leaf = 0.40, k_NChl = 55, fvc_min = 0.50)
  )
  out <- tab[[canonical]]
  out$crop <- canonical
  out
}
