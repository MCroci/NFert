#' End-to-end Nitrogen Nutrition Index pipeline from Sentinel-2 L2A
#'
#' Combines the outputs of \code{\link{estimate_biophysical}} with a crop-
#' specific critical-N dilution curve (\code{\link{crop_params_NNI}}) to
#' produce a pixel-level NNI map and the derived agronomic zones
#' (\emph{deficient} / \emph{optimal} / \emph{excessive}) in a single call.
#'
#' The pipeline implements the classical Lemaire & Gastal (1997)
#' formulation:
#' \deqn{W_{leaf} = LAI \cdot C_m \cdot 10^4  \quad (\mathrm{g\ m^{-2}})}
#' \deqn{W = W_{leaf} / \alpha_{leaf}  \quad (\mathrm{g\ m^{-2}})}
#' \deqn{N_{total} = k \cdot CNC\_Cab  \quad \mathrm{or}  \quad N_{total} = CNC\_Cprot}
#' \deqn{N_{actual}(\%) = 100 \cdot N_{total} / W}
#' \deqn{N_c(\%) = a \cdot (W_{tha})^{-b}}
#' \deqn{NNI = N_{actual} / N_c}
#' where \eqn{\alpha_{leaf}} is the leaf-to-total aboveground allocation
#' coefficient and \eqn{k} the N-to-chlorophyll ratio (both crop-specific,
#' see \code{\link{crop_params_NNI}}). \eqn{W_{tha}} is W rescaled to
#' t DM ha\eqn{^{-1}}.
#'
#' Two N-content pathways are supported:
#' \itemize{
#'   \item \strong{Protein path} (\code{cnc_layer = "CNC_Cprot"}, default).
#'     Uses the GPR retrieval trained on canopy N from protein content
#'     directly — the most internally consistent path, no extra
#'     coefficient is needed.
#'   \item \strong{Chlorophyll path} (\code{cnc_layer = "CNC_Cab"}).
#'     Uses the canopy chlorophyll retrieval and multiplies it by a
#'     canopy-level N:Chl ratio \eqn{k} (default ~55-60, requires local
#'     calibration — see \code{\link{crop_params_NNI}}). Preferred only
#'     when CNC_Cab is the best-trained model for the target crop or
#'     when the calibration work has already been done.
#' }
#'
#' @section Masking:
#' Two masks are applied before the NNI computation:
#' \itemize{
#'   \item If an \code{fvc} raster is supplied, pixels with
#'     \code{FVC < crop_params$fvc_min} are removed (default threshold
#'     0.50). Below this level the BRDF is contaminated by the soil
#'     background and the GPR retrievals degrade.
#'   \item Pixels with W below \code{crop_params$w_min} are masked because
#'     the critical-N dilution curve is not defined at the seedling
#'     stage (default 1 t DM ha\eqn{^{-1}}).
#' }
#' Optional Sentinel-2 SCL (Scene Classification Layer) classes can be
#' dropped via \code{scl_keep} (vector of valid classes; defaults to the
#' standard vegetation-safe classes 4, 5, 6, 7 = VEGETATION, NOT_VEGETATED,
#' WATER, UNCLASSIFIED).
#'
#' @section Zones:
#' The NNI is discretised into three classes following Lemaire & Gastal
#' (1997):
#' \tabular{lll}{
#'   \strong{Zone} \tab \strong{NNI range} \tab \strong{Code} \cr
#'   Deficient \tab NNI < 0.90 \tab 1 \cr
#'   Optimal   \tab 0.90 <= NNI <= 1.10 \tab 2 \cr
#'   Excessive \tab NNI > 1.10 \tab 3
#' }
#' Thresholds are user-configurable via the \code{nni_thresholds} argument.
#'
#' @param lai_rast SpatRaster or path to the LAI layer produced by
#'   \code{\link{estimate_biophysical}}.
#' @param cm_rast SpatRaster or path to the Cm layer.
#' @param cnc_rast SpatRaster or path to the canopy N layer.
#' @param cnc_layer Character. Either \code{"CNC_Cprot"} (default) or
#'   \code{"CNC_Cab"}. Controls which conversion is applied.
#' @param crop Character. Crop name, resolved via
#'   \code{\link{crop_params_NNI}}.
#' @param params Optional list overriding the defaults returned by
#'   \code{\link{crop_params_NNI}} (e.g. for locally calibrated
#'   \code{alpha_leaf} or \code{k_NChl}).
#' @param fvc A SpatRaster (or path) with fractional vegetation cover,
#'   optional. If provided the pipeline drops pixels with
#'   \code{FVC < params$fvc_min}.
#' @param scl A SpatRaster (or path) with the Sentinel-2 SCL layer,
#'   optional. Pixels not in \code{scl_keep} are masked.
#' @param scl_keep Integer vector of SCL classes to keep (default
#'   \code{c(4, 5, 6, 7)} = VEGETATION, NOT_VEGETATED, WATER, UNCLASSIFIED).
#' @param nni_thresholds Numeric length-2 vector with the lower and upper
#'   NNI thresholds (default \code{c(0.90, 1.10)}).
#' @param return_intermediates Logical. If \code{TRUE} the returned list
#'   also contains the intermediate W, N_total and N_actual rasters.
#'
#' @return A named list of SpatRaster layers:
#' \itemize{
#'   \item \code{W} (t DM ha\eqn{^{-1}}) -- aboveground biomass
#'   \item \code{N_actual} (pct DM) -- plant N concentration
#'   \item \code{N_crit} (pct DM) -- critical N from dilution curve
#'   \item \code{NNI} -- Nitrogen Nutrition Index (dimensionless)
#'   \item \code{zones} -- integer raster (1 deficient / 2 optimal / 3 excessive)
#'   \item \code{mask} -- logical raster of retained pixels
#' }
#' Returned items are \code{NULL} when \code{return_intermediates = FALSE}
#' for any intermediate variable not requested.
#'
#' @seealso \code{\link{estimate_biophysical}}, \code{\link{compute_NNI}},
#'   \code{\link{diagnose_N_status}}, \code{\link{crop_params_NNI}},
#'   \code{\link{variable_rate_N}}
#'
#' @references
#' Lemaire G, Gastal F. N uptake and distribution in plant canopies. In:
#' Diagnosis of the Nitrogen Status in Crops, Springer, 1997.
#'
#' Houles V, Guerif M, Mary B. Elaboration of a nitrogen nutrition
#' indicator for winter wheat based on leaf area index and chlorophyll
#' content. Eur J Agron 2007;27:1-11.
#'
#' @examples
#' \dontrun{
#' maps <- estimate_biophysical("scene.tif", "out",
#'   variables = c("LAI", "Cm", "CNC_Cprot", "FVC"),
#'   model_dir = "models_json")
#' nni <- compute_NNI_from_S2(
#'   lai_rast = maps$LAI,
#'   cm_rast  = maps$Cm,
#'   cnc_rast = maps$CNC_Cprot,
#'   cnc_layer = "CNC_Cprot",
#'   crop     = "maize",
#'   fvc      = maps$FVC
#' )
#' terra::writeRaster(nni$NNI,   "NNI.tif",   overwrite = TRUE)
#' terra::writeRaster(nni$zones, "zones.tif", overwrite = TRUE)
#' }
#' @export
compute_NNI_from_S2 <- function(
  lai_rast,
  cm_rast,
  cnc_rast,
  cnc_layer             = c("CNC_Cprot", "CNC_Cab"),
  crop,
  params                = NULL,
  fvc                   = NULL,
  scl                   = NULL,
  scl_keep              = c(4L, 5L, 6L, 7L),
  nni_thresholds        = c(0.90, 1.10),
  return_intermediates  = TRUE
) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for compute_NNI_from_S2().")

  cnc_layer <- match.arg(cnc_layer)
  if (is.null(params)) params <- crop_params_NNI(crop)
  # user overrides
  for (nm in c("a", "b", "w_min", "alpha_leaf", "k_NChl", "fvc_min"))
    if (!(nm %in% names(params)))
      params[[nm]] <- crop_params_NNI(crop)[[nm]]

  lai <- .as_spatraster(lai_rast)
  cm  <- .as_spatraster(cm_rast)
  cnc <- .as_spatraster(cnc_rast)

  # -- Biomass: leaf, then total
  # LAI [m2/m2] * Cm [g/cm2 leaf] * 1e4 = g m^-2 ground leaf dry-mass
  W_leaf_g_m2  <- lai * cm * 1e4
  W_total_g_m2 <- W_leaf_g_m2 / params$alpha_leaf
  # g m^-2 -> t ha^-1:  1 g m^-2 = 0.01 t ha^-1
  W <- W_total_g_m2 * 0.01

  # -- N content (g N m^-2 ground)
  N_total_g_m2 <- switch(cnc_layer,
    CNC_Cprot = cnc,                       # already in g N m^-2
    CNC_Cab   = cnc * params$k_NChl        # g Chl m^-2 * k -> g N m^-2
  )

  # -- Plant N concentration (% DM)
  # N_actual(%) = 100 * N_total_g_m2 / W_total_g_m2
  N_actual <- 100 * N_total_g_m2 / W_total_g_m2

  # -- Critical N from dilution curve
  N_crit <- params$a * (W ^ (-params$b))

  NNI <- N_actual / N_crit

  # -- Mask
  keep <- !is.na(W) & W >= params$w_min
  if (!is.null(fvc)) {
    fvc_r <- .as_spatraster(fvc)
    keep  <- keep & !is.na(fvc_r) & (fvc_r >= params$fvc_min)
  }
  if (!is.null(scl)) {
    scl_r <- .as_spatraster(scl)
    keep  <- keep & (scl_r %in% scl_keep)
  }

  W        <- terra::mask(W,        keep, maskvalues = FALSE)
  N_actual <- terra::mask(N_actual, keep, maskvalues = FALSE)
  N_crit   <- terra::mask(N_crit,   keep, maskvalues = FALSE)
  NNI      <- terra::mask(NNI,      keep, maskvalues = FALSE)

  thr_lo <- nni_thresholds[1]
  thr_hi <- nni_thresholds[2]

  rcl <- matrix(c(-Inf,  thr_lo, 1,
                  thr_lo, thr_hi, 2,
                  thr_hi,  Inf,  3),
                ncol = 3, byrow = TRUE)
  zones <- terra::classify(NNI, rcl, include.lowest = TRUE, right = FALSE)

  out <- list(
    NNI     = NNI,
    zones   = zones,
    N_crit  = N_crit,
    mask    = keep,
    params  = params
  )
  if (isTRUE(return_intermediates)) {
    out$W        <- W
    out$N_actual <- N_actual
  }
  out
}
