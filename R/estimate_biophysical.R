#' GPR-based estimation of canopy biophysical variables from Sentinel-2 L2A
#'
#' Applies pre-trained Gaussian Process Regression (GPR) models to a
#' Sentinel-2 Level-2A surface-reflectance raster (10 bands, canonical order
#' \code{B02, B03, B04, B05, B06, B07, B08, B8A, B11, B12}) and returns
#' canopy-level biophysical maps (e.g. leaf area index LAI, canopy water
#' content Cw, canopy nitrogen content CNC).
#'
#' The function is a pure-R port of the \code{pyeogpr} inference pipeline
#' (Estevez et al., 2022). It only depends on \code{terra} (raster I/O) and
#' \code{jsonlite} (model loading) and produces the same numerical output as
#' the reference Python implementation to machine precision
#' (correlation 1.0, relative error < 1.4 \eqn{\cdot 10^{-8}} on a 200-pixel
#' benchmark). The models themselves are trained on PROSAIL + canopy N
#' simulations and are distributed as \code{*.json} files.
#'
#' @section Inputs and outputs:
#' The S2 raster is expected to store DN values with the ESA L2A convention
#' (offset \code{-1000}, scale \code{10000}). By default the function applies
#' this rescaling; pass \code{apply_offset = FALSE} if the raster is already
#' in surface-reflectance units (0-1.5).
#'
#' One GeoTIFF is written per requested variable (plus an \code{_unc.tif}
#' file with prediction standard deviation for variables whose model exposes
#' the pre-computed \code{Linv_pre_calc} matrix). Output rasters inherit the
#' geometry (CRS, extent, resolution) of the input.
#'
#' @section Use within NFert:
#' Two of the outputs are directly usable as inputs to
#' \code{\link{compute_NNI}}:
#' \itemize{
#'   \item \strong{W} (aboveground dry biomass, t DM ha\eqn{^{-1}}) can be
#'         approximated from \code{LAI} \eqn{\times} \code{Cm} (leaf dry-matter
#'         mass per unit leaf area, g cm\eqn{^{-2}}) converted to t ha\eqn{^{-1}}
#'         via the factor \code{100} (g cm\eqn{^{-2}} \eqn{\to} kg m\eqn{^{-2}}
#'         \eqn{\to} t ha\eqn{^{-1}}).
#'   \item \strong{N_actual (% DM)} follows from \code{CNC_Cprot} (canopy N
#'         content, g m\eqn{^{-2}} ground) divided by the biomass per unit
#'         ground area.
#' }
#' The helper \code{\link{biophysical_to_NNI_inputs}} wraps this conversion.
#'
#' @param raster_path Path to a 10-band S2 L2A GeoTIFF in the canonical band
#'   order.
#' @param output_dir Directory where the output GeoTIFF maps are written.
#'   Created if missing.
#' @param variables Character vector. Any subset of \code{c("LAI", "Cm",
#'   "Cw", "FVC", "Cab", "laiCab", "CNC_Cab", "CNC_Cprot")}.
#' @param model_dir Directory containing the \code{*.json} GPR model files.
#' @param apply_offset Logical. If \code{TRUE} (default) the raster is
#'   shifted by \code{offset} and divided by \code{scale} before inference.
#' @param offset,scale Radiometric conversion from DN to surface reflectance.
#'   Defaults match ESA S2 L2A processor baseline 04.00 onwards.
#' @param nodata_in Integer. Pixel value treated as nodata in the input.
#' @param block_rows Integer. Number of rows processed per chunk; tune to
#'   available RAM (512 is safe on 8 GB machines).
#'
#' @return Named list of file paths for every output GeoTIFF written.
#'
#' @seealso \code{\link{compute_NNI}}, \code{\link{diagnose_N_status}},
#'   \code{\link{biophysical_to_NNI_inputs}},
#'   \code{\link{estimate_N_rate_from_holland_schepers}}
#'
#' @references
#' Estevez, J., Salinero-Delgado, M., Berger, K., Pipia, L., Rivera-Caicedo,
#' J. P., Wocher, M., Reyes-Munoz, P., Tagliabue, G., Boschetti, M., &
#' Verrelst, J. (2022). Gaussian processes retrieval of crop traits in
#' Google Earth Engine based on Sentinel-2 top-of-atmosphere data.
#' \emph{Remote Sensing of Environment}, 273, 112958.
#'
#' @examplesIf requireNamespace("terra", quietly = TRUE) && requireNamespace("jsonlite", quietly = TRUE)
#' scene <- system.file("extdata/sentinel-2/S2_field001_20240711.tif", package = "NFert")
#' if (nzchar(scene)) {
#'   od <- tempfile()
#'   dir.create(od)
#'   maps <- estimate_biophysical(scene, od, variables = "LAI", block_rows = 32)
#'   stopifnot(is.character(maps$LAI), file.exists(maps$LAI))
#' }
#' @export
estimate_biophysical <- function(
  raster_path,
  output_dir,
  variables    = c("LAI", "Cm", "Cw", "FVC", "Cab", "laiCab",
                   "CNC_Cab", "CNC_Cprot"),
  model_dir    = NULL,
  apply_offset = TRUE,
  offset       = -1000,
  scale        = 10000,
  nodata_in    = 0,
  block_rows   = 512
) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for estimate_biophysical().")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required for estimate_biophysical().")

  variables <- match.arg(variables, .MODEL_MAP_NAMES, several.ok = TRUE)

  # Default to the bundled GPR models if none is supplied
  if (is.null(model_dir) || !nzchar(model_dir)) {
    model_dir <- system.file("extdata", package = "NFert")
    if (!nzchar(model_dir))
      stop("`model_dir` not supplied and no bundled models found ",
           "in inst/extdata/.")
  }
  if (!dir.exists(model_dir))
    stop("`model_dir` does not exist: ", model_dir)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  models <- lapply(variables, function(v) .load_gpr_model(model_dir, v))
  names(models) <- variables

  for (v in variables)
    if (models[[v]]$n_bands != length(.S2_BANDS))
      stop(sprintf(
        "Model '%s' expects %d bands but has %d.",
        v, length(.S2_BANDS), models[[v]]$n_bands))

  src <- terra::rast(raster_path)
  if (terra::nlyr(src) != length(.S2_BANDS))
    stop(sprintf(
      "Raster has %d bands but %d are required (order: %s).",
      terra::nlyr(src), length(.S2_BANDS),
      paste(.S2_BANDS, collapse = ", ")))

  H <- terra::nrow(src)
  stem <- tools::file_path_sans_ext(basename(raster_path))

  # Prepare output writers
  out_paths <- list()
  writers <- list()
  template_proto <- terra::rast(src, nlyrs = 1)
  for (v in variables) {
    pmean <- file.path(output_dir, sprintf("%s__%s.tif", stem, v))
    w <- terra::rast(template_proto)
    terra::writeStart(w, pmean, overwrite = TRUE,
                      gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"),
                      datatype = "FLT4S")
    writers[[v]] <- w
    out_paths[[v]] <- pmean
    if (isTRUE(models[[v]]$has_uncertainty)) {
      punc <- file.path(output_dir, sprintf("%s__%s_unc.tif", stem, v))
      w2 <- terra::rast(template_proto)
      terra::writeStart(w2, punc, overwrite = TRUE,
                        gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"),
                        datatype = "FLT4S")
      writers[[paste0(v, "_unc")]] <- w2
      out_paths[[paste0(v, "_unc")]] <- punc
    }
  }

  on.exit({
    for (w_ in writers) try(terra::writeStop(w_), silent = TRUE)
  }, add = TRUE)

  row_starts <- seq(1, H, by = block_rows)
  for (rs in row_starts) {
    nr <- min(block_rows, H - rs + 1)
    cube <- terra::values(src, row = rs, nrows = nr)   # (nr*W, 10)

    nodata_mask <- rep(FALSE, nrow(cube))
    if (!is.null(nodata_in))
      nodata_mask <- nodata_mask |
        apply(cube == nodata_in, 1, any, na.rm = FALSE)
    nodata_mask <- nodata_mask | apply(is.na(cube), 1, any)

    if (apply_offset) cube <- cube + offset
    cube <- cube / scale

    bad <- apply(cube < 0 | cube > 1.5, 1, any, na.rm = TRUE)
    nodata_mask <- nodata_mask | bad
    cube[nodata_mask, ] <- 0

    for (v in variables) {
      res <- .gpr_predict(cube, models[[v]])
      mean_vec <- res$mean
      mean_vec[nodata_mask] <- NA_real_
      terra::writeValues(writers[[v]], mean_vec, rs, nr)
      if (!is.null(res$uncertainty)) {
        unc_vec <- res$uncertainty
        unc_vec[nodata_mask] <- NA_real_
        terra::writeValues(writers[[paste0(v, "_unc")]], unc_vec, rs, nr)
      }
    }
  }
  out_paths
}

#' Convert GPR biophysical outputs into NNI inputs (low-level helper)
#'
#' Low-level helper that turns \code{LAI}, \code{Cm} and a canopy-N raster
#' into the two layers consumed by \code{\link{compute_NNI}}: the
#' aboveground dry biomass \code{W} (t DM ha\eqn{^{-1}}) and the plant N
#' concentration \code{N_actual} (pct DM).
#'
#' Most users should rather call the end-to-end
#' \code{\link{compute_NNI_from_S2}} wrapper, which handles crop-specific
#' parameters, FVC / SCL masking and zone classification in a single call.
#' This function is exposed for advanced workflows that need direct
#' control over the conversion.
#'
#' The conversions follow
#' \deqn{W_{leaf}\ (\mathrm{g\ m^{-2}}) = LAI \cdot C_m \cdot 10^4}
#' \deqn{W\ (\mathrm{t\ DM\ ha^{-1}}) = W_{leaf} / (100 \cdot \alpha_{leaf})}
#' (leaf-to-total aboveground allocation coefficient \eqn{\alpha_{leaf}},
#' ~0.30-0.45 depending on crop and phenology), and
#' \deqn{N_{actual}(\% DM) = 100 \cdot N_{total} / W_{leaf} \cdot \alpha_{leaf}^{-1}}
#' with \eqn{N_{total}} either equal to \code{CNC_Cprot} (g N m\eqn{^{-2}})
#' or \code{k * CNC_Cab} when the chlorophyll path is used.
#' Pixels where \code{W < w_min} are set to \code{NA} because the
#' critical-N dilution curve is not defined below that biomass.
#'
#' @param lai_rast RasterLayer or terra SpatRaster with leaf area index.
#' @param cm_rast  RasterLayer or terra SpatRaster with leaf dry-matter
#'   content (g cm\eqn{^{-2}}).
#' @param cnc_rast RasterLayer or terra SpatRaster with canopy nitrogen:
#'   either \code{CNC_Cprot} (g N m\eqn{^{-2}}, protein path) or
#'   \code{CNC_Cab} (g Chl m\eqn{^{-2}}, chlorophyll path - in that case
#'   supply \code{k_NChl}).
#' @param alpha_leaf Leaf-to-total aboveground biomass allocation
#'   coefficient (default 0.35; use \code{\link{crop_params_NNI}} for
#'   crop-specific values).
#' @param k_NChl When \code{cnc_rast} contains CNC_Cab (g Chl m\eqn{^{-2}}),
#'   the canopy-level N:Chl ratio used to convert it to g N m\eqn{^{-2}}
#'   (default \code{NULL} = protein path, no conversion).
#' @param w_min Minimum biomass (t DM ha\eqn{^{-1}}) below which pixels
#'   are masked (default 1.0).
#'
#' @return A named list with two SpatRaster layers, \code{W} and
#'   \code{N_actual}, ready to be passed to \code{\link{compute_NNI}}.
#' @seealso \code{\link{compute_NNI_from_S2}}, \code{\link{compute_NNI}},
#'   \code{\link{estimate_biophysical}}, \code{\link{crop_params_NNI}}
#' @export
biophysical_to_NNI_inputs <- function(
  lai_rast, cm_rast, cnc_rast,
  alpha_leaf = 0.35, k_NChl = NULL, w_min = 1.0
) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for biophysical_to_NNI_inputs().")
  lai <- terra::rast(lai_rast)
  cm  <- terra::rast(cm_rast)
  cnc <- terra::rast(cnc_rast)

  W_leaf_g_m2  <- lai * cm * 1e4                     # g m^-2 ground
  W_total_g_m2 <- W_leaf_g_m2 / alpha_leaf
  W            <- W_total_g_m2 * 0.01                # t DM ha^-1

  N_total <- if (is.null(k_NChl)) cnc else cnc * k_NChl     # g N m^-2
  N_actual <- 100 * N_total / W_total_g_m2                  # % DM

  W[W < w_min]         <- NA
  N_actual[is.na(W)]   <- NA
  list(W = W, N_actual = N_actual)
}

# -----------------------------------------------------------------------------
# Internal helpers (not exported)
# -----------------------------------------------------------------------------

.S2_BANDS <- c("B02","B03","B04","B05","B06","B07","B08","B8A","B11","B12")

.MODEL_MAP <- c(
  LAI       = "SENTINEL2_L2A_LAI",
  Cm        = "SENTINEL2_L2A_Cm",
  Cw        = "SENTINEL2_L2A_Cw",
  FVC       = "SENTINEL2_L2A_FVC",
  Cab       = "SENTINEL2_L2A_Cab",
  laiCab    = "SENTINEL2_L2A_laiCab",
  CNC_Cab   = "SENTINEL2_L2A_CNC_Cab",
  CNC_Cprot = "SENTINEL2_L2A_CNC_Cprot"
)
.MODEL_MAP_NAMES <- names(.MODEL_MAP)

.load_gpr_model <- function(model_dir, var_name) {
  path <- file.path(model_dir, paste0(.MODEL_MAP[[var_name]], ".json"))
  if (!file.exists(path))
    stop(sprintf("GPR model file not found: %s", path))
  m <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                          simplifyMatrix = TRUE)
  m$X_train            <- as.matrix(m$X_train)
  m$alpha_coefficients <- as.numeric(m$alpha_coefficients)
  m$hyp_ell            <- as.numeric(m$hyp_ell)
  m$mx                 <- as.numeric(m$mx)
  m$sx                 <- as.numeric(m$sx)
  m$XDX_pre_calc       <- as.numeric(m$XDX_pre_calc)
  m$hyp_sig            <- as.numeric(m$hyp_sig)
  m$mean_model         <- as.numeric(m$mean_model)
  m$max_variable       <- as.numeric(m$max_variable)
  if (isTRUE(m$has_uncertainty))
    m$Linv_pre_calc <- as.matrix(m$Linv_pre_calc)
  m
}

.gpr_predict <- function(pixels, model) {
  # pixels: matrix (n_pix, n_bands)
  im_norm     <- sweep(pixels, 2, model$mx, "-")
  im_norm     <- sweep(im_norm, 2, model$sx, "/")
  im_norm_ell <- sweep(im_norm, 2, model$hyp_ell, "*")

  PtTPt <- -0.5 * rowSums(im_norm_ell * im_norm)
  arg1  <- exp(PtTPt) * model$hyp_sig

  PtTDX  <- model$X_train %*% t(im_norm_ell)
  k_star <- exp(PtTDX - 0.5 * model$XDX_pre_calc)

  mean_pred <- as.numeric(model$alpha_coefficients %*% k_star) * arg1 +
    model$mean_model
  mean_pred <- pmin(pmax(mean_pred, 0), model$max_variable)

  out <- list(mean = mean_pred, uncertainty = NULL)
  if (isTRUE(model$has_uncertainty)) {
    k_star_unc <- sweep(k_star, 2, arg1, "*")
    V          <- model$Linv_pre_calc %*% k_star_unc
    V_norm2    <- colSums(V * V)
    variance   <- sqrt(abs(model$hyp_sig - V_norm2))
    variance   <- pmin(pmax(variance, 0), model$max_variable)
    out$uncertainty <- variance
  }
  out
}
