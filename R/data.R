#' NFert Package Data
#'
#' Internal datasets used by the NFert package for nitrogen balance calculations
#' following Emilia-Romagna Regional Recommendations.
#'
#' @name NFert-data
#' @aliases uptake_table e.table f.table g.table soil.table tri2.table tri3.table 
#'          ca.table cb.table coefN_readily coefN_mineralised coef_time s2.rast
#'
#' @details
#' These datasets are loaded automatically with the package using `LazyData: true` 
#' in the DESCRIPTION file. They contain lookup tables and coefficients used 
#' throughout the nitrogen balance calculations.
#'
#' @section Available Datasets:
#' \describe{
#'   \item{uptake_table}{Table containing unit crop nitrogen absorption values 
#'         (kg N/t yield) for different crops. Used by `calc_crop_N_demand()`.}
#'   \item{e.table}{Table with nitrogen values (kg/ha) available from previous 
#'         crop residues. Used by `nitrogen_from_previous_crop_residues()`.}
#'   \item{f.table}{Table containing nitrogen content and application factors 
#'         for organic fertilizers by source and frequency. Used by 
#'         `organic_fertilization()`.}
#'   \item{g.table}{Table with annual nitrogen deposition rates (kg/ha/year) 
#'         for different locations. Used by `natural_contribution()`.}
#'   \item{soil.table}{Table for determining soil group and drainage index 
#'         (ID_Rag) based on soil texture. Used by `calc_soil_group_and_id_rag()`.}
#'   \item{tri2.table}{Matrix for detailed soil texture classification using 
#'         USDA triangle method. Returns ID_Rag values. Used by `tri2()`.}
#'   \item{tri3.table}{Matrix for simplified soil texture classification using 
#'         USDA triangle method. Returns simplified texture classes. Used by `tri3()`.}
#'   \item{ca.table}{Table for determining ID_Dre (drainage easiness index) 
#'         based on oxygen availability. Used in leaching and immobilization 
#'         loss calculations.}
#'   \item{cb.table}{Table containing leaching coefficients (C) based on 
#'         ID_Rag and ID_Dre. Used in leaching loss calculations.}
#'   \item{coefN_readily}{Table with coefficients for calculating readily 
#'         available nitrogen (b1) from total soil nitrogen by soil group. 
#'         Used by `soil_fertility()`.}
#'   \item{coefN_mineralised}{Table with coefficients for calculating 
#'         mineralizable nitrogen (b2) from soil organic matter by soil group 
#'         and C/N ratio. Used by `soil_fertility()`.}
#'   \item{coef_time}{Table with time adjustment factors (C_tempo) for 
#'         different crop categories and planting periods. Used in natural 
#'         contribution and soil fertility calculations.}
#'   \item{s2.rast}{Example Sentinel-2 NDVI raster data for demonstration 
#'         purposes. Used in vignettes and examples.}
#' }
#'
#' @examples
#' # Access datasets
#' head(NFert::uptake_table)
#' head(NFert::e.table)
#' head(NFert::f.table)
#' head(NFert::g.table)
#' head(NFert::soil.table)
#'
#' @keywords datasets internal
NULL
