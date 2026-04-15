# ----------------------------------------------------------------------------
# data-raw/build-rda.R
#
# Rebuild all NFert datasets from the canonical CSVs in data-raw/csv/ using
# R's native serialisation (NFert >= 0.4.0, all-English schema).
#
# Usage (one-off, from RStudio with the package as project root):
#
#   source("data-raw/build-rda.R")
#
# This will overwrite the .rda files under data/.
# ----------------------------------------------------------------------------

stopifnot(file.exists("DESCRIPTION"))
csv_dir  <- "data-raw/csv"
data_dir <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir)

read_table_csv <- function(name) {
  fp <- file.path(csv_dir, paste0(name, ".csv"))
  if (!file.exists(fp)) stop(sprintf("Missing CSV: %s", fp))
  df <- utils::read.csv(fp, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                        check.names = FALSE, na.strings = c("", "NA", "nan"))
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.character(v) && all(v %in% c("True","False","TRUE","FALSE", NA))) {
      df[[j]] <- toupper(v) == "TRUE"
    }
  }
  df
}

save_as_rda <- function(df, name) {
  out <- file.path(data_dir, paste0(name, ".rda"))
  assign(name, df, envir = environment())
  save(list = name, file = out, compress = "xz", version = 2)
  message(sprintf("  wrote %s  (%d x %d)", out, nrow(df), ncol(df)))
}

datasets <- c(
  # Core balance tables
  "uptake_table",
  "mas.table",
  "crops.table",
  "crop_groups.table",
  "e.table",
  "f.table",
  "g.table",
  "coef_time",
  # Soil
  "texture_groups.table",
  "so.table",
  "so_max_input",
  "ph.table",
  "total_carbonate.table",
  "active_carbonate.table",
  # PK availability
  "p_availability.table",
  "p_availability_meta",
  "k_availability.table",
  # Fertilisers
  "fertilizer_types.table",
  "organic_fertilizers.table",
  "efficiency.table",
  "mineral_fertilizers.table",
  # Distribution
  "distribution_modalities.table",
  "cycle_modality.table",
  # Cycles
  "cycles.table",
  "cycle_phases.table",
  # Loss
  "c_d.table",
  # Standard scheda
  "standard_pk_doses.table",
  "standard_decrements.table",
  "standard_increments.table",
  "standard_multicycle.table"
)

message("Rebuilding ", length(datasets), " datasets ...")
for (nm in datasets) {
  df <- read_table_csv(nm)
  save_as_rda(df, nm)
}
message("Done. Now run: devtools::document(); devtools::install()")
