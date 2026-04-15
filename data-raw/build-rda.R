# ----------------------------------------------------------------------------
# data-raw/build-rda.R
#
# Rebuild all NFert datasets from the canonical CSVs in data-raw/csv/ using
# R's native serialisation. Use this whenever the binary .rda files become
# unreadable (e.g. after a Python-based extraction with pyreadr) or when the
# CSV sources are updated.
#
# Usage (one-off, from RStudio with the package as project root):
#
#   source("data-raw/build-rda.R")
#
# This will overwrite all .rda files under data/.
# ----------------------------------------------------------------------------

stopifnot(file.exists("DESCRIPTION"))   # run from package root
csv_dir  <- "data-raw/csv"
data_dir <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir)

# --- helper -----------------------------------------------------------------
# Read a CSV with proper UTF-8 + handle "True"/"False" and integer columns.
read_table_csv <- function(name) {
  fp <- file.path(csv_dir, paste0(name, ".csv"))
  if (!file.exists(fp)) stop(sprintf("Missing CSV: %s", fp))
  df <- utils::read.csv(fp, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                        check.names = FALSE, na.strings = c("", "NA", "nan"))
  # Convert "True"/"False" string columns to logical
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.character(v) && all(v %in% c("True","False","TRUE","FALSE", NA))) {
      df[[j]] <- toupper(v) == "TRUE"
    }
  }
  df
}

# Save a data.frame as a .rda with the desired variable name in data/ ----
save_as_rda <- function(df, name) {
  out <- file.path(data_dir, paste0(name, ".rda"))
  assign(name, df, envir = environment())
  save(list = name, file = out, compress = "xz", version = 2)
  message(sprintf("  wrote %s  (%d x %d)", out, nrow(df), ncol(df)))
}

# --- list of datasets to rebuild --------------------------------------------
datasets <- c(
  "uptake_table",
  "mas.table",
  "crops.table",
  "gruppo.table",
  "e.table",
  "f.table",
  "g.table",
  "coef_time",
  "ragg_tes.table",
  "so.table",
  "so_max_input",
  "ph.table",
  "calcare_tot.table",
  "calcare_att.table",
  "gri_p.table",
  "gri_p_meta",
  "gri_k.table",
  "tipo_fert.table",
  "fert_org.table",
  "efficienza.table",
  "mod_distribuz.table",
  "ciclo_modalita.table",
  "cicli.table",
  "cicli_fase.table",
  "cd.table",
  "standard_pk_doses.table",
  "standard_decrements.table",
  "standard_increments.table",
  "standard_multicycle.table",
  "concimi.table"
)

message("Rebuilding ", length(datasets), " datasets ...")
for (nm in datasets) {
  df <- read_table_csv(nm)
  save_as_rda(df, nm)
}
message("Done. Now run:  devtools::document() ; devtools::install()")
