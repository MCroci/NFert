# Bundled data must be ASCII (CRAN), and accent-insensitive matching must
# keep Italian/accented user input resolving to the canonical row.

test_that("no bundled dataset contains non-ASCII characters", {
  data_files <- list.files(system.file("..", package = "NFert", mustWork = FALSE),
                           pattern = "\\.rda$", full.names = TRUE)
  # When testing the source tree, fall back to the data/ directory.
  if (!length(data_files) && dir.exists("data"))
    data_files <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
  skip_if(!length(data_files), "no .rda data files found in this layout")
  bad <- character()
  for (f in data_files) {
    ev <- new.env(); load(f, envir = ev)
    for (nm in ls(ev)) {
      obj <- ev[[nm]]
      cols <- if (is.data.frame(obj)) obj else list(obj)
      for (col in cols) {
        x <- if (is.factor(col)) levels(col) else col
        if (is.character(x) && any(grepl("[^ -~]", x))) bad <- c(bad, basename(f))
      }
    }
  }
  expect_identical(unique(bad), character())
})

test_that("accent-insensitive matching keeps accented input resolving", {
  fold <- NFert:::.nfert_ascii_fold
  # Build the accented inputs from code points so this test file stays ASCII.
  deg  <- intToUtf8(0xB0L)    # degree sign
  ugr  <- intToUtf8(0xF9L)    # u-grave
  dash <- intToUtf8(0x2013L)  # en-dash
  expect_equal(fold(paste0("1", deg, " taglio")), "1o taglio")
  expect_equal(fold(paste0("pi", ugr, " / 70", dash, "100")), "piu / 70-100")
  # resolve_crop still maps a legacy Italian crop name to the English canonical
  expect_equal(resolve_crop("Mais trinciato classe 700"), "Silage maize (class 700)")
})
