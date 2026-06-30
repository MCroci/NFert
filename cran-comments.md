# CRAN submission comments — NFert 0.14.0

This is a new submission.

## Test environments

- Local: Windows 11, R 4.5.1
- (Before submission, also run win-builder (release and devel) and R-hub.)

## R CMD check results

`R CMD check --as-cran` on the source tarball returns:

    0 errors | 0 warnings | 1 note

The single NOTE is the standard new-submission note:

    * checking CRAN incoming feasibility ... NOTE
      Maintainer: 'Michele Croci <michele.croci@unicatt.it>'
      New submission

There is also an informational message about installed size:

    * checking installed package size ... INFO
      installed size is 5.8Mb
      sub-directories of 1Mb or more: extdata 3.9Mb

`inst/extdata` ships small example datasets needed to run the documented
workflows out of the box: a cropped Sentinel-2 scene and field boundary for
the CERZOO case study, six soil-property GeoTIFFs for the spatial balance, and
the pre-trained Gaussian-process JSON models used by `estimate_biophysical()`.

## Notes on the package

- The bundled lookup tables encode Italian crop and growth-stage names from the
  official Emilia-Romagna guidelines. All bundled data is ASCII; the few
  non-ASCII source labels (degree sign, dashes, accented vowels) are folded to
  ASCII, and lookups are accent-insensitive, so Italian/accented user input
  still resolves to the canonical row.
- The only hard dependency is `terra`. All other functionality (sf, jsonlite,
  Rquefts, shiny, ggplot2) is optional and loaded lazily from Suggests.

## Reverse dependencies

None (new submission).
