# Installing Rtools for NFert Package

## Problem
Rtools is required to build and check R packages on Windows, even for pure R packages like NFert.

## Solution: Install Rtools

### Step 1: Download Rtools
For R 4.5.1, download Rtools from:
**https://cran.r-project.org/bin/windows/Rtools/rtools45.html**

Or use the direct link for R 4.5.x:
**https://cran.r-project.org/bin/windows/Rtools/rtools45.exe**

### Step 2: Install Rtools
1. Run the installer (rtools45.exe)
2. Accept the default installation path: `C:\rtools45`
3. During installation, make sure to check:
   - ✅ "Add rtools to system PATH" (if available)
   - ✅ All components

### Step 3: Verify Installation
After installation, restart R/RStudio and run:

```r
# Check if Rtools is found
pkgbuild::check_build_tools(debug = TRUE)

# Or check manually
Sys.which("make")
```

If `make` is found, Rtools is correctly installed.

### Step 4: Configure PATH (if needed)
If Rtools is not automatically found, you may need to add it to your PATH:

1. Add `C:\rtools45\usr\bin` to your system PATH
2. Restart R/RStudio

Or in R:

```r
# Add to PATH temporarily
Sys.setenv(PATH = paste("C:/rtools45/usr/bin", Sys.getenv("PATH"), sep = ";"))
```

### Step 5: Test Package Build
After Rtools installation:

```r
# Check tools
pkgbuild::check_build_tools()

# Build package (should work now)
devtools::check()
```

## Alternative: Building Without Rtools

If you cannot install Rtools immediately, you can still work with the package:

### Option 1: Use RStudio Cloud or GitHub Actions
These environments have Rtools pre-installed.

### Option 2: Use Docker
Use an R Docker container with Rtools pre-installed.

### Option 3: Minimal Package Checks
Some checks can be done without Rtools:

```r
# Check code syntax (no Rtools needed)
devtools::load_all()

# Run tests (no Rtools needed if testthat is installed)
devtools::test()

# Document package (no Rtools needed)
devtools::document()
```

However, **full package check and build requires Rtools**.

## Notes

- **NFert is a pure R package** (no C/C++/Fortran code), but Rtools is still needed for:
  - Building vignettes
  - Creating package tarballs (.tar.gz)
  - Running `devtools::check()`
  - Some CRAN submission checks

- **For CRAN submission**, Rtools is absolutely necessary.

## Troubleshooting

If Rtools is installed but not found:

1. **Check PATH**: Make sure `C:\rtools45\usr\bin` is in PATH
2. **Restart R/RStudio**: PATH changes require restart
3. **Check installation**: Verify Rtools is installed in `C:\rtools45`
4. **Version compatibility**: Use Rtools 4.5 for R 4.5.1

## Verification Command

Run this to verify everything works:

```r
# Should return TRUE if everything is OK
pkgbuild::check_build_tools()
```
