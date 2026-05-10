test_that("Spell-check documentation (skip on CI to avoid fragile WORDLIST drift)", {
  skip_on_ci()
  skip_if_not_installed("spelling")
  spelling::spell_check_test(vignettes = TRUE, lang = "en-US")
})
