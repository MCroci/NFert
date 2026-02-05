test_that("calc_crop_N_demand returns correct structure", {
  result <- calc_crop_N_demand(
    expected_yield_tons_ha = 10,
    crop = "Grano tenero FF (granella)"
  )
  
  expect_type(result, "list")
  expect_named(result, c("N_requirement", "units"))
  expect_true(is.numeric(result$N_requirement))
  expect_equal(result$units, "kg/ha")
  expect_true(result$N_requirement > 0)
})

test_that("calc_crop_N_demand validates input", {
  expect_error(
    calc_crop_N_demand(expected_yield_tons_ha = -5),
    "Expected yield must be a positive value"
  )
  
  expect_error(
    calc_crop_N_demand(crop = "Invalid Crop"),
    "not found in the uptake table"
  )
})

test_that("calc_crop_N_demand calculates correctly", {
  result <- calc_crop_N_demand(expected_yield_tons_ha = 10, crop = "Shredded corn class 700")
  # N_requirement should be positive
  expect_true(result$N_requirement > 0)
})
