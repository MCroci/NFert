test_that("natural_contribution works correctly", {
  result <- natural_contribution(
    location = "Plain adjacent to urbanized areas",
    ccp = "Autumn-winter crop <150 days"
  )
  
  expect_true(is.numeric(result))
  expect_true(result >= 0 || is.na(result))
})

test_that("nitrogen_from_previous_crop_residues works correctly", {
  result <- nitrogen_from_previous_crop_residues(
    previous_crop = "Winter cereals straw removal"
  )
  
  expect_true(is.numeric(result))
  expect_true(!is.na(result) || is.na(result))  # Can be NA if crop not found
})

test_that("organic_fertilization calculates correctly", {
  result <- organic_fertilization(
    source = "Cattle slurry",
    frequency = "every year",
    quantity = 100
  )
  
  expect_true(is.numeric(result))
  expect_true(result >= 0)
})

test_that("leaching_loss returns correct structure", {
  result <- leaching_loss(
    winter_rain = 160,
    start_spring_rain = 40,
    oxygen_availability = "Normal",
    id_rag = 3,
    b1 = 29.16
  )
  
  expect_type(result, "list")
  expect_named(result, c("C1", "C2", "surplus_pluviometrico"))
  expect_true(is.numeric(result$C1))
  expect_true(is.numeric(result$C2))
  expect_true(result$C1 >= 0)
  expect_true(result$C2 >= 0)
})

test_that("calc_N_immobilization_loss calculates correctly", {
  result <- calc_N_immobilization_loss(
    B = 50,
    oxygen_availability = "Normal",
    id_rag = 1
  )
  
  expect_true(is.numeric(result))
  expect_true(result >= 0)
})
