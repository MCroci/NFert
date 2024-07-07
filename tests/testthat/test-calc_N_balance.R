test_that("multiplication works", {
  expect_equal(N_balance(
         location = "Plain adjacent to urbanized areas",
         expected_yield_tons_ha = 6, crop = "Grano tenero FF (granella)",
         ccp = "Autumn-winter crop <150 days",
         clay = 10, sand = 40,
         Ntot = 1.2, SOM = 2, CN = 8,
         oxygen_availability = "Normal",
         winter_rain = 160,
         start_spring_rain = 160,
         prev_crop = "Winter cereals straw removal",
         source = "Cattle slurry",
         fertorg_frequency = "every year",
         forg_quantity = 100
     )$A.N_requirement, 144)
})
