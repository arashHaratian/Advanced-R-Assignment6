suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(din <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3700))
  expect_named(din, c("value", "elements"))
})


test_that("Function fails with wrong arguments", {
  expect_error(knapsack_dynamic("knapsack_objects", 3700))
  expect_error(knapsack_dynamic(knapsack_objects[1:12,], "3700"))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:12,], W = -3700))
})

test_that("Function WORKS", {
  din <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(din$value), 15428)
  expect_true(all(round(din$elements) %in% c(3, 8)))
  
  din <- knapsack_dynamic(x = knapsack_objects[1:15,], W = 3700)
  expect_equal(round(din$value), 22505)
  expect_true(all(round(din$elements) %in% c(6, 8, 14)))
  
  din <- knapsack_dynamic(x = knapsack_objects[1:17,], W = 3700)
  expect_equal(round(din$value), 22505)
  expect_true(all(round(din$elements) %in% c(6, 8, 14)))
  
  din <- knapsack_dynamic(x = knapsack_objects[1:20,], W = 4200)
  expect_equal(round(din$value), 29973)
  expect_true(all(round(din$elements) %in% c(3, 8, 18, 19)))
  
  din <- knapsack_dynamic(x = knapsack_objects[1:25,], W = 4200)
  expect_equal(round(din$value), 42169)
  expect_true(all(round(din$elements) %in% c(3, 8, 14, 18, 22, 25)))
  
  st <- system.time(din <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})