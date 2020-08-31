
test_that("formula_generator correctly writes formulas from strings", {
  requireNamespace("ggplot2", quietly = T)
  pred = names(diamonds)[names(diamonds) != "price"]
  response = "price"
  str_formula = "price ~ carat + cut + color + clarity + depth + table + x + y + z"
  fun_formula = formula_generator(pred, response)
  expect_equal(formula_generator(pred, response), str_formula)
  expect_that(fun_formula, is_a("character"))
})
test_that("combine_to_formula produces correct number of formulas", {
  requireNamespace("ggplot2", quietly = T)
  pred = names(diamonds)[names(diamonds) != "price"]
  response = "price"
  formula_list = combine_to_formula(3, pred, response)
  expect_that(formula_list, is_a("character"))
  expect_equal(length(formula_list), 84)
})
