
#' Formula generator
#'
#' Writes a formula as a string.
#'
#' @param predictors A vector of predictor variables as strings
#' @param response A response variable as a string
#' @return a formula of the form \code{y ~ x} as string
#' @examples
#' \dontrun{formula_generator(predictors, 'Cluster')}

formula_generator = function(predictors, response) {
  paste0(response, " ~ ", paste0(predictors, collapse = " + "))
}

#' Combinations and formulas
#'
#' Writes formulas for different combinations of vectors and stores
#' them into a list. See also \code{\link{formula_generator}}.
#'
#' @param n The number of predictors to be taken at a time,
#' @inheritParams formula_generator
#' @return a list of formulas of the form \code{y ~ x} as strings

combine_to_formula = function(n, predictors, response) {
  requireNamespace("rlang")

  response = rlang::enexpr(response)
  response = rlang::expr_text(response)

  cmbn = t(combn(predictors, n))
  expr = apply(cmbn, 1, formula_generator, response)

  return(expr)
}
