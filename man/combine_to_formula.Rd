% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula_functions.R
\name{combine_to_formula}
\alias{combine_to_formula}
\title{Combinations and formulas}
\usage{
combine_to_formula(n, predictors, response)
}
\arguments{
\item{n}{The number of predictors to be taken at a time,}

\item{predictors}{A vector of predictor variables as strings}

\item{response}{A response variable as a string}
}
\value{
a list of formulas of the form \code{y ~ x} as strings
}
\description{
Writes formulas for different combinations of vectors and stores
them into a list. See also \code{\link{formula_generator}}.
}
