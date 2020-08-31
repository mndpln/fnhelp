
library(ggplot2)
library(vdiffr)

test_that("gg.category produces correct plots", {
  requireNamespace("ggplot2", quietly = T)
  box_out = gg.category(diamonds, boxplot, clarity, price)
  hist_out = gg.category(diamonds, histogram, clarity, price)
  vdiffr::expect_doppelganger("boxplot from function", box_out)
  vdiffr::expect_doppelganger("histogram from function", hist_out)
})
