context("test-dw")
library(rlang)
library(tidyverse)
library(vdiffr)

test_that("plots have known output", {
  # disp_hist_base <- function() hist(mtcars$disp)
  # expect_doppelganger("disp-histogram-base", disp_hist_base)

  # disp_hist_ggplot <- ggplot(mtcars, aes(disp)) + geom_histogram()
  # expect_doppelganger("disp-histogram-ggplot", disp_hist_ggplot)

  mpg_plot <- ggplot(mtcars) + geom_bloc(aes(x = c(mpg), height = c(P(mpg | cyl)), y = c(cyl), fill = cyl))
  expect_doppelganger("mpg_plot", mpg_plot)
})
