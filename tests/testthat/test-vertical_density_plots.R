library(ggplot2)

context("vertical_density_plots")

test_that("x", {
  p <- ggplot(mtcars) + geom_bloc(aes(x = c(mpg),
                                      height = c(P(mpg | cyl)),
                                      y = c(cyl), fill = cyl))

  vdiffr::expect_doppelganger("Vertical density plot", p)

})
