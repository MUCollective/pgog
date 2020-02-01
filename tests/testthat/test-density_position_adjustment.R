library(ggplot2)


common_bw <- 1.5
context("density_position_adjustment")

test_that("equiv to using position fill", {

  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  pgog_fill <-
    ggplot(mtcars) +
    geom_bloc(aes(
      x = c(mpg),
      height = c(P(cyl | mpg)),
      fill = factor(cyl)),
      bw = common_bw)
  vdiffr::expect_doppelganger("pgog-fill-density", pgog_fill)


  pgog_stacked <-
    ggplot(mtcars) +
    geom_bloc(aes(
      x = c(mpg),
      height = c(P(cyl|mpg), P(mpg)),
      fill = factor(cyl)), bw = common_bw)

  vdiffr::expect_doppelganger("pgog-stacked-density", pgog_stacked)
})
