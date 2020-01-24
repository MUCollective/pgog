library(ggplot2)

common_bw <- 1.5

# note: base ggplot doesn't seem to set the same bandwith for different
# data `group`s

test_that("P(A,B), equiv to using position=stack", {
  base_ggplot <- ggplot(mtcars) +
    geom_density(aes(x = mpg, y = ..count..,
                     fill = factor(cyl)), position = "stack",
                 bw = common_bw)
  vdiffr::expect_doppelganger("ggplot2 stacked density", base_ggplot)

  pgog_stacked <-
    ggplot(mtcars) +
    geom_bloc(aes(
      x = c(mpg),
      height = c(P(cyl|mpg), P(mpg)),
      fill = factor(cyl)), bw = common_bw)

  vdiffr::expect_doppelganger("pgog stacked density", pgog_stacked)
})
