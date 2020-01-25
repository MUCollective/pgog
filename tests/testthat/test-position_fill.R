library(ggplot2)


common_bw <- 1.5

test_that("P(A,B), equiv to using position=fill", {

  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")


  base_ggplot <- ggplot(mtcars) + geom_density(aes(x = mpg, fill = factor(cyl)), position = "fill", bw=common_bw)
  vdiffr::expect_doppelganger("ggplot2 fill", base_ggplot)

  pgog_fill <-
    ggplot(mtcars) +
    geom_bloc(aes(x = c(mpg),
                  height = c(P(cyl | mpg)),
                  fill = factor(cyl)), bw = common_bw)
  vdiffr::expect_doppelganger("pgog fill density", pgog_fill)
})
