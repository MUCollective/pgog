library(vdiffr)
#control shift P for Qiang use(ignore)
test_that("color,fill,alpha doesn't change", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  pgog_fill <-
    ggplot(mtcars) +
    geom_bloc(aes(
      x = c(mpg),
      height = c(P(cyl | mpg)),
      fill = factor(mpg)))
  vdiffr::expect_doppelganger("pgog-fill", pgog_fill)


  pgog_stacked <-
    ggplot(mtcars) +
    geom_bloc(aes(
      x = c(mpg),
      height = c(P(cyl|mpg), P(mpg)),
      color = factor(cyl)))

  vdiffr::expect_doppelganger("pgog-color", pgog_stacked)

  pgog_stacked <-
    ggplot(mtcars) +
    geom_bloc(aes(
      x = c(mpg),
      height = c(P(cyl|mpg), P(mpg)),
      alpha = factor(cyl)))

  vdiffr::expect_doppelganger("pgog-alpha", pgog_stacked)

  pgog_multiple_variable1 <- ggplot(mtcars) + geom_bloc(aes(height = c(P(cyl|gear)),
                                 x = c(gear), fill = factor(cyl), alpha = factor(cyl)))
  vdiffr::expect_doppelganger("pgog-alpha-fill1", pgog_multiple_variable1)

  pgog_multiple_variable2 <- ggplot(mtcars) + geom_bloc(aes(height = c(P(cyl|gear)),
                                 x = c(gear), fill = factor(gear), alpha = factor(cyl)))
  vdiffr::expect_doppelganger("pgog-alpha-fill2", pgog_multiple_variable1)
})

