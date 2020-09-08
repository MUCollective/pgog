library(ggplot2)
library(vdiffr)


context("vertical_density_plots")

test_that("test 1", {
  p <- ggplot(mtcars) + geom_bloc(aes(x = c(mpg),
                                      height = c(P(mpg | cyl)),
                                      y = c(cyl), fill = cyl))
  vdiffr::expect_doppelganger("test 1", p)
})

test_that("test 2", {
  p <- ggplot(mtcars) + geom_bloc(aes(y = c(mpg), width = c(P(mpg | cyl)), x = c(cyl), fill = cyl))
  vdiffr::expect_doppelganger("test 2", p)
})

test_that("test 3", {
  p <- ggplot(mtcars) +  geom_bloc(aes(x = c(mpg), height = c(P(mpg))))
  vdiffr::expect_doppelganger("test 3", p)
})

test_that("test 4", {
  p <- ggplot(mtcars) +  geom_bloc(aes(y = c(mpg), width = c(P(mpg))))
  vdiffr::expect_doppelganger("test 4", p)
})

test_that("test 5", {
  p <- ggplot(mtcars) + geom_bloc(aes(x = c(mpg,cyl), y = c(gear), fill=factor(cyl), height = c(P(mpg | gear, cyl))))
  vdiffr::expect_doppelganger("test 5", p)
})

test_that("test 6", {
  p <- ggplot(mtcars) + geom_bloc(aes(y = c(mpg,cyl), x = c(gear), fill=factor(cyl), width = c(P(mpg | gear, cyl))))
  vdiffr::expect_doppelganger("test 6", p)
})
