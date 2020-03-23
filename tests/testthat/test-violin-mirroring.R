library(vdiffr)
library(ggplot2)
#control shift P for Qiang use(ignore)
test_that("testing mirroring violin plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  pgog_single_none = ggplot(mtcars) +  geom_bloc(aes(y = c(mpg), width = c(P(mpg))))
  vdiffr::expect_doppelganger("pgog-single-none", pgog_single_none)

  pgog_single_both = ggplot(mtcars) +  geom_bloc(aes(y = c(mpg), width = c(P(mpg))), side = "both")
  vdiffr::expect_doppelganger("pgog-single-both", pgog_single_both)

  pgog_single_up = ggplot(mtcars) +  geom_bloc(aes(y = c(mpg), width = c(P(mpg))),side = "up")
  vdiffr::expect_doppelganger("pgog-single-up", pgog_single_up)

  pgog_single_down = ggplot(mtcars) +  geom_bloc(aes(y = c(mpg), width = c(P(mpg))), side = "down")
  vdiffr::expect_doppelganger("pgog-single-down", pgog_single_down)

  pgog_multiple_none = ggplot(mtcars) + geom_bloc(aes(x = c(mpg,cyl), y = c(gear), fill=(cyl), height = c(P(mpg | gear, cyl))))
  vdiffr::expect_doppelganger("pgog-multiple-none", pgog_multiple_none)

  pgog_multiple_up = ggplot(mtcars) + geom_bloc(aes(x = c(mpg,cyl), y = c(gear), fill=(cyl), height = c(P(mpg | gear, cyl))),side = "up")
  vdiffr::expect_doppelganger("pgog-multiple-up", pgog_multiple_up)

  pgog_multiple_down = ggplot(mtcars) + geom_bloc(aes(x = c(mpg,cyl), y = c(gear), fill=(cyl), height = c(P(mpg | gear, cyl))),side = "down")
  vdiffr::expect_doppelganger("pgog-multiple-down", pgog_multiple_down)

  pgog_multiple_both = ggplot(mtcars) + geom_bloc(aes(x = c(mpg,cyl), y = c(gear), fill=(cyl), height = c(P(mpg | gear, cyl))),side = "both")
  vdiffr::expect_doppelganger("pgog-multiple-both", pgog_multiple_both)

})
