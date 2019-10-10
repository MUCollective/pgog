context("parse.R")
library(rlang)



# legit mapping
m1 <- aes(height = c(P(cyl|gear)), x = c(gear))

# TODO: write more cases

# bad mapping

m2 <- aes(height = c(P(cyl|gear), P(cyl)), x = c(gear))

# TODO: write more cases


test_that("parse_aes conditional pmf", {
  expect_named(parse_aes(m1)) # TODO: is there a better way to test this?
})


test_that("parse_aes conditional pmf", {
  expect_error(parse_aes(m2)) # TODO: is there a better way to test this?
})
