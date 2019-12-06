context("parse.R")
library(rlang)

# legit mapping

# TODO: write more cases

# bad mapping
m1 = aes(height = c(P(cyl|gear)), x = c(gear))
m2 = aes(height = c(P(cyl)), x =c(gear))
m3 = aes(height = c(P(cyl|gear, carb)), x = c(gear, carb))
# TODO: write more cases


test_that("parse_aes pmf 1", {
  expect_named(parse_aes(m1),c("marginals","conditionals","aes","level"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 2", {
  expect_named(parse_aes(m2),c("marginals","conditionals","aes","level"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 3", {
  expect_named(parse_aes(m3),c("marginals","conditionals","aes","level"),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 4", {
  expect_equal(flatten_aes(m1)$mapping[[1]],quote(gear),ignore.order = TRUE, ignore.case = TRUE)
})

test_that("parse_aes pmf 5", {
  expect_equal(flatten_aes(m1)$mapping[[2]],quote(P(cyl | gear)),ignore.order = TRUE, ignore.case = TRUE)
})


















